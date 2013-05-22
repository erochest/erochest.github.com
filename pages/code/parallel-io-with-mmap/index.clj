; > *This is a recipe that I wrote for the [*Clojure Data
; Analysis
; Cookbook*](/pages/announcements/clj-data-analysis/index.html).
; However, it didn't make it into the final book, so I'm
; sharing it with you today.*
;
; Parallelizing the pure parts of processes, the parts
; that don’t involve side effects like reading input
; files, is relatively easy. But once disk IO enters the
; picture, things become more complicated. There’s a
; reason for that: when reading from a disk, all threads
; are inherently contending for one resource, the disk.
;
; There are ways to mitigate this, but ultimately it comes
; down to working with the disk and the processing
; requirements to get the best performance we can from a
; single, sequential process. So to be completely honest,
; the title of this post is a little misleading. We can’t
; parallelize IO on a single, shared resource. But there
; are several things to keep in mind to make reading from
; a disk faster.
;
; As a test case, we’ll look at parsing a sample of the
; comments from the [Stack Exchange data
; dumps](http://blog.stackexchange.com/category/cc-wiki-dump/).
; For each user, we’ll find the date range when that user
; posted comments to Stack Overflow. The data’s in XML,
; and if we were sane, we’d just use a lazy XML parser.
; But for the sake of demonstration, we’ll use regular
; expressions to pull out the attributes and their values
; for each comment.
;
; For this problem, we’ll look at two ways of handling the
; file IO. First, we’ll consider using straightforward
; lazy, sequential access, and then we’ll look at memory
; mapping the file. We’ll look at the performance of each
; and consider what situations each might be useful in.
;
; ### Getting ready…
;
; For this, we’ll need a number of dependencies, so we’ll
; list these in our [Leiningen](http://leiningen.org/)
; [`project.clj`](project.clj) file.
;

; ```clojure
; :dependencies [[org.clojure/clojure "1.5.1"]
;                [nio "0.0.5"]
;                [org.apache.commons/commons-lang3 "3.1"]
;                [clj-time "0.5.0"]]
; ```

;
; Then, we’ll also have a number of imports and includes.

(import '[java.io File RandomAccessFile]
        '[java.nio.charset Charset]
        '[org.apache.commons.lang3 StringEscapeUtils])
(use '[clj-time.core :exclude (extend)]
     '[clj-time format coerce])
(require '[nio.core :as nio]
         '[clojure.java.io :as io]
         '[clojure.string :as string]
         '[clojure.core.reducers :as r])

; And for the data file, we can just use a sample of the
; Stack Exchange comments. You can find a (non-random)
; sample of 100,000 lines from that file in the source
; code for this chapter or download it from
; [here](/clj-data-analysis/data/comments.xml).
;
; ### How to do it…
;
; First, let’s tackle the problem domain. We’ll define a
; record that holds a user’s identifier and the range of
; comments seen so far. And we’ll write a function that
; will combine two `UserPost` records into one with the
; maximum range from both (assuming they represent the
; same user). Also, we’ll write a combiner function for a
; map of UserPost records.

(defrecord UserPost
  [user-id earliest-date latest-date])

(defn combine-user-posts
  [up1 up2]
  (UserPost.
    (:user-id up1)
    (:earliest-date
      (from-long (min (to-long (:earliest-date up1))
                      (to-long (:earliest-date up2)))))
    (:latest-date
      (from-long (max (to-long (:latest-date up1))
                      (to-long (:latest-date up2)))))))

(def combiner
  (r/monoid (partial merge-with combine-user-posts)
            hash-map))

; Next, we’ll add some general data parsing functions. The
; first is simply a lazy version of
; `clojure.string/split-lines`. The rest are more
; specifically targeted to the task at hand. They identify
; lines from the input that contain data, parse those into
; map, and eventually parse them into a UserPost.

(defn split-lines
  ([input] (split-lines input 0))
  ([input start]
   (let [end (.indexOf input 10 start)]
     (when-not (= end -1)
       (lazy-seq
         (cons (String.
                 (.trim (.substring input start end)))
               (split-lines input (inc end))))))))

(defn data-line?
  [line]
  (.startsWith (string/trim line) "<row "))

(defn parse-pair
  [[k v]]
  [(keyword k) (StringEscapeUtils/unescapeXml v)])

(defn parse-line
  [line]
  (->> line
    (re-seq #"(\w+)=\"([^\"]*)\"")
    (map next)
    (map parse-pair)
    flatten
    (apply hash-map)))

(def ^:dynamic *date-formatter*
  (formatters :date-hour-minute-second-ms))

(defn line->user-post
  [line]
  (let [user-id (if-let [uid (:UserId line)]
                  (read-string uid)
                  nil)
        cdate (parse *date-formatter*
                     (:CreationDate line))]
    (UserPost. user-id cdate cdate)))

; The next two functions process a sequence of lines into
; a map, which associates the users’ identifiers with the
; `UserPost` objects for each user.

(defn process-user-map
  ([] {})
  ([user-post-map line]
   (let [user-post (line->user-post line)
         user-id (:user-id user-post)]
     (assoc user-post-map
            user-id
            (if-let [current (get user-post-map user-id)]
              (combine-user-posts current user-post)
              user-post)))))

(defn process-lines
  [lines]
  (->>
    lines
    (r/map parse-line)
    (r/filter data-line?)
    (r/fold combiner process-user-map)))

; #### Reading a File Serially
;
; With that basis, the function to read the file serially
; is quite simple.

(defn serial-process
  [file-name]
  (with-open [reader (io/reader file-name)]
    (process-lines (line-seq reader))))

; #### Reading from a Memory-Mapped File
;
; The other option is to read from a memory-mapped file.
; This allows us to treat a file’s contents as a byte
; array. For certain kinds of access, this can be very
; fast, but it takes a lot more set-up to get going.
;
; First, we’ll have a couple of parameters: the character
; set for the input and a hint for the size of chunk to
; break the file into.

(def ^:dynamic *charset* (Charset/forName "UTF-8"))
(def ^:dynamic *chunk-size* (* 10 1024 1024))

; With those, we’ll break the file into chunks by skipping
; through it and reading ahead until we get to the end of
; a line. Later, when we actually read the file, this will
; make sure that lines aren’t broken across chunks.

(defn get-chunk-offsets
  [f pos offsets chunk-size]
  (let [skip-to (+ pos chunk-size)]
    (if (>= skip-to (.length f))
      (conj offsets (.length f))
      (do
        (.seek f skip-to)
        (while (not= (.read f) (int \newline)))
        (let [new-pos (.getFilePointer f)]
          (recur f new-pos (conj offsets new-pos)
                 chunk-size))))))

(defn get-chunks
  ([file-name] (get-chunks file-name *chunk-size*))
  ([file-name chunk-size]
   (with-open [f (RandomAccessFile. file-name "r")]
     (doall
       (partition 2 1 (get-chunk-offsets
                        f 0 [0] chunk-size))))))

; And with those, we can memory map each chunk and read
; the lines out of it as a sequence.

(defn read-chunk
  [channel [from to]]
  (let [chunk-mmap
        (.map
          channel
          java.nio.channels.FileChannel$MapMode/READ_ONLY
          from
          (- to from))
        decoder (.newDecoder *charset*)]
    (doall 
      (split-lines
        (str (.decode decoder chunk-mmap))))))

; These let us bring everything together similar to how we
; did with serial-process.

(defn mmap-process
  [file-name]
  (let [chan (nio/channel file-name)]
    (->>
      file-name
      get-chunks
      (r/mapcat (partial read-chunk chan))
      (r/map parse-line)
      (r/filter data-line?)
      (r/fold combiner process-user-map))))

; #### Performance
;
; Now we compare two methods’ performance. (Measured with
; the [criterium](https://github.com/hugoduncan/criterium)
; library.)
; 
;  **Function**       **Mean Time**
;  ------------------ -------------
;  `serial-process`   14.15 sec  
;  `mmap-process`     15.35 sec  
;

; ### How it works…
;
; So for this, the serial process is better. It’s become a
; cliché to say that disk is the new tape, but there’s a
; lot of truth in that. If we can arrange processing so
; that we access the disk sequentially, as we do whenever
; we read one line at a time, we'll get about the best
; performance we can.
;
; If we must have random access, for a disk-based index
; for example, then we’re better off memory mapping the
; part of the file that we’re going to access.
;
; The numbers above support these guidelines. As usual,
; the devil is in the details and exactly how our
; processing is going to happen.
;
; -----
;
; > *This post is a literate programming file. Click on
; the [raw](index.clj) link below---and the
; [project.clj](project.clj) file linked to above---to
; download a version of this post that you can load
; directly into a Clojure REPL.*

; <!-- vim: set textwidth=58: -->
