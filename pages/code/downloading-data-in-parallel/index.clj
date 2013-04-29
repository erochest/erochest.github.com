---
title: Downloading Data in Parallel
date: 2013-04-29T13:04:02Z
tags: clojure, data analysis, Clojure Data Analysis Cookbook, code
---

; > *This is a recipe that I wrote for the [*Clojure Data
; Analysis
; Cookbook*](/pages/announcements/clj-data-analysis/index.html).
; However, it didn't make it into the final book, so I'm
; sharing it with you today.*
;
; Sometimes when getting resources, we have to download
; them from many URLs. Doing that sequentially for one or
; two sources is fine, but if there are too many, we will
; really want to make better use of our Internet
; connection by downloading several at once.
;
; This recipe does that. It chunks a sequence of URLs and
; downloads a block in parallel. It uses the
; [`http.async.client`](http://neotyk.github.com/http.async.client/)
; library to perform the download asynchronously, and
; we’ll simply manage how we trigger those jobs.
;
; ### Getting ready…
;
; First, we need to make sure that our
; [Leiningen](http://leiningen.org/)
; [`project.clj`](project.clj) file lists the dependencies
; we’ll need:
;

; ```clojure
; :dependencies [[org.clojure/clojure "1.4.0"]
;                [http.async.client "0.4.5"]]
; ```

;
; And we need to use those in our script or REPL.

(require '[http.async.client :as http])
(import [java.net URL])

; For this example, we’ll download all of the ZIP files
; related to the [World Health Organization’s mortality
; data](http://www.who.int/whosis/mort/download/en/index.html).
; Let’s bind those to the name `urls`.

(def urls 
  (let [who-ftp 
        (str "http://www.who.int/whosis/database/"
             "mort/download/ftp/")]
    [(str who-ftp "documentation.zip")
     (str who-ftp "availability.zip")
     (str who-ftp "country_codes.zip")
     (str who-ftp "notes.zip")
     (str who-ftp "Pop.zip")
     (str who-ftp "morticd07.zip")
     (str who-ftp "morticd08.zip")
     (str who-ftp "morticd09.zip")
     (str who-ftp "morticd10.zip")]))

; ### How to do it…
;
; First, let’s set our default block size. We’ll do this
; using a dynamic variable so we can easily change it with
; the `binding` form.

(def ^:dynamic *block-size* 3)

; Now, we want to be able to see what we’re doing, so
; let’s wrap the `http.async.client/GET` function in a
; function that prints the URL when we start and returns
; the URL and the download.

(defn get-verbose
  "This uses http.async.client to download a URL."
  [client url]
  (println "GET" url)
  [url (http/GET client url)])

; Next, let’s take the output of `get-verbose` and force
; the response. Since we don’t care about the response
; itself, we’ll throw most of it away and just return the
; status information. Because we’re curious, this will
; also print out information as it’s working.

(defn get-response
  "This forces the response to download and prints out
  what's happening."
  [[url resp]]
  (println "awaiting" url)
  (http/await resp)
  (println "done" url)
  (http/status resp))

; To see how this will work, let’s write a function to
; download all of the URLs sequentially. This will also
; serve as a baseline to see how much of a speed-up we
; will get.

(defn sequential
  "This downloads the resources sequentially."
  []
  (with-open [client (http/create-client
                       :follow-redirects true)]
    (doall
      (map get-response
           (map (partial get-verbose client)
                urls)))))

; Now to process the blocks, we’ll partition the URLs
; using `partition-all` and use a new function,
; `get-block`, to force all the downloads in each block to
; complete before we move on to the next block.

(defn get-block
  "This forces a block of responses to download."
  [block]
  (doall (map get-response block)))

(defn async
  "This downloads the resources asynchronously."
  []
  (with-open [client (http/create-client
                       :follow-redirects true)]
    (doall
      (mapcat get-block
              (partition-all
                *block-size*
                (map
                  (partial get-verbose client)
                  urls))))))

; Now we run this by simply calling async.
;

; ```clojure
; user=> (async)
; GET http://www.who.int/whosis/database/mort/download/ftp/documentation.zip
; …
; ```

;
; ### How it works…
;
; First, we partition the URLs into blocks that will be
; downloaded in parallel. Because this process is
; IO-bound, we don’t have to worry about matching the
; number of CPUs on our machines. The function `get-block`
; then takes each block and forces `get-response` to
; complete the download and return.
;
; Playing around with the block size shows some impressive
; speed-ups. Using a block size of five takes almost half
; the time as the serial version. Experiment with a small
; subset of your downloads to see what the optimal block
; size is for your network and the resources you’re
; interested in.
;
; -----
;
; > *This post is a literate programming file. Click on
; the [raw](index.clj) link below---and the
; [project.clj](project.clj) file linked to above---to
; download a version of this post that you can load
; directly into a Clojure REPL.*

; <!-- vim: set textwidth=58: -->
