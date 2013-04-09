; > *This is a recipe that I wrote for the [*Clojure Data
; Analysis
; Cookbook*](/pages/announcements/clj-data-analysis/index.html).
; However, it didn't make it into the final book, so I'm
; sharing it with you today.*
;
; One of the benefits of [linked
; data](http://linkeddata.org/) is that it *is* linked.
; Data in one place points to data in another place, and
; the two integrate easily. However, although the links
; are explicit, we still have to bring the data together
; manually. Let's see how to do that with Clojure.
;
; ### Getting ready
;
; We'll first need to list the dependencies that we'll
; need in our [Leiningen](http://leiningen.org/)
; [`project.clj`](project.clj) file.
;

; ```clojure
; :dependencies [[org.clojure/clojure "1.4.0"]
;                [incanter/incanter "1.4.1"]
;                [edu.ucdenver.ccp/kr-sesame-core "1.4.5"]
;                [org.clojure/tools.logging "0.2.4"]
;                [org.slf4j/slf4j-simple "1.7.2"]]
; ```

;
; And we'll need to include them in our script or REPL.

(require '(clojure.java [io :as io]))
(require '(clojure [xml :as xml] 
                   [pprint :as pp]
                   [zip :as zip]))
(use 'incanter.core
     'edu.ucdenver.ccp.kr.kb
     'edu.ucdenver.ccp.kr.rdf
     'edu.ucdenver.ccp.kr.sparql
     'edu.ucdenver.ccp.kr.sesame.kb
     'clojure.set)
(import [java.io File]
        [java.net URL URLEncoder])

; We’ll also use the
; [currencies.ttl](/clj-data-analysis/data/currencies.ttl)
; file.
;
; ### How to do it…
;
; For this, we'll load data from the `currencies.ttl` file
; and from [DBPedia](http://dbpedia.org/About) into the
; triple store. Because the triples in one references the
; triples in the other, the two datasets are automatically
; merged. Then we can query the triple store and get data
; from both of the original sources back out.
;
; To make this happen, first we need some functions to set
; up the plumbing for working with RDF. These will create
; and initialize the triple store that we’ll need to use.

(defn kb-memstore
  "This creates a Sesame triple store in memory."
  []
  (kb :sesame-mem))
(def tele-ont "http://telegraphis.net/ontology/")
(defn init-kb
  "This creates an in-memory knowledge base and
  initializes it with a default set of namespaces."
  [kb-store]
  (register-namespaces
    kb-store
    [["geographis" (str tele-ont 
                        "geography/geography#")]
     ["code" (str tele-ont "measurement/code#")]
     ["money" (str tele-ont "money/money#")]
     ["owl" "http://www.w3.org/2002/07/owl#"]
     ["rdf" (str "http://www.w3.org/"
                 "1999/02/22-rdf-syntax-ns#")]
     ["xsd" "http://www.w3.org/2001/XMLSchema#"]
     ["currency" (str "http://telegraphis.net/"
                      "data/currencies/")]
     ["dbpedia" "http://dbpedia.org/resource/"]
     ["dbpedia-ont" "http://dbpedia.org/ontology/"]
     ["dbpedia-prop" "http://dbpedia.org/property/"]
     ["err" "http://ericrochester.com/"]]))

; And we'll use the following utilities later on.

(defn rekey
  "This just flips the arguments for 
  clojure.set/rename-keys to make it more
  convenient."
  ([k-map map]
   (rename-keys 
     (select-keys map (keys k-map)) k-map)))

(defn binding-str
  "This takes a binding, pulls out the first tag's 
  content, and concatenates it into a string."
  ([b]
   (apply str (:content (first (:content b))))))

(defn result-seq
  "This takes the first result and returns a sequence 
  of this node, plus all the nodes to the right of it."
  ([first-result]
   (cons (zip/node first-result)
         (zip/rights first-result))))

; These build the
; [SPARQL](http://www.w3.org/TR/sparql11-overview/) query
; and create a URL out of them for querying
; [DBPedia](http://dbpedia.org/About). The last,
; `query-sparql-results` gets the results, parses them,
; and navigates the XML tree to get to the results.

(defn make-query
  "This creates a query that returns all the 
  triples related to a subject URI. It does 
  filter out non-English strings."
  ([subject kb]
   (binding [*kb* kb
             *select-limit* 200]
     (sparql-select-query
       (list (list subject '?/p '?/o)
             '(:or (:not (:isLiteral ?/o))
                   (!= (:datatype ?/o) rdf/langString)
                   (= (:lang ?/o) ["en"])))))))

(defn make-query-uri
  "This constructs a URI for the query."
  ([base-uri query]
   (URL. (str base-uri
              "?format=" 
              (URLEncoder/encode "text/xml")
              "&query=" (URLEncoder/encode query)))))

(defn query-sparql-results
  "This queries a SPARQL endpoint and returns a 
  sequence of result nodes."
  ([sparql-uri subject kb]
   (->>
     kb
     ;; Build the URI query string.
     (make-query subject)
     (make-query-uri sparql-uri)
     ;; Get the results, parse the XML, and
     ;; return the zipper.
     io/input-stream
     xml/parse
     zip/xml-zip
     ;; Find the first child.
     zip/down
     zip/right
     zip/down
     ;; Convert all children into a sequence.
     result-seq)))

; We’ll download the data we need from
; [DBPedia](http://dbpedia.org/About) and insert it into
; the triple store alongside the RDF file’s data.
;
; As part of this, we will split all URI strings into
; prefixes and resources. If each prefix has a namespace
; abbreviation defined for it in `init-kb` above, the
; abbreviation needs to be used, and that and the resource
; are converted into a symbol together. Otherwise, the URI
; as a whole is converted into a symbol. What does this
; look like?

(defn split-symbol
  "This splits a string on an index and returns a symbol
  created by using the first part as the namespace and the
  second as the symbol."
  ([kb string index]
     (if-let [ns-prefix (get (:ns-map-to-short kb)
                             (.substring string 0 index))]
       (symbol ns-prefix (.substring string index))
       (symbol string))))

(defn str-to-ns
  "This maps a URI string to a ns and symbol, given the
  namespaces registered in the KB."
  ([uri-string] (str-to-ns *kb* uri-string))
  ([kb uri-string]
   (let [index-gens
         (list #(.lastIndexOf uri-string (int \#))
               #(.lastIndexOf uri-string (int \/)))]
     (if-let [index
              (first 
                (filter #(> % -1) 
                        (map (fn [f] (f)) index-gens)))]
       (split-symbol kb uri-string (inc index))
       (symbol uri-string)))))

; Next, we’ll need to convert a variety of data types as
; encoded in the result XML into native Clojure types, the
; way the triple store interface wants to work with them.
; For that we’ll use a multimethod. 

(def xmls "http://www.w3.org/2001/XMLSchema#")

(defmulti from-xml
  (fn [r] [(:tag r) (:datatype (:attrs r))]))

(defmethod from-xml [:uri nil] [r]
  (str-to-ns (apply str (:content r))))
(defmethod from-xml [:literal nil] [r]
  (apply str (:content r)))
(defmethod from-xml [:literal (str xmls 'int)] [r]
  (read-string (apply str (:content r))))
(defmethod from-xml :default [r]
  (apply str (:content r)))

; Now we need a function to convert each result node into
; a vector triple. This will be used by a later function
; that loads the data from DBPedia into the triple store.

(defn result-to-triple
  "This converts a result node into a triple vector."
  ([iri r]
   (let [{:keys [tag attrs content]} r
         [p o] content]
     [iri
      (str-to-ns (binding-str p))
      (from-xml (first (:content o)))])))

(defn load-dbpedia
  "This loads data from dbpedia for a specific IRI into a
  KB."
  ([kb sparql-uri iri]
   (binding [*kb* kb]
     (->>
       kb
       (query-sparql-results sparql-uri iri)
       (map #(result-to-triple iri %))
       (add-statements kb)))))

; We’ll define a function to pull the objects of all
; same-as statements out of an RDF query and load all
; statements for that URI from DBPedia.

(defn load-same-as
  "This takes the results of a query for owl:sameAs and
  loads the object URIs into the triple store from
  DBPedia."
  ([kb [_ _ same-as]]
   (load-dbpedia kb "http://dbpedia.org/sparql" same-as)
   kb))

; Finally, `aggregate-dataset` drives the whole thing. It
; takes the triple store, the datafile, a query to execute
; on the final results, and a mapping between SPARQL query
; parameters and keywords for the final result.

(defn aggregate-dataset
  [t-store data-file q col-map]
  (binding [*kb* t-store]
    ;;; Load primary data.
    (load-rdf-file t-store (File. data-file))
    ;;; Load associated data.
    (reduce load-same-as
            t-store
            (query-rdf t-store nil 'owl/sameAs nil))
    ;;; Query 
    (to-dataset (map (partial rekey col-map)
                     (query t-store q)))))

; Now let’s use all this to create the dataset. We’ll bind
; the parameters to names so we can refer to them more
; easily and then use them to call `aggregate-dataset`.

(def data-file
  "../../../clj-data-analysis/data/currencies.ttl")

(def col-map {'?/name :fullname
              '?/iso :iso
              '?/shortName :name
              '?/symbol :symbol
              '?/country :country
              '?/minorName :minor-name
              '?/minorExponent :minor-exp
              '?/peggedWith :pegged-with
              '?/usedBanknotes :used-banknotes
              '?/usedCoins :used-coins})

(def q '[[?/c rdf/type money/Currency]
         [?/c owl/sameAs ?/d]
         [?/c money/name ?/name]
         [?/c money/shortName ?/shortName]
         [?/c money/isoAlpha ?/iso]
         [?/c money/minorName ?/minorName]
         [?/c money/minorExponent ?/minorExponent]
         [:optional
          [[?/d dbpedia-prop/symbol ?/symbol]]]
         [:optional
          [[?/d dbpedia-ont/usingCountry ?/country]]]
         [:optional
          [[?/d dbpedia-prop/peggedWith ?/peggedWith]]]
         [:optional
          [[?/d dbpedia-prop/usedBanknotes
            ?/usedBanknotes]]]
         [:optional
          [[?/d dbpedia-prop/usedCoins ?/usedCoins]]]])

; Let’s put it all together.
;
; ```clojure
; user=> (aggregate-dataset (init-kb (kb-memstore))
;             data-file q col-map)
; [:used-coins :symbol :pegged-with :country :name :minor-exp :iso :minor-name :used-banknotes :fullname]
; [2550 "د.إ" "U.S. dollar = 3.6725 dirhams" dbpedia/United_Arab_Emirates "dirham" "2" "AED" "fils" 9223372036854775807 "United Arab Emirates dirham"]
; [1 "Af  or Afs" nil dbpedia/United_States_dollar "afghani" "2" "AFN" "pul" 1 "Afghan afghani"]
; [nil dbpedia/Albanian_lek nil dbpedia/Albania "lek" "2" "ALL" "qindarkë" nil "Albanian lek"]
; [102050100200500 nil nil dbpedia/Armenia "dram" "0" "AMD" "luma" 9223372036854775807 "Armenian dram"]
; …
; ```
;

; ### How it works…
;
; Linked data is, well, linked. Basically, we took all the
; data we’re interested in and dumped it into one big
; database. We used the links in the data itself to drive
; this by following *same-as* relationships already
; encoded in the data. We just used our query to pull it
; all together. 
;
; As an aside, notice the multimethod `from-xml` that
; dispatches on the result node’s tag name and its
; datatype attribute. Currently, this handles strings,
; integers, and URIs. They are sufficient for this
; dataset. If we need more, though, we can add them
; easily.
;
; Also, in the query, all the phrases that are pulled in
; from DBPedia are marked `:optional`. We don’t want the
; overall query to fail because any of them are missing,
; and we can’t mark them all optional as a group because
; we don’t want the optional phrases as a whole to fail if
; any one is missing.
;
; -----
;
; > *This post is a literate programming file. Click on
; the [raw](index.clj) link below---and the
; [project.clj](project.clj) file linked to above---to
; download a version of this post that you can load
; directly into a Clojure REPL.*

; <!-- vim: set textwidth=58: -->
