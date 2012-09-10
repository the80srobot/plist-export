(ns plist-export.core
  (:require [clojure.string :as string]
            [clojure.zip :as zip])
  (:import [org.apache.commons.codec.binary Base64]))

(def plist-header ^:private
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
    "<plist version=\"1.0\">"])

(def plist-footer ^:private
  ["</plist>"])

(def iso8601-formatter ^:private
  (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ssZ"))

(defn- coll-children
  [coll]
  (if (map? coll)
    (apply concat
      ;; If the collection is a map, we flatten out the KV pairs and tag keys with metadata.
      ;; Note that in order to have metadata, the key (usually a string) is wrapped in a lambda that returns it.
      (map (fn [e] [(with-meta #(first e) {:plist-func-hint 'plist-key}) (second e)]) (seq coll)))
    (seq coll)))

(defn- coll-zip
  [coll]
  (zip/zipper coll? coll-children nil coll))

(defn- zip-depth
  [z]
  (count (zip/path z)))

(defn- zip-walk
  "Returns a lazy sequence of a depth-first walk through the zipper, including
  nodes that are encountered on the way up from leaves. Each element in the
  sequence is a vector of (in order) the encountered node and a tag marking it
  as :leaf, :open, or :close."
  ([z]
    (lazy-seq
      (when-not (zip/end? z)
        (let [tag (if (and (zip/branch? z) (zip/down z)) :open :leaf) ; can/does the node have children?
              next (zip/next z)
              depth-diff (- (zip-depth z) (zip-depth next))] ; are we going up or down?
          (if (pos? depth-diff) ; If the next node is closer to the root...
            (let [unwound-nodes (take depth-diff (reverse (zip/path z))) ; ...nodes we hit on our way up...
                  tagged-unwound (map #(vector % :close) unwound-nodes)] ; ...tagged with :close.
              (cons [(zip/node z) tag] (concat tagged-unwound (zip-walk next))))
            (cons [(zip/node z) tag] (zip-walk next))))))))

(defn ls-test
  [n]
  (lazy-seq
    (let [m (inc n)]
      (concat [n m] (ls-test (inc m))))))

(defmulti ^:private plist-node
  (fn [node tag]
      (type node)))

(defmethod plist-node clojure.lang.PersistentVector
  [node tag]
  (case tag
    :open "<array>"
    :leaf "<array/>"
    :close "</array>"))

(defmethod plist-node clojure.lang.APersistentMap
  [node tag]
  (case tag
    :open "<dict>"
    :leaf "<dict/>"
    :close "</dict>"))

(defmethod plist-node clojure.lang.PersistentList
  [node tag]
  (case tag
    :open "<array>"
    :leaf "<array/>"
    :close "</array>"))

(defmethod plist-node clojure.lang.PersistentList$EmptyList
  [node tag]
  "<array/>")

(defmethod plist-node clojure.lang.Cons
  [node tag]
  (case tag
    :open "<array>"
    :leaf "<array/>"
    :close "</array>"))

(defmethod plist-node clojure.lang.LazySeq
  [node tag]
  (case tag
    :open "<array>"
    :leaf "<array/>"
    :close "</array>"))

(defmethod plist-node java.lang.Double
  [node tag]
  (format "<real>%f</real>" node))

(defmethod plist-node java.lang.Long
  [node tag]
  (format "<integer>%d</integer>" node))

(defmethod plist-node java.lang.Character
  [node tag]
  (format "<string>%c</string>" node))

(defmethod plist-node java.lang.String
  [node tag]
  (format "<string>%s</string>" (string/replace node #"<|>" #(case % "<" "&lt;" ">" "&gt;"))))

(defmethod plist-node clojure.lang.Named
  [node tag]
  (format "<string>%s</string>" (name node)))

(defmethod plist-node java.lang.Boolean
  [node tag]
  (if node "<true/>" "<false/>"))

(defmethod plist-node java.util.Date
  [node tag]
  (let [date (. iso8601-formatter (format node))]
    (format "<date>%s</date>" date)))

(defmethod plist-node (Class/forName "[B") ; byte array
  [node tag]
  (let [encoded (. (Base64.) (encode node))
        base64-string (apply str (map char encoded))]
    (format "<data>%s</data>" base64-string)))

(defmethod plist-node :default
  [node tag]
  (throw (Exception. (format "Type %s can't be stored in plist." (type node)))))

(defn- plist-key
  [node-func tag]
  (format "<key>%s</key>" (str (node-func))))

(defn- plist-line
  "Returns a plist line that represents the node, which can be either the first or the second appearance
  of the nested collection member, depending on whether the zipper is walking up or down or hitting a leaf node."
  [node]
  (let [value (first node)
        tag (second node)
        ;; If there's a function reference hinted by the value then honor it, otherwise
        ;; use plist-node.
        formatter (if-let [hint (and (meta value) ((meta value) :plist-func-hint))]
                    (resolve hint)
                    plist-node)]
    (formatter value tag)))

(defn- plist-body
  "Returns a lazy seq of lines for the plist, minus header and footer."
  [coll]
  (let [z (coll-zip coll)
        w (zip-walk z)]
    (map plist-line w)))

(defn lazy-plist
  "Returns a lazy sequence of lines that make up a Property List (XML) representation of the parameter.
  Supports any arbitrarily nested combination of vectors, byte-arrays, Java dates, strings, numbers, symbols/keyword, maps, lists and booleans."
  [coll]
  (lazy-cat plist-header (plist-body coll) plist-footer))
