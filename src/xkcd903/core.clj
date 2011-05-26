(ns xkcd903.core
  (:require [clojure-http.resourcefully :as res])
  (:import (org.htmlcleaner HtmlCleaner)))

(def *random-page* "http://en.wikipedia.org/wiki/Special:Random")

(def *cdn-domain* "en.wikipedia.org.nyud.net")

(defn full-url
  "Use a CDN. We don't want to kill Wikipedia."
  [name]
  (str "http://" *cdn-domain* name))

(defn log
  [msg]
  (let [date-str (.format (java.text.DateFormat/getDateTimeInstance)
                          (java.util.Date.))]
    (println date-str "-" msg)))

(defn xpath
  [node query]
  (.evaluateXPath node query))

(defn to-html
  [node]
  (let [writer (java.io.StringWriter.)]
    (.serialize
      node
      (org.htmlcleaner.SimpleXmlSerializer.
        (.getProperties (org.htmlcleaner.HtmlCleaner.)))
      writer)
    (str writer)))

(defn parse-page
  [html]
  (let [cleaner (new HtmlCleaner)]
    (doto (.getProperties cleaner)
      (.setOmitComments true)
      (.setPruneTags "script,style"))
    (let [node (.clean cleaner html)
          content (xpath node "//div[@id='bodyContent']//p")]
      [(str (.getText (.findElementByName node "title" true)))
       (map to-html content)])))

(defn get-body
  [addr]
  (apply str (:body-seq (res/get addr))))

(defn valid-link
  [paragraph]
  (let [link-index  (.indexOf paragraph "<a")
        lparen-index (.indexOf paragraph "(")
        rparen-index (.indexOf paragraph ")")
        link-is-not-in-parens (or (= -1 rparen-index)
                                  (< link-index lparen-index))
        link (second (re-find #"<a href=\"(/wiki/.*?)\"" paragraph))
        link-is-valid (and link-is-not-in-parens
                           (not (or (re-find #"^/wiki/Wikipedia:" link)
                                    (re-find #"^/wiki/File:" link))))]
    (cond
      (= -1 link-index) nil
      link-is-valid link
      true (recur (apply str (drop (inc rparen-index) paragraph))))))

(def leading-to-philosophy (atom #{(full-url "/wiki/Philosophy")}))

(defn leads-to-philosophy?
  [addr]
  (contains? @leading-to-philosophy addr))

(defn mark-as-leading-to-philosophy
  [addr]
  (when (not (= addr (full-url *random-page*)))
    (swap! leading-to-philosophy #(conj % addr))))

(defn analyse-page
  ([addr history]
   (if (contains? history addr)
     (do
       (log "Great success! We've found a loop:")
       (map #(log (str "  " %)) history)
       true)
     (let [body-str (get-body addr)
           [title paragraphs] (parse-page body-str)]
       (loop [head (first paragraphs) tail (rest paragraphs)]
         (if-let [url (full-url (valid-link head))]
           (do
             (log url)
             (if (leads-to-philosophy? url)
               (do
                 (log "We've lost again")
                 (when (not (empty? history))
                   (map mark-as-leading-to-philosophy history))
                 false)
               (analyse-page url (conj history addr))))
           (recur (first tail) (rest tail)))))))
  ([addr] (analyse-page addr #{})))

(defn start!
  []
  (analyse-page *random-page*))
