(ns xkcd903.core
  (:require [clojure.xml :as xml]
            [clojure-http.resourcefully :as res]
            [clojure.contrib.str-utils2 :as str2])
  (:import (org.htmlcleaner HtmlCleaner)))

(def *random-page* (str "http://en.wikipedia.org/wiki/Special:Random"))

(defn full-url
  "Use a CDN. We don't want to kill Wikipedia."
  [name]
  (str "http://en.wikipedia.org.nyud.net" name))

(defn log
  [msg & rest]
  (println (.format (java.text.DateFormat/getDateTimeInstance)
                    (java.util.Date.))
           "-" (str2/join " " (cons msg rest))))

(def philosophy? (partial = "/wiki/Philosophy"))

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
        rparen-index (.indexOf paragraph ")")]
    (cond
      (= -1 link-index) nil
      (or (= -1 rparen-index)
          (< link-index lparen-index))
      (second (re-find #"<a href=\"(/wiki/.*?)\"" paragraph))
      true (recur (apply str (drop (inc rparen-index) paragraph))))))

(defn analyse-page
  [addr]
  (let [body-str (get-body addr)
        [title paragraphs] (parse-page body-str)]
    (loop [head (first paragraphs) tail (rest paragraphs)]
      (if-let [url (valid-link head)]
        (do
          (log url)
          (if (philosophy? url)
            (log "We've lost again")
            (analyse-page (full-url url)))))
      (recur (first tail) (rest tail)))))
