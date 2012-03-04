;Copyright (c) 2012 Praveen Kumar Telugu
;Licensed under the Apache License, Version 2.0 (the "License");
;you may not use this file except in compliance with the License.
;You may obtain a copy of the License at
;
;    http://www.apache.org/licenses/LICENSE-2.0
;
;Unless required by applicable law or agreed to in writing, software
;distributed under the License is distributed on an "AS IS" BASIS,
;WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;See the License for the specific language governing permissions and
;limitations under the License.

(ns tpk.hs
  (:require [net.cgrand.enlive-html :as enlive]))

(defn fetch-url [url]
  (enlive/html-resource (java.net.URL. url)))

(defn url-join [base rel]
  (.toString (java.net.URL. (java.net.URL. base) rel)))

(defn parse-permalinks [base tree]
  (map #(url-join base (get-in % [:attrs :href]))
       (enlive/select tree [:td.title :a])))

(defn parse-titles [tree]
  (map enlive/text
       (enlive/select tree [:td.title :a])))

(defn parse-domains [tree]
  (map (fn [x] (let [t (enlive/text x) v (.trim t)]
           (->> v .length dec (subs v 1))))
       (enlive/select tree [:span.comhead])))

(defn parse-scores [tree]
  (map (fn [x] (let [t (enlive/text x)]
           (Integer/parseInt (subs t 0 (.indexOf t " ")))))
       (enlive/select tree [:td.subtext enlive/first-child])))

(defn parse-num-comments [tree]
  (map (fn [x] (let [t (enlive/text x)]
           (if (= t "discuss") 0
               (Integer/parseInt (subs t 0 (.indexOf t " "))))))
       (enlive/select tree [:td.subtext (enlive/nth-child 3)])))

(defn parse-sub-times [tree]
  (map (fn [x] (let [u (enlive/text x)
                     v (re-find #" (\d+) (minutes?|hours?|days?) ago  \|" u)]
                 (* (Integer/parseInt (v 1))
                    (cond
                     (.startsWith (v 2) "minute") 60
                     (.startsWith (v 2) "hour") 3600
                     (.startsWith (v 2) "day") 86400))))
       (enlive/select tree [:td.subtext])))

(defn parse-users [tree]
  (map enlive/text
       (enlive/select tree [:td.subtext (enlive/nth-child 2)])))

(defn parse-discussions [base tree]
  (map #(url-join base (get-in % [:attrs :href]))
       (enlive/select tree [:td.subtext [(enlive/nth-child 3) (enlive/attr? :href)]])))

(defn parse-ids [tree]
  (map (fn [x] (Integer/parseInt ((re-find #"^item\?id=(\d+)$" (get-in x [:attrs :href])) 1)))
       (enlive/select tree [:td.subtext [(enlive/nth-child 3) (enlive/attr? :href)]])))

(defstruct story :title :permalink :domain :num-comments :score :sub-time :user :id :discussion)

(defn fetch-stories [base]
  (def tree (fetch-url base))
  (def permalinks (parse-permalinks base tree))
  (def next-page (last permalinks))
  (def args (map list
                 (parse-titles tree) permalinks (parse-domains tree)
                 (parse-num-comments tree) (parse-scores tree)
                 (parse-sub-times tree) (parse-users tree)
                 (parse-ids tree) (parse-discussions base tree)))
  (lazy-cat (map #(apply struct story %) args) (fetch-stories next-page)))

(defn -main[]
  (println (take 10 (fetch-stories "http://hackerstreet.in/"))))