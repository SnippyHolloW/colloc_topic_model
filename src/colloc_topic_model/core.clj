(ns colloc-topic-model.core
  (:use [clojure.java.io]
        [clojure.string :only (join split replace-first)]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn read-corpus []
  (with-open [rdr (reader "resources/nipscollocation.yld")]
    (let [lines (doall (line-seq rdr))]
      (reduce (partial merge-with #(conj %1 (first %2)))
              (map (fn [x] {(first x) [(second x)]})
                   (map (fn [x] [(Integer. (replace-first (first x) #"_" "")) 
                                 (vec (rest x))]) 
                        (map (fn [x] (split x #" ")) lines)))))))

(defn random-init-topic-boundaries [c]
  ())

(defn -main [& args]
  (let [corpus (read-corpus)]
        ;z (random-init-topic-boundaries corpus)]
    (prn corpus))
  )
