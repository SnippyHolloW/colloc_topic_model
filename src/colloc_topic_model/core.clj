(ns colloc-topic-model.core
  (:use [clojure.java.io]
        [clojure.string :only (join split replace-first)]))

(defn read-corpus []
  (with-open [rdr (reader "resources/nipscollocation.yld")]
    (let [lines (doall (line-seq rdr))]
      (reduce (partial merge-with #(conj %1 (first %2)))
              (map (fn [x] {(first x) [(second x)]})
                   (map (fn [x] [(Integer. (replace-first (first x) #"_" "")) 
                                 (vec (rest x))]) 
                        (map (fn [x] (split x #" ")) lines)))))))

(defn initialize-doc [n-topics document]
  (conj (pop (vec 
               (map (fn [x] (rand-int (+ n-topics 1))) (vec document)))) 
        (+ (rand-int n-topics) 1)))

(defn random-init-topic-boundaries [corpus n-topics]
  (into {} (map (fn [id-doc] [(first id-doc) 
                              (map (partial initialize-doc n-topics) 
                                   (second id-doc))]) corpus)))

(defn initialize-colloc-topics [n-topics alpha corpus z]
  ;(vec (repeat n-topics (map (fn [doc-w doc-z] (map (fn [sent-w sent-z] ))) corpus z))))
  ;TODO
  )

(defn parse-ut [words indicators buffer accs]
  (if (= words '())
    accs
    (let [ word (first words)
         ind  (first indicators)]
         (if (= ind 0)
           (parse-ut (rest words) (rest indicators) (conj buffer word) accs)
           (parse-ut (rest words) (rest indicators) [] 
               (let 
                 [acc (get accs ind-1)
                  colloc (conj buffer word)
                  new-count (+ (get acc colloc 0) 1)]
                 (assoc accs ind
                        (assoc acc word new-count))
                )
            )
         )
    )
  )
)




(defn -main [& args]
  (let [n-topics 10
        corpus (read-corpus)
        z (random-init-topic-boundaries corpus n-topics)
        phi-k (initialize-colloc-topics n-topics alpha corpus z)]
    (prn z))
  )
