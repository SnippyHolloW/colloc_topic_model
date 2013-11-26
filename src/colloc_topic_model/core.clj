(ns colloc-topic-model.core
  (:use [clojure.java.io]
        [clojure.string :only (join split replace-first)]))

(defn read-corpus []
  (with-open [rdr (reader "resources/nipscollocation_1000.yld")]
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


(defn parse-ut [buffer accs words_indicators]
  (let [words (first words_indicators)
        indicators (second words_indicators)]
    (if (or (= words nil) (= words '()))
      accs
      (let [word (first words)
            ind  (first indicators)
            remainder [(rest words) (rest indicators)]]
        (if (= ind 0)
          (parse-ut (conj buffer word) accs remainder)
          (parse-ut [] 
                    (let 
                        [topic-ind (- ind 1)
                         acc (get accs topic-ind)
                         colloc (conj buffer word)
                         new-count (+ (get acc colloc 0) 1)]
                      (assoc accs topic-ind
                             (assoc acc colloc new-count)))
                    remainder))))))


(defn merge-hm-vecs [l1 l2]
  (if (= l1 '())
      '()
      (let [l1r (rest l1)
            l2r (rest l2)
            hm1 (first l1)
            hm2 (first l2)]
           (cons (merge-with #(+ %1 %2) hm1 hm2)
                 (merge-hm-vecs l1r l2r)))))



(defn initialize-colloc-topics [n-topics corpus z]
  (reduce merge-hm-vecs 
          ;; TODO --- initialize accs properly...
          (map (partial parse-ut [] [{} {} {} {}])
               (reduce (fn[x y] (conj (second x) (second y)))
                       (merge-with #(zipmap %1 %2) corpus z)))))

  ;(vec (repeat n-topics (map (fn [doc-w doc-z] (map (fn [sent-w sent-z] ))) corpus z))))
  ;TODO
;  )

;combine individual utt-parse-hms

(defn -main [& args]
  (let [n-topics 4
        corpus (read-corpus)
        z (random-init-topic-boundaries corpus n-topics)
        phi-k (initialize-colloc-topics n-topics corpus z)]
;    (prn z))
    (prn phi-k))
 )
