(ns erudit
  (:gen-class))

(defn multi-subset? [ms1 ms2]
  {:pre [(every? map? [ms1 ms2])]}
  (every? (fn [[x c]]
            (>= (get ms2 x 0) c))
          ms1))

(defn enough-letters-for-word? [letter-pool word]
  {:pre [(map? letter-pool)
         (string? word)]}
  (multi-subset? (frequencies word) letter-pool))

(defn enum-stems
  ([word]
   (set (enum-stems word 7)))
  ([word n]
   {:pre  [(string? word)]
    :post [(coll? %)]}
   (let [len (.length word)]
     (if (and (pos? n)
              (pos? len))
       (cons word
             (concat (enum-stems (subs word 1)           (dec n))
                     (enum-stems (subs word 0 (dec len)) (dec n))))
       [word]))))

(defn find-precursors [dictionary word]
  {:pre [(set? dictionary)
         (string? word)]}
  (->> (enum-stems word)
       (filter #(and (not= % word)
                     (contains? dictionary %)))))

(defn find-word-chains [dictionary word]
  {:pre [(set? dictionary)
         (string? word)]}
  (->> word
       (find-precursors dictionary)
       (map (fn [w]
              {w (find-word-chains dictionary w)})) ;; [] ?
       (into {})))

(defn full-solution? [solution]
  {:pre [(map? solution)]}
  (some (fn [[k v]]
          (or (-> k .length (<= 7))
              (full-solution? v)))
        solution))

(defn fits-on-board? [word]
  (-> word .length (<= 15)))

(defn longest-first-comp [^String a ^String b]
  (let [r (compare (.length b) (.length a))]
    (if (not= r 0)
      r
      (compare a b))))

(defn find-longest-solutions [letter-pool dictionary]
  (->> dictionary
       (filter fits-on-board?)
       (sort longest-first-comp)
       (filter #(enough-letters-for-word? letter-pool %))
       (map (fn [word]
              [word (find-word-chains dictionary word)]))
       (filter (fn [[word chains]]
                 (full-solution? chains)))))

(defn -main [& args]
  (println "Hello, Erudit!"))

(comment
  (sort longest-first-comp ["abc" "ab" "def" "cd"])

  (def ru-pool (frequencies "истукановедение"))
  (def ru-dict
    #{"тук" "стук" "тукан" "истукан" "истукановед" "истукановедение"})
  (find-word-chains ru-dict "истукановедение")
  ;; => { "истукановед" { "истукан" { "стук" { "тук" {} }, "тук" {}, "тукан" { "тук" {} } }, "стук" { "тук" {} }, "тукан" { "тук" {} } } }

  (def ru-pool
    {
\а 9
\б 2
\в 4
\г 3
\д 4
\е 9
\ж 2
\з 2
\и 8
\й 3
\к 5
\л 4
\м 4
\н 8
\о 8
\п 5
\р 4
\с 4
\т 3
\у 3
\ф 1
\х 2
\ц 1
\ч 2
\ш 1
\щ 1
\ы 2
\ь 2
\э 1
\ю 1
\я 3})

  (require '[clojure.string :as s])
  (def ru-dict
    (-> "ru-dict.txt"
        slurp
        (s/replace #"ё" "e")
        (s/split #"\r\n")
        set))

  (find-longest-solutions ru-pool ru-dict))
