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
     (if (every? pos? [n len])
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

(defn find-longest-solutions [letter-pool dictionary]
  (->> dictionary
       (sort-by #(.length %))
       reverse
       (filter #(enough-letters-for-word? letter-pool %))
       (map (fn [word]
              [word (find-word-chains dictionary word)]))
       (filter (fn [[word chains]]
                 (full-solution? chains)))))

(defn -main [& args]
  (println "Hello, Erudit!"))

(comment
  (def ru-pool (frequencies "истукановедение"))
  (def ru-dict
    #{"тук" "стук" "тукан" "истукан" "истукановед" "истукановедение"})
  (find-word-chains ru-dict "истукановедение")
  ;; => { "истукановед" { "истукан" { "стук" { "тук" {} }, "тук" {}, "тукан" { "тук" {} } }, "стук" { "тук" {} }, "тукан" { "тук" {} } } }
  (find-longest-solutions ru-pool ru-dict))
