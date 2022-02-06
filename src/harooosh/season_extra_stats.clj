(ns harooosh.season-extra-stats)

(defn get-total-scored [{:keys [results]}]
  (reduce
   (fn [acc [_ {:keys [ga]}]]
     (+ acc ga))
   0
   results))

(defn check-for-best-result [{:keys [best-result] :as acc} total-scored day]
  (cond (> best-result total-scored)
        acc

        (= best-result total-scored)
        (update acc :best-days conj day)

        :else
        (assoc acc
               :best-result total-scored
               :best-days [day])))

(defn check-for-worst-result [{:keys [worst-result] :as acc} total-scored day]
  (cond (> total-scored worst-result)
        acc

        (= worst-result total-scored)
        (update acc :worst-days conj day)

        :else
        (assoc acc
               :worst-result total-scored
               :worst-days [day])))

(defn get-some-value-from-results [results comp-fn value-fn initial-value]
  (reduce
   (fn [{:keys [value] :as acc} [team r]]
     (let [next-value (value-fn r)]
       (cond (comp-fn value next-value)
             acc

             (= value next-value)
             (update acc :teams conj team)

             :else
             {:teams [team]
              :value next-value})))
   {:teams []
    :value initial-value}
   results))

(defn check-something-from-results-per-day
  [value-key days-key comp-fn value-fn initial-value]
  (fn [acc day]
    (let [results       (get-in day [:data :results])
          current-value (get acc value-key initial-value)
          {:keys [teams value]}
          (get-some-value-from-results results comp-fn value-fn initial-value)]
      (cond (comp-fn current-value value)
            acc

            (= current-value value)
            (update acc days-key conj {:teams teams
                                       :day   day})

            :else
            (assoc acc
                   days-key [{:teams teams
                              :day   day}]
                   value-key value)))))

(def check-best-scorer-per-day
  (check-something-from-results-per-day
   :best-score :best-score-days
   > :gf 0))

(def check-worst-scorer-per-day
  (check-something-from-results-per-day
   :worst-score :worst-score-days
   < :gf 1000))

(def check-worst-defence-per-day
  (check-something-from-results-per-day
   :worst-defence :worst-defence-days
   > :ga 0))

(def check-best-defence-per-day
  (check-something-from-results-per-day
   :best-defence :best-defence-days
   < :ga 1000))

(def check-best-gd-per-day
  (check-something-from-results-per-day
   :best-gd :best-gd-days
   > :gd 0))

(def check-worst-gd-per-day
  (check-something-from-results-per-day
   :worst-gd :worst-gd-days
   < :gd 1000))

(def check-max-points-per-day
  (check-something-from-results-per-day
   :max-points :max-points-days
   > :p 0))

(def check-min-points-per-day
  (check-something-from-results-per-day
   :min-points :min-points-days
   < :p 1000))

(def check-max-wins-per-day
  (check-something-from-results-per-day
   :max-wins :max-wins-days
   > :w 0))

(def check-max-draws-per-day
  (check-something-from-results-per-day
   :max-draws :max-draws-days
   > :d 0))

(def check-max-defeats-per-day
  (check-something-from-results-per-day
   :max-defeats :max-defeats-days
   > :l 0))

(def check-min-wins-per-day
  (check-something-from-results-per-day
   :min-wins :min-wins-days
   < :w 1000))

(def check-min-draws-per-day
  (check-something-from-results-per-day
   :min-draws :min-draws-days
   < :d 1000))

(def check-min-defeats-per-day
  (check-something-from-results-per-day
   :min-defeats :min-defeats-days
   < :l 1000))

(defn check-results [acc day]
  (reduce
   (fn [acc f] (f acc day))
   acc
   [check-best-scorer-per-day
    check-worst-scorer-per-day
    check-worst-defence-per-day
    check-best-defence-per-day
    check-best-gd-per-day
    check-worst-gd-per-day
    check-max-points-per-day
    check-min-points-per-day
    check-max-wins-per-day
    check-max-draws-per-day
    check-max-defeats-per-day
    check-min-wins-per-day
    check-min-draws-per-day
    check-min-defeats-per-day]))

(defn get-scores-stats [data]
  (reduce
   (fn [acc {:keys [data] :as day}]
     (let [total-scored (get-total-scored data)]
       (-> acc
           (update :all-scores conj total-scored)
           (check-for-best-result total-scored day)
           (check-for-worst-result total-scored day)
           (check-results day))))
   {:best-result        0
    :worst-result       1000
    :best-days          []
    :worst-days         []
    :all-scores         []}
   data))

(defn count-draws [matches]
  (reduce
   (fn [acc {:keys [team1-score team2-score]}]
     (if (= team1-score team2-score)
       (inc acc)
       acc))
   0
   matches))

(defn count-matches [data]
  (reduce
   (fn [acc {:keys [data]}]
     (let [matches (get data :matches)]
       (-> acc
           (update :total + (count matches))
           (update :draws + (count-draws matches)))))
   {:total 0
    :draws 0}
   data))
