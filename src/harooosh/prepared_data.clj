(ns harooosh.prepared-data
  (:require [harooosh.common :as common]
            [clojure.pprint :as pp]))

(defn prepare-results [{:keys [points results matches]}]
  (cond
    (number? (get-in points [:yellow :ga]))
    points
    (number? (get-in results [:yellow :ga]))
    results
    :else
    (->> matches
         vals
         (filter :ended?)
         (reduce
          (fn [acc {:keys [team1 team2 team1-score team2-score]}]
            (-> acc
                (update-in [team1 :gf] + team1-score)
                (update-in [team2 :ga] + team1-score)
                (update-in [team1 :gd] + (- team1-score team2-score))
                (update-in [team2 :gf] + team2-score)
                (update-in [team1 :ga] + team2-score)
                (update-in [team2 :gd] + (- team2-score team1-score))))
          (zipmap (keys points) (repeat {:gf 0, :ga 0, :gd 0})))
         (merge-with merge points))))

(defn prepare-goals [{:keys [log started-at]}]
  (->> log
       (filter (fn [{:keys [event]}]
                 (= event :goal-scored)))
       (map (fn [{:keys [time details] :as event}]
              (let [diff (- time started-at)]
                (cond-> (assoc event :time-diff diff)
                  (not details)
                  (assoc :details "")))))
       (sort-by :time)))

(defn prepare-matches [{:keys [matches]}]
  (->> (vals matches)
       (sort-by :id)))

(defn prepare-data [{:keys [started-at] :as data}]
  {:youtube-id    nil
   :offsets      {:offset1         nil
                  :offset2         nil
                  :offset3         nil
                  :battery-change1  nil
                  :battery-change2 nil}
   :started-at   started-at
   :goals        (prepare-goals data)
   :results      (prepare-results data)
   :matches      (prepare-matches data)})

(defn prepare-data-cmd
  [{:keys [file season]
    :or {season common/current-season}}]
  (let [raw-data (common/data (str "raw_data/" season "/" file))
        prepared-data (prepare-data raw-data)]
    (spit
     (str "prepared_data/" season "/" file)
     (with-out-str (pp/pprint prepared-data)))))
