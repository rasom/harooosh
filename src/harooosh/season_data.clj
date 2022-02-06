(ns harooosh.season-data
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [harooosh.common :as common]))

(defn season-data-files [season]
  (->> (str "prepared_data/" season)
       io/file
       file-seq
       (filter
        #(string/ends-with? (.getName %) ".edn"))))

(defn season-data [season]
  (->> (season-data-files season)
       (map (fn [file]
              (let [{:keys [started-at] :as match-day-data}
                    (common/data (.getAbsolutePath file))]
                {:name       (-> (.getName file)
                                 (string/split #"\.")
                                 first)
                 :started-at started-at
                 :data       match-day-data})))
       (sort-by :started-at)))

(defn update-points [acc results]
  (let [[[team3 {team3-p :p}]
         [team2 {team2-p :p}]
         [team1 {team1-p :p}]]
        (sort-by (fn [[_ {:keys [p]}]] p) results)
        add-points
        (fn [acc teams]
          (reduce
           (fn [acc [team p-main p-alternative]]
             (update acc team
                     (fn [t]
                       (-> t
                           (update :p-main + p-main)
                           (update :p-alternative + p-alternative)))))
           acc
           teams))]
    (cond (= team1-p team2-p team3-p)
          (add-points acc [[team1 3 4]
                           [team2 3 4]
                           [team3 3 4]])

          (= team1-p team2-p)
          (add-points acc [[team1 2 2]
                           [team2 2 2]
                           [team3 1 1]])

          (= team2-p team3-p)
          (add-points acc [[team1 3 4]
                           [team2 2 2]
                           [team3 2 2]])

          :else
          (add-points acc [[team1 3 4]
                           [team2 2 2]
                           [team3 1 1]]))))

(defn aggregate-results [data]
  (reduce
   (fn [acc {{:keys [results]} :data}]
     (-> (reduce
          (fn [acc team]
            (update acc team #(merge-with + % (team results))))
          acc
          (keys acc))
         (update-points results)))
   (zipmap [:yellow :red :white]
           (repeat 3 {:p-main 0, :p-alternative 0, :p 0,
                      :w 0, :d 0, :l 0, :gf 0, :ga 0, :gd 0}))
   data))
