(ns harooosh.season-page
  (:require
   [harooosh.common :as common]
   [harooosh.season-extra :as season-extra]
   [harooosh.season-data :as season-data]))

(def season-table-rows
  [[:p-main        {:title         "Основной зачёт (3-2-1)"
                    :value-wrapper (fn [v] [:span.fw-bold (str v)])}]
   [:p             {:title "Сумма очков за все туры"}]
   [:p-alternative {:title "Aльтернативный зачёт (4-2-1)"}]
   [:games         {:title "Всего игр"
                    :f     (fn [{:keys [w d l]}] (+ w d l))}]
   [:w             {:title "Победы"}]
   [:d             {:title "Ничьи"}]
   [:l             {:title "Поражения"}]
   [:gf            {:title "Забитые"}]
   [:ga            {:title "Пропущенные"}]
   [:gd            {:title "Разница голов"}]])

(def one-to-one-rows
  (remove
   (fn [[k]]
     (contains? #{:p-main :p-alternative :ga :gd :l :games} k))
   season-table-rows))

(defn sort-results [data]
  (sort-by (fn [[_ {:keys [p-main p]}]] (or p-main p)) > data))

(defn season-table
  ([data] (season-table data season-table-rows))
  ([data rows]
   (let [sorted-teams (sort-results data)
         teams-keys   (map first sorted-teams)]
     [:table.table.table-bordered.table-striped
      [:thead
       [:tr
        [:th]
        (for [team teams-keys]
          [:th (common/get-team team :min)])]]
      [:tbody
       (for [[k {:keys [title f value-wrapper]
                 :or {value-wrapper str
                      f k}}]
             rows]
         [:tr
          (concat
           [[:td title]]
           (for [team teams-keys]
             [:td (value-wrapper (f (get data team)))]))])]])))

(defn result-summary [{:keys [results]}]
  [:span
   (interpose
    ", "
    (for [[t {:keys [p]}]
          (sort-by (fn [[_ {:keys [p]}]] p) > results)]
      [:span.text-nowrap (common/get-team t) " " [:span.fw-bold p]]))])

(defn match-days-table [season all-days]
  [:table.table.table-bordered.text-center
   [:thead
    [:tr
     [:th "#"]
     [:th "Дата"]
     [:th "Результат"]]]
   [:tbody
    (for [[idx {:keys [name data started-at]}]
          (->> all-days
               (map-indexed
                (fn [idx data]
                  [(inc idx) data]))
               (sort-by (fn [[_ {:keys [started-at]}]] started-at) >))]
      [:tr
       [:td (common/match-day-link season name (str idx " Тур"))]
       [:td (common/dd-mm-year started-at)]
       [:td (result-summary data)]])]])

(def team-pairs [#{:yellow :red}
                 #{:white :red}
                 #{:white :yellow}])

(defn group-matches-by-pairs [{{:keys [matches]} :data}]
  (group-by (fn [{:keys [team1 team2]}] #{team1 team2}) matches))

(defn one-to-one-matches [stats]
  (reduce
   (fn [acc day]
     (merge-with concat acc (group-matches-by-pairs day)))
   (zipmap team-pairs (repeat 3 []))
   stats))

(defn calculate-results [pair matches]
  (reduce
   (fn [res {:keys [team1-score team2-score
                    team1 team2 winner
                    ended?]}]
     (if-not ended?
       res
       (let [res (-> res
                     (update-in [team1 :gf] + team1-score)
                     (update-in [team1 :ga] + team2-score)
                     (update-in [team1 :gd] + (- team1-score team2-score))
                     (update-in [team2 :gf] + team2-score)
                     (update-in [team2 :ga] + team1-score)
                     (update-in [team2 :gd] + (- team2-score team1-score)))]
         (cond
           (> team1-score team2-score)
           (-> res
               (update-in [team1 :p] + 3)
               (update-in [team1 :w] inc)
               (update-in [team2 :l] inc))

           (> team2-score team1-score)
           (-> res
               (update-in [team2 :p] + 3)
               (update-in [team2 :w] inc)
               (update-in [team1 :l] inc))

           (= team1-score team2-score)
           (cond-> (-> res
                       (update-in [team1 :p] inc)
                       (update-in [team1 :d] inc)
                       (update-in [team2 :p] inc)
                       (update-in [team2 :d] inc))

             winner
             (update-in [winner :p] inc))))))
   (zipmap pair
           (repeat (zipmap [:p :w :d :l :ga :gf :gd]
                           (repeat 0))))
   matches))

(defn one-to-one-stats [stats]
  (map (fn [[pair matches]]
         [pair (calculate-results pair matches)])
       (one-to-one-matches stats)))

(defn one-to-one-pair-header [pair stats]
  (let [[t1 t2] (vec pair)]
    [:h3 (common/get-team t1) " vs " (common/get-team t2)
     (let [games  (apply +
                         (-> stats
                             first
                             second
                             (select-keys [:w :d :l])
                             vals))]
       [:span.h5 " (всего матчей - " (str games) ")"])]))

(defn sorted-teams-pairs [data]
  (let [[t1 t2 t3] (keys (sort-results data))]
    [[t1 t2]
     [t1 t3]
     [t2 t3]]))

(defn season-page-html [season data]
  (common/page
   :home
   [:div.container-fluid
    (let [aggregated-results (season-data/aggregate-results data)]
      [:div.row
       [:div.col-sm-6.overflow-auto
        [:h3 "Таблица (сыграно туров - " (count data) ")"]
        (season-table aggregated-results)]
       [:div.col-sm-6
        [:h3 "Туры"]
        (match-days-table season data)]
       [:div.col-sm-6
        [:h3 "Дополнительная статистика"]
        (season-extra/extra-stats-table data aggregated-results)]
       [:div.col-sm-6
        (let [one-to-one-data (into {} (one-to-one-stats data))]
          (for [pair (sorted-teams-pairs aggregated-results)]
            (let [data (get one-to-one-data (set pair))]
              [:div.row
               (one-to-one-pair-header pair data)
               (season-table data one-to-one-rows)])))]])]))

(defn season-page [season]
  (let [data (season-data/season-data season)]
    (season-page-html season data)))
