(ns harooosh.season-extra
  (:require
   [harooosh.common :as common]
   [harooosh.season-extra-stats :as extra-stats]))

(defn round-float
  ([n]
   (round-float n 2))
  ([n precision]
   (let [base (int (java.lang.Math/pow 10 precision))]
     (float (/ (java.lang.Math/round (* base (float n))) base)))))

(defn percentage [a b]
  (round-float (* 100 (/ a b))))

(defn print-days [days]
  (interpose
   ","
   (map (fn [{:keys [started-at name]}]
          (common/match-day-link
           common/current-season
           name
           (common/dd-mm-year started-at)))
        days)))

(defn print-days-with-teams [days]
  [:span
   (interpose
    "; "
    (map
     (fn [{:keys [teams]
           {:keys [started-at name]} :day}]
       [:span
        (interpose
         ","
         (map common/get-team teams))
        " "
        (common/match-day-link
         common/current-season
         name
         (str "(" (common/dd-mm-year started-at) ")"))])
     days))])

(defn match-day-row [stats title value-description value-key days-key]
  [:tr
   [:td title]
   [:td value-description ": " (get stats value-key) "; "
    (print-days-with-teams (get stats days-key))]])

(defn extra-stats-table [data aggregated-results]
  [:table.table.table-bordered.table-striped
   (let [total-games                           (count data)
         {:keys [total draws]}                 (extra-stats/count-matches data)
         {:keys [all-scores worst-result worst-days best-result best-days]
          :as stats}
         (extra-stats/get-scores-stats data)]
     [:tbody
      [:tr
       [:td "Всего игр"]
       [:td (str total)]]
      [:tr
       [:td "В среднем игр за тур"]
       [:td (str (round-float (/ total total-games)))]]
      [:tr
       [:td "Всего ничьих"]
       [:td (str draws " (" (percentage draws total) "%)")]]
      [:tr
       [:td "Среднее количество голов за тур"]
       [:td (round-float (/ (apply + all-scores) total-games))]]
      [:tr
       [:td "Самые результативные туры"]
       [:td (str "забитых голов: " best-result ", ")
        (print-days best-days)]]
      [:tr
       [:td "Наименее результативные туры"]
       [:td (str "забитых голов: " worst-result ", ")
        (print-days worst-days)]]
      (for [config
            [["Больше всего голов за тур" "забито голов" :best-score :best-score-days]
             ["Меньше всего голов за тур" "забито голов" :worst-score :worst-score-days]
             ["Больше всего пропущено за тур" "пропущено голов" :worst-defence :worst-defence-days]
             ["Меньше всего пропущено за тур" "пропущено голов" :best-defence :best-defence-days]
             ["Лучшая разница голов за тур" "разница" :best-gd :best-gd-days]
             ["Худшая разница голов за тур" "разница" :worst-gd :worst-gd-days]
             ["Больше всего очков за тур" "очков" :max-points :max-points-days]
             ["Меньше всего очков за тур" "очков" :min-points :min-points-days]
             ["Больше всего побед за тур" "игр" :max-wins :max-wins-days]
             ["Меньше всего побед за тур" "игр" :min-wins :min-wins-days]
             ["Больше всего ничьих за тур" "игр" :max-draws :max-draws-days]
             ["Меньше всего ничьих за тур" "игр" :min-draws :min-draws-days]
             ["Больше всего поражений за тур" "игр" :max-defeats :max-defeats-days]
             ["Меньше всего поражений за тур" "игр" :min-defeats :min-defeats-days]]]
        (apply match-day-row stats config))])])
