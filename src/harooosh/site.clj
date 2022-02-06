(ns harooosh.site
  (:require
   [clojure.java.io :as io]
   [harooosh.common :as common]
   [harooosh.prepared-data :as prepared-data]
   [harooosh.round-page :as round-page]
   [harooosh.season-data :as season-data]
   [hiccup.core :as hiccup]))

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

(defn match-day-link [season file content]
  [:a {:href (str season "/" file ".html")}
   content])

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
       [:td (match-day-link season name (str idx " Тур"))]
       [:td (common/dd-mm-year started-at)]
       [:td (result-summary data)]])]])

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

(defn round-float
  ([n]
   (round-float n 2))
  ([n precision]
   (let [base (int (java.lang.Math/pow 10 precision))]
     (float (/ (java.lang.Math/round (* base (float n))) base)))))

(defn percentage [a b]
  (round-float (* 100 (/ a b))))

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

(defn print-days [days]
  (interpose
   ","
   (map (fn [{:keys [started-at name]}]
          (match-day-link common/current-season name (common/dd-mm-year started-at)))
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
        (match-day-link common/current-season name
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
         {:keys [total draws]}                 (count-matches data)
         {:keys [all-scores worst-result worst-days best-result best-days]
          :as stats}
         (get-scores-stats data)]
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
        (extra-stats-table data aggregated-results)]
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

(defn generate-season-page [season]
  (spit (if (= season common/current-season)
          (str "public/index.html")
          (str "public/" season ".html"))
        (hiccup/html (season-page season))))

(defn update-season [season]
  (doseq [file (season-data/season-data-files season)]
    (round-page/generate-round-html {:file (.getName file)})))

(defn stats-page []
  (common/page
   :stats
   [:div#harooosh-frame]
   [:script {:src "js/compiled/common.js"}]
   [:script {:src "js/compiled/game.js"}]))

(defn stats-list-page []
  (common/page
   :stats-list
   [:div#harooosh-frame]
   [:script {:src "js/compiled/common.js"}]
   [:script {:src "js/compiled/main.js"}]))

(defn generate-stats-page []
  (spit "public/game.html"
        (hiccup/html (stats-page))))

(defn generate-stats-list-page []
  (spit "public/list.html"
        (hiccup/html (stats-list-page))))

;;(generate-season-page 2022)

(defn ensure-seson-dir-exists []
  (io/make-parents (str "public/" common/current-season "/index.html")))

(defn generate-site-cmd [_]
  (ensure-seson-dir-exists)
  (generate-stats-page)
  (generate-stats-list-page)
  (generate-season-page common/current-season)
  (update-season common/current-season))

(comment
  (generate-site-cmd nil)

  (generate-round-html {:file "16_01.edn"})

  "01 4 51"
  (stats-page)

  (generate-stats-page)

  (generate-stats-list-page)

  (update-season 2022)

  (round-html :1)
  (prepared-data/prepare-data-cmd  {:file "16_01.edn"})

  "https://www.youtube.com/embed/wglacyQqOf4"

  (common/dd-mm-year 1640504588368)

  (hms-msecs 0 55 31)

  (* 1000 (+ 44 (* 18 60)));; => 1124

  (season-data 2022))
