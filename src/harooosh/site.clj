(ns harooosh.site
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [hiccup.core :as hiccup]))

(def current-season 2022)

(def teams
  {:white  "Белые"
   :red    "Красные"
   :yellow "Желтые"})

(def team-icon
  {:white  "bi-circle"
   :red    "bi-circle-fill"
   :yellow "bi-circle-fill"})

(def team-style
  {:white  nil
   :red    "color: red;"
   :yellow "color: gold"})

(defn get-team [team]
  [:span [:i.bi
         {:class (get team-icon team)
          :style (get team-style team)}]
   " " (get teams team)])

(defn dd-mm-year [timestamp]
  (let [f (java.text.SimpleDateFormat. "dd.MM.yyyy")
        date (java.util.Date. timestamp)]
    (.format f date)))

(defn to-h-min-sec [ms]
  (let [sec-len 1000
        min-len (* 60 sec-len)
        h-len   (* 60 min-len)
        hs      (quot ms h-len)
        mins    (quot (- ms (* hs h-len)) min-len)
        seconds (quot (- ms (* mins min-len) (* hs h-len)) sec-len)]
    (format "%02d:%02d:%02d" hs mins seconds)))

(defn data [file]
  (->
   (slurp file)
   (edn/read-string)))

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

(defn adjust-diff
  [diff
   correction
   {:keys [offset1 offset2 offset3
           battery-change1 battery-change2]}]
  (let [diff (if correction
               (+ diff (* correction 1000))
               diff)]
    (cond (and battery-change2
               (> diff (* 1000 battery-change2)))
          (- diff (* 1000 offset3))

          (and battery-change1
               (> diff (* 1000 battery-change1)))
          (- diff (* 1000 offset2))

          offset1
          (- diff (* 1000 offset1))

          :else
          diff)))

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

(defn prepare-data-cmd [{:keys [file season]
                         :or {season current-season}}]
  (let [raw-data (data (str "raw_data/" season "/" file))
        prepared-data (prepare-data raw-data)]
    (spit
     (str "prepared_data/" season "/" file)
     (with-out-str (pp/pprint prepared-data)))))

(defn check-active [props page current-page]
  (cond-> props
    (= page current-page)
    (assoc :aria-current true
           :class "active")))

(defn nav [page]
  [:nav.navbar.navbar-expand-lg.navbar-light.bg-light
   [:div.container-fluid
    [:a.navbar-brand
     {:href "#"}
     "ХАРОООШ!"]
    [:button.navbar-toggler
     {:type           "button"
      :data-bs-toggle "collapse"
      :data-bs-target "#navbarNav"
      :aria-controls  "navbarNav"
      :aria-expanded  "false"
      :aria-label     "Toggle navigation"}
     [:span.navbar-toggler-icon]]
    [:div#navbarNav.collapse.navbar-collapse
     [:ul.navbar-nav
      [:li.nav-item
       [:a.nav-link
        (check-active {:href "/"} :home page)
        "Главная"]]
      [:li.nav-item
       [:a.nav-link
        (check-active {:href "/game.html"} :stats page)
        "Сбор статистики"]]]]]])

(defn page [page-name & body]
  [:html
   {:lang :en}
   [:head
    [:meta {:charset "UTF-8"}]
    [:meta {:name   "viewport"
            :content "width=device-width, initial-scale=1"}]
    [:title "harooosh"]
    [:link
     {:href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"
      :rel "stylesheet"
      :integrity "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3"
      :crossorigin "anonymous"}]
    [:link {:rel"stylesheet"
            :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.7.2/font/bootstrap-icons.css"}]
    [:script {:src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"
              :integrity "sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p"
              :crossorigin "anonymous"}]
    [:style ".max-width {max-width: 30;}"]]
   (vec (concat [:body (nav page-name)] body))])

(defn goals-table [goals offsets]
  [:table.table.table-striped
   [:thead
    [:td "Время"]
    [:td "Команда"]
    [:td "Детали"]]
   [:tbody
    (for [{:keys [time-diff time-correction data details]} goals]
      (let [adjusted-time (adjust-diff time-diff time-correction offsets)]
        [:tr
         [:td [:a {:href "javascript:void(0);"
                   :onclick (format "setCurrentTime(%d)"
                                    (quot adjusted-time 1000))}
               (to-h-min-sec adjusted-time)]]
         [:td (get-team (:team data))]
         [:td details]]))]])

(defn results-table [results]
  [:table.table.table-bordered
   [:thead
    (let [th :th.max-width.overflow-hidden]
      [:tr
       [th {:title "Команда"} "Команда"]
       [th {:title "Очки"} "O"]
       [th {:title "Игры"} "И"]
       [th {:title "Выигрыши"} "В"]
       [th {:title "Hичьи"} "Н"]
       [th {:title "Поражения"} "П"]
       [th {:title "Забитые"} "ЗАБ"]
       [th {:title "Пропущенные"} "ПРО"]
       [th {:title "Разница"} "РАЗН"]])]
   [:tbody
    (for [[team {:keys [p w d l gf ga gd]}] (sort-by (fn [[_ {:keys [p]}]] p) > results)]
      ^{:keys (str team)}
      [:tr
       [:td (get-team team)]
       [:td [:span.fw-bold (str p)]]
       [:td (str (+ w d l))]
       [:td (str w)]
       [:td (str d)]
       [:td (str l)]
       [:td (str gf)]
       [:td (str ga)]
       [:td (str gd)]])]])

(defn matches-table [matches]
  [:table.table.table-striped.text-center
    [:tbody
     (for [{:keys [id team1 team2 team1-score team2-score]} matches]
       [:tr
        [:td (str "#" (inc id))]
        [:td {:style {:background-color team1}}
         (get-team team1)]
        [:td [:p.text-nowrap
              (str team1-score " : " team2-score)]]
        [:td {:style {:background-color team2}}
         (get-team team2)]])]])

(defn game-html
  [{:keys [youtube-id goals results matches started-at offsets]}]
  (page
   :match-day
   [:div.container-fluid
    [:div.row
     [:div.col-sm-12.overflow-auto
      [:h3 "Результат " (dd-mm-year started-at)]
      (results-table results)]
      [:div.col-sm-7 [:div#player [:h1 "Тут будет видео!"]]]
     [:div.col-sm-5.overflow-auto
      {:style "max-height: 400;"}
      (goals-table goals offsets)]
     [:div.col-sm-6
      [:h3 "Матчи"]
      (matches-table matches)]]
    (when youtube-id
      [:script
       (str
        "var tag = document.createElement('script');
      tag.src = \"https://www.youtube.com/iframe_api\";
      var firstScriptTag = document.getElementsByTagName('script')[0];
      firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);
      var player;
      function onYouTubeIframeAPIReady() {
        player = new YT.Player('player', {
          videoId:'" youtube-id "',
          width: '100%',
          height: 400
        });
      }
      function setCurrentTime(time) {
        player.seekTo(time);
      }")])]))

(defn generate-game-html [{:keys [file]}]
  (let [data (data (str "prepared_data/" current-season "/" file))
        html-edn (game-html data)
        html (hiccup/html html-edn)
        [file-name] (string/split file #"\.")]
    (spit (str "public/" current-season "/" file-name ".html") html)))

(defn hms-msecs [h m s]
  (*
   (+ (* h 60 60)
      (* m 60)
      s)
   1000))

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
                    (data (.getAbsolutePath file))]
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

(defn season-table [data]
  [:table.table.table-bordered
   [:thead
    (let [th :th.max-width.overflow-hidden]
      [:tr
       [th {:title "Команда"} "Команда"]
       [th {:title "Зачёт"} "З"]
       [th {:title "Очки"} "O"]
       [th {:title "Альтернативный Зачёт"} "АЗ"]
       [th {:title "Игры"} "И"]
       [th {:title "Выигрыши"} "В"]
       [th {:title "Hичьи"} "Н"]
       [th {:title "Поражения"} "П"]
       [th {:title "Забитые"} "ЗАБ"]
       [th {:title "Пропущенные"} "ПРО"]
       [th {:title "Разница"} "РАЗН"]])]
   [:tbody
    (for [[team {:keys [p-main p-alternative p w d l gf ga gd]}]
          (sort-by (fn [[_ {:keys [p-main]}]] p-main) > data)]
      [:tr
       [:td (get-team team)]
       [:td [:span.fw-bold (str p-main)]]
       [:td (str p)]
       [:td (str p-alternative)]
       [:td (str (+ w d l))]
       [:td (str w)]
       [:td (str d)]
       [:td (str l)]
       [:td (str gf)]
       [:td (str ga)]
       [:td (str gd)]])]])

(defn result-summary [{:keys [results]}]
  [:span
   (interpose
    ", "
    (for [[t {:keys [p]}]
          (sort-by (fn [[_ {:keys [p]}]] p) > results)]
      [:span (get-team t) " " [:span.fw-bold p]]))])

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
       [:td (match-day-link season name (str idx))]
       [:td (dd-mm-year started-at)]
       [:td (result-summary data)]])]])

(defn season-page-html [season data]
  (page
   :home
   [:div.container-fluid
    [:div.row
     [:div.col-sm-12.overflow-auto
      [:h3 "Таблица"]
      (season-table (aggregate-results data))]]
    [:div.row
     [:div.col-sm-6
      [:h3 "Игровые дни"]
      (match-days-table season data)]]]))

(defn season-page [season]
  (let [data (season-data season)]
    (season-page-html season data)))

(defn generate-season-page [season]
  (spit (if (= season current-season)
          (str "public/index.html")
          (str "public/" season ".html"))
        (hiccup/html (season-page season))))

(defn update-season [season]
  (doseq [file (season-data-files season)]
    (generate-game-html {:file (.getName file)})))

(defn stats-page []
  (page
   :stats
   [:div#harooosh-frame]
   [:script {:src "js/compiled/common.js"}]
   [:script {:src "js/compiled/game.js"}]))

(defn stats-list-page []
  (page
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
  (io/make-parents (str "public/" current-season "/index.html")))

(defn generate-site-cmd [_]
  (ensure-seson-dir-exists)
  (generate-stats-page)
  (generate-stats-list-page)
  (generate-season-page current-season)
  (update-season current-season))

(comment
  (generate-game-html {:file "16_01.edn"})

  "01 4 51"
  (stats-page)

  (generate-stats-page)
  
  (generate-stats-list-page)
  
  (update-season 2022)
  
  (game-html :1)
  (prepare-data-cmd  {:file "16_01.edn"})

  "https://www.youtube.com/embed/wglacyQqOf4"

  (dd-mm-year 1640504588368)

  (hms-msecs 0 55 31)

  (* 1000 (+ 44 (* 18 60)));; => 1124

  (season-data 2022)

)
