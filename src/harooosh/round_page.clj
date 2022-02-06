(ns harooosh.round-page
  (:require [clojure.string :as string]
            [harooosh.common :as common]
            [hiccup.core :as hiccup]))

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
       [:td (common/get-team team)]
       [:td [:span.fw-bold (str p)]]
       [:td (str (+ w d l))]
       [:td (str w)]
       [:td (str d)]
       [:td (str l)]
       [:td (str gf)]
       [:td (str ga)]
       [:td (str gd)]])]])

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
               (common/to-h-min-sec adjusted-time)]]
         [:td (common/get-team (:team data))]
         [:td details]]))]])

(defn matches-table [matches]
  [:table.table.table-striped.text-center
   [:tbody
    (for [{:keys [id team1 team2 team1-score team2-score]} matches]
      [:tr
       [:td (str "#" (inc id))]
       [:td {:style {:background-color team1}}
        (common/get-team team1)]
       [:td [:p.text-nowrap
             (str team1-score " : " team2-score)]]
       [:td {:style {:background-color team2}}
        (common/get-team team2)]])]])

(defn round-html
  [{:keys [youtube-id goals results matches started-at offsets]}]
  (common/page
   :match-day
   [:div.container-fluid
    [:div.row
     [:div.col-sm-12.overflow-auto
      [:h3 "Результат " (common/dd-mm-year started-at)]
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

(defn generate-round-html [{:keys [file]}]
  (let [data (common/data (str "prepared_data/" common/current-season "/" file))
        html-edn (round-html data)
        html (hiccup/html html-edn)
        [file-name] (string/split file #"\.")]
    (spit (str "public/" common/current-season "/" file-name ".html") html)))
