(ns harooosh.game
  (:require
   [re-frame.core :as re-frame]
   [reagent.core :as reagent]
   [goog.string :as gstring]
   [goog.string.format]
   [harooosh.common :as common]
   [clojure.set :as clojure.set]))

(def teams
  {:yellow {:name "Желтые"}
   :red    {:name "Красные"}
   :white  {:name "Белые"}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; timer
(common/reg-root-sub :timer-time)
(common/reg-root-sub :timer-running?)

(re-frame/reg-fx
 :schedule-timer-update
 (fn []
   (re-frame/dispatch
    [:set-timer-interval
     (js/setInterval
      #(re-frame/dispatch [:update-timer])
      999)])))

(common/handler-fx
 :check-timer
 (fn [_ {:keys [timer-running?]}]
   (when timer-running?
     {:schedule-timer-update nil})))

(common/handler
 :set-timer-interval
 (fn [db interval]
   (assoc db :timer-interval interval)))

(common/handler-fx
 :start-timer
 (fn [_ {:keys [timer-time] :as db}]
   {:db (assoc db
               :timer-running?  true
               :last-timer-time (common/timestamp)
               :timer-time      (or timer-time 0))
    :schedule-timer-update nil}))

(re-frame/reg-fx
 :cancel-interval
 (fn [interval]
   (js/clearInterval interval)))

(common/handler-fx
 :stop-timer
 (fn [_ {:keys [timer-interval] :as db}]
   {:db (assoc db :timer-running? false)
    :cancel-interval timer-interval}))

(common/handler-fx
 :update-timer
 (fn [_ {:keys [last-timer-time] :as db}]
   (let [next-last-time (common/timestamp)]
     {:db (-> db
              (assoc :last-timer-time next-last-time)
              (update :timer-time + (- next-last-time last-timer-time)))})))

(common/handler
 :reset-timer
 (fn [db]
   (assoc db
          :last-timer-time (common/timestamp)
          :timer-time      0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handlers

(defn add-log
  ([db event] (add-log db event nil))
  ([{:keys [current-game-id] :as db} event data]
   (update-in db [:games current-game-id :log]
              conj
              (cond-> {:event event
                       :time  (common/timestamp)}
                data
                (assoc :data data)))))

(defn set-game-id
  [{:keys [current-game-id] :as db} id-str]
  (let [id (if id-str
             (js/parseInt id-str)
             (or current-game-id 1))
        game (get-in db [:games id])]
    (cond-> (assoc db :current-game-id id)

      (not game)
      (common/new-game id))))

(common/handler
 :set-game-id
 set-game-id)

(common/handler
 :next-game
 (fn [{:keys [games] :as db}]
   (set-game-id db (inc (or (apply max (keys games)) 1)))))

(common/handler
 :start-game
 (fn [db id]
   (update-in db [:games id]
              assoc
              :started-at (common/timestamp)
              :started? true)))

(common/handler
 :select-team1
 (fn [{:keys [current-game-id] :as db} team]
   (assoc-in db [:games current-game-id :team1] team)))

(common/handler
 :select-team2
 (fn [{:keys [current-game-id] :as db} team]
   (assoc-in db [:games current-game-id :team2] team)))

(defn current-game-path [{:keys [current-game-id]}]
  [:games current-game-id])

(defn current-game [{:keys [current-game-id] :as db}]
  (get-in db [:games current-game-id]))

(defn new-match [{:keys [current-game-id] :as db} team1 team2]
  (let [{:keys [current-match-id]} (current-game db)
        next-match-id ((fnil inc -1) current-match-id)]
    (-> db
        (update-in [:games current-game-id]
                   (fn [game]
                     (-> game
                         (assoc-in [:matches next-match-id]
                                   {:id    next-match-id
                                    :team1 team1
                                    :team2 team2
                                    :team1-score 0
                                    :team2-score 0})
                         (assoc :current-match-id next-match-id
                                :team1 team1
                                :team2 team2))))
        (add-log :match-started {:id    next-match-id
                                 :team1 team1
                                 :team2 team2}))))

(common/handler
 :save-initial-teams
 (fn [{:keys [current-game-id] :as db}]
   (let [{:keys [team1 team2]}
         (get-in db [:games current-game-id])]
       (-> db
           (assoc-in [:games current-game-id :initial-teams?] true)
           (add-log :new-match-day)
           (new-match team1 team2)))))

(common/handler
 :goal
 (fn [{:keys [current-game-id] :as db}]
   (assoc-in db [:games current-game-id :goal-selection?] true)))

(defn current-match-path [{:keys [current-game-id] :as db}]
  (let [{:keys [current-match-id]} (current-game db)]
    [:games current-game-id
     :matches current-match-id]))

(defn calculate-points [db]
  (let [{:keys [matches id]} (current-game db)]
    (assoc-in db [:games id :points]
              (reduce
               (fn [res [_ {:keys [team1-score team2-score
                                   team1 team2 winner
                                   ended?]}]]
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
               (zipmap (keys teams)
                       (repeat (zipmap [:p :w :d :l :ga :gf :gd]
                                       (repeat 0))))
               matches))))

(defn get-winner
  [{:keys [team1 team2 team1-score team2-score winner]}]
  (cond
    winner winner

    (> team1-score team2-score)
    team1

    (> team2-score team1-score)
    team2

    :else nil))

(defn next-match [db {:keys [team1 team2] :as match}]
  (let [all-teams  (set (keys teams))
        winner     (get-winner match)
        next-team2 (-> all-teams
                       (clojure.set/difference #{team1 team2})
                       first)
        next-team1 (or winner team2)]
    (new-match db next-team1 next-team2)))

(defn end-current-match
  [{:keys [current-game-id] :as db}]
  (let [match-path (current-match-path db)
        {:keys [team1-score team2-score winner] :as match}
        (get-in db match-path)]
    (if (and (= team1-score
                team2-score)
             (nil? winner)
             (= 1 (count (get-in db [:games current-game-id :matches]))))
      (assoc-in db [:games current-game-id :draw-in-first-game?] true)
      (do
        ;; i'm too lazy to do it right
        #(re-frame/dispatch [:reset-timer])
        (-> db
            (update-in match-path assoc :ended? true)
            (add-log :match-ended match)
            calculate-points
            (next-match match))))))

(common/handler :match-ended end-current-match)

(defn penalties-winner
  [{:keys [current-game-id] :as db} winner]
  (let [match-path (current-match-path db)]
    (-> db
        (update-in match-path assoc :winner winner)
        (assoc-in [:games current-game-id :draw-in-first-game?] false)
        (end-current-match))))

(common/handler :penalties-winner penalties-winner)

(defn consider-ending-match
  [db]
  (let [{:keys [team1-score team2-score]}
        (get-in db (current-match-path db))]
    (if (or (= 2 team1-score)
            (= 2 team2-score))
      (end-current-match db)
      db)))

(common/handler
 :goal-scored
 (fn [{:keys [current-game-id] :as db} team]
   (let [match-path (current-match-path db)
         {:keys [team1 team2 team1-score team2-score]}
         (get-in db match-path)
         new-team1-score (if (= team team1)
                           (inc team1-score)
                           team1-score)

         new-team2-score (if (= team team2)
                           (inc team2-score)
                           team2-score)]
     (-> db
         (assoc-in [:games current-game-id :goal-selection?] false)
         (update-in match-path
                    (fn [match]
                      (-> match
                          (assoc :team1-score new-team1-score
                                 :team2-score new-team2-score)
                          (update :goals
                                  (fnil conj [])
                                  {:team      team
                                   :timestamp (common/timestamp)}))))
         (add-log :goal-scored {:team team})
         consider-ending-match))))

(defn edit-match-result
  [{:keys [current-game-id] :as db} id]
  (assoc-in db [:games current-game-id
                :matches id :editing-result?] true))

(common/handler :edit-match edit-match-result)

(defn update-score
  [{:keys [current-game-id] :as db} match-id team score]
  (let [match-path [:games current-game-id :matches match-id]
        {:keys [team1]} (get-in db match-path)

        score-key (if (= team team1)
                    :new-team1-score
                    :new-team2-score)]
    (update-in db match-path assoc score-key
               (if (js/isNaN score)
                 nil
                 score))))

(common/handler :update-score update-score)

(defn cancel-editing
  [{:keys [current-game-id] :as db} match-id]
  (let [match-path [:games current-game-id :matches match-id]]
    (update-in db match-path dissoc
               :editing-result? :new-team1-score :new-team2-score)))

(common/handler :cancel-editing-match cancel-editing)

(defn save-edited-match
  [{:keys [current-game-id] :as db} match-id]
  (let [match-path [:games current-game-id :matches match-id]
        {:keys [new-team1-score new-team2-score] :as match}
        (get-in db match-path)]
    (-> db
        (add-log :match-results-edited match)
        (update-in match-path assoc
                   :team1-score new-team1-score
                   :team2-score new-team2-score)
        (cancel-editing match-id)
        (calculate-points))))

(common/handler :save-edited-match save-edited-match)

(defn delete-match
  [{:keys [current-game-id] :as db} match-id]
  (let [match-path [:games current-game-id :matches match-id]
        match      (get-in db match-path)]
    (-> db
        (add-log :match-deleted match)
        (update-in [:games current-game-id :matches]
                   dissoc match-id)
        (calculate-points))))

(common/handler :delete-match delete-match)

(common/handler
 :toggle-pro
 (fn [db]
   (update-in db (current-game-path db)
              update :pro? not)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subs

(common/reg-root-sub :current-game-id)

(re-frame/reg-sub
 :game
 :<- [:games]
 :<- [:current-game-id]
 (fn [[games current-game-id]]
   (get games current-game-id)))

(re-frame/reg-sub
 :match
 :<- [:game]
 (fn [{:keys [current-match-id matches]}]
   (get matches current-match-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; view

(defn to-min-sec [ms]
  (let [mins (js/Math.floor (/ ms 60000))]
    (gstring/format
     "%02d:%02d"
     mins
     (js/Math.floor (/ (- ms (* mins 60000)) 1000)))))

(defn to-h-min-sec [ms]
  (let [h    (js/Math.floor (/ ms (* 60 60000)))
        mins (js/Math.floor (/ (- ms (* h (* 60 60000))) 60000))]
    (gstring/format
     "%02d:%02d:%02d"
     h
     mins
     (js/Math.floor (/ (- ms (* mins 60000)) 1000)))))

(defn timer []
  (let [timer-running? @(re-frame/subscribe [:timer-running?])
        time @(re-frame/subscribe [:timer-time])]
    [:div.row
     [:div.col.col-12
      [:p.text-center "ТАЙМЕР"]
      [:h2.text-center (to-min-sec time)]]
     [:div.col.col-xs-6
      [:p.text-center
       (if-not timer-running?
         [:i.bi-play-circle-fill.text-success
          {:style {:font-size 48}
           :on-click #(re-frame/dispatch [:start-timer])}]
         [:i.bi-pause-circle-fill.text-warning
          {:style    {:font-size 48}
           :on-click #(re-frame/dispatch [:stop-timer])}])]]
     [:div.col.col-xs-6
      [:p.text-center
       [:i.bi-x-circle-fill.text-danger
        {:style    {:font-size 48}
         :on-click #(re-frame/dispatch [:reset-timer])}]]]]))

(defn team-select [handler]
  [:select.form-control
   {:on-change #(handler (keyword (common/get-value %)))}
   [:option {:value :none} "------"]
   (for [[team {:keys [name]}] teams]
     ^{:key team}
     [:option
      {:value team
       :style {:background-color team}}
      name])])

(defn team-button [team handler]
  [:div.col.col-sm-4.d-flex.align-items-center
   {:style    {:height           "100%"
               :background-color team
               :flex-direction   :column}
    :on-click #(when (js/confirm "Точно?")
                 (handler))}
   [:div.d-flex.align-items-center
    {:style {:flex-direction :row
             :height         "100%"}}
    [:p.user-select-none
     (get-in teams [team :name])]]])

(defn matches-history [matches pro?]
  [:div.row
   [:h4.text-center "История"]
   [:table.table.table-bordered.text-center
    [:tbody
     (for [[id {:keys [team1 team2
                       team1-score team2-score
                       new-team1-score new-team2-score
                       editing-result?]}]
           (reverse (butlast matches))]
       ^{:keys id}
       [:tr
        [:td (str "#" (inc id))]
        [:td {:style {:background-color team1}}
         (get-in teams [team1 :name])]
        (if editing-result?
          [:td
           [:select.form-control
            {:on-change #(re-frame/dispatch
                          [:update-score
                           id
                           team1
                           (js/parseInt (common/get-value %))])}
            [:option "---"]
            [:option "0"]
            [:option "1"]
            [:option "2"]]
           " : "
           [:select.form-control
            {:on-change #(re-frame/dispatch
                          [:update-score
                           id
                           team2
                           (js/parseInt (common/get-value %))])}
            [:option "---"]
            [:option "0"]
            [:option "1"]
            [:option "2"]]]
          [:td [:span.text-nowrap
                (str team1-score " : " team2-score)]])
        [:td {:style {:background-color team2}}
         (get-in teams [team2 :name])]
        (when pro?
          (if editing-result?
            [:td
             [:button.btn.btn-success.bi-x-square
              {:on-click #(re-frame/dispatch [:cancel-editing-match id])}]
             (when (and new-team1-score new-team2-score)
               [:button.btn.btn-warning.bi-save
                {:on-click #(re-frame/dispatch [:save-edited-match id])}])]
            [:td
             [:button.btn.btn-default.bi-pencil-square
              {:on-click #(re-frame/dispatch [:edit-match id])}]
             [:button.btn.btn-danger.bi-x-square
              {:on-click #(when (js/confirm "Точно?")
                            (re-frame/dispatch
                             [:delete-match id]))}]]))])]]])

(defn end-match []
  [:div.col.col-sm-4.d-flex.align-items-center
   {:on-click #(when (js/confirm "Точно?")
                 (re-frame/dispatch [:match-ended]))
    :style    {:height           "100%"
               :background-color :gray
               :flex-direction   :column}}
   [:div.d-flex.align-items-center
    {:style {:flex-direction :row
             :height         "100%"}}
    [:p.user-select-none "Время!"]]])

(defn copy [game]
  (if true #_(.includes
       #js ["iPad Simulator"
            "iPhone Simulator"
            "iPod Simulator"
            "iPad"
            "iPhone"
            "iPod"]
       js/navigator.platform)
    (let [show? (reagent/atom false)]
      (fn [game]
        [:div
         {:style {:margin-top 42}}
         [:p.user-select-none
          {:on-click #(swap! show? not)}
          "Данные"]
         (when @show?
           [:div.input-group
            [:textarea.from-control
             (prn-str game)]])]))
    (let [copied (reagent/atom false)]
      (fn [game]
        [:div {:style {:margin-top 42}
               :on-click #(do
                            (reset! copied true)
                            (js/setTimeout
                              (fn []
                                (reset! copied false))
                              (* 3 1000))
                            (js/navigator.clipboard.writeText (prn-str game)))}
         [:button.btn.btn-light
          "Скопировать данные"
          (when @copied
            [:i.bi.bi-check
             {:style {:color :green}}])]]))))

(def small :small.span)

(defn root-view []
  (let [{:keys [id started? initial-teams? draw-in-first-game?
                team1 team2 points matches pro?] :as game}
        (common/get-sub [:game])
        {:keys [team1-score team2-score]}
        (common/get-sub [:match])]
    [:div.container-fluid
     [:div.row
      [:div.col.col-xs-7 [timer]]
      [:div.col.col-xs-5
       [:p "Результат"]
       [:table.table.table-bordered
        [:thead
         [:tr
          [:th [small "К"]]
          [:th [small "О"]]
          [:th [small "И"]]
          [:th [small "В"]]
          [:th [small "H"]]
          [:th [small "П"]]]]
        [:tbody
         (for [[team {:keys [p w d l]}] (sort-by (fn [[_ {:keys [p]}]] p) > points)]
           ^{:keys (str team)}
           [:tr {:style {:background-color team}}
            [:td [small (apply str (take 3 (get-in teams [team :name])))]]
            [:td [small (str (or p 0))]]
            [:td [small (str (+ w d l))]]
            [:td [small (str (or w 0))]]
            [:td [small (str (or d 0))]]
            [:td [small (str (or l 0))]]])]]]]

     (cond
       (not started?)
       [:div.row
        [:div.col-sm-12
         [:button.btn.btn-primary
          {:on-click #(re-frame/dispatch [:start-game id])}
          "Начать игру"]]]

       (and started? (not initial-teams?))
       [:div.row
        [:div.col.col-sm-4
         [team-select #(re-frame/dispatch [:select-team1 %])]]
        [:div.col.col-sm-4
         [team-select #(re-frame/dispatch [:select-team2 %])]]
        [:div.col.col-sm-2
         (when (and team1 team2 (not= team1 team2))
           [:button.btn.btn-success
            {:on-click #(re-frame/dispatch [:save-initial-teams])}
            [:i.bi-save
             {:role :img}]])]]

       draw-in-first-game?
       [:div.row {:style
              {:margin-top 24
               :margin-bottom 24}}
        [:div.col.col-sm-12.text-center
         {:style {:margin-bottom 8}}
         [:h3 "Кто выиграл серию пенальти?"]]
        [:div.row
         {:style {:height 100}}
         [team-button team1
          #(re-frame/dispatch [:penalties-winner team1])]
         [team-button team2
          #(re-frame/dispatch [:penalties-winner team2])]]]

       :else
       [:div.row
        [:div.col-sm-12
         [:h3.text-center
          (str (-> teams team1 :name) " " (str team1-score)
               " : "
               (str team2-score) " " (-> teams team2 :name))]]
        [:div.col-sm-12
         [:div.row
          {:style {:height 100
                   :border "1px solid grey"}}
          [team-button team1
           #(re-frame/dispatch [:goal-scored team1])]
          [end-match]
          [team-button team2
           #(re-frame/dispatch [:goal-scored team2])]]]]) 
     [matches-history matches pro?]
     [copy game]
     [:div.row
      [:div.col
       [:a.btn.btn-primary
        {:href "/list.html"}
        "Открыть список игр"]]]
     [:div.row
      [:div.col
       [:button.btn.btn-danger
        {:on-click #(re-frame/dispatch [:next-game])}
        "Начать новую игру"]]]

     [:div {:style {:margin-top 42}
            :on-click #(re-frame/dispatch [:toggle-pro])}
      "pro"]
     #_[:div {:style {:margin-top 24}}
      [:div "Log"]
      [:table
       {:border 1
        :cellspacing 0}
       (for [{:keys [time event data]} log]
         ^{:key (str time event)}
         [:tr
          [:td {:style {:width 100}} (common/to-local-time time)]
          [:td {:style {:width 100}} event]
          [:td (when data (prn-str data))]])]]]))

(defn ^:dev/after-load render []
  (common/render root-view))

(defn init []
  (common/init)
  (re-frame/dispatch-sync
   [:set-game-id (some-> js/window.location.href
                         js/URL.
                         .-searchParams
                         (.get "id"))])
  (re-frame/dispatch [:check-timer])
  (render))

(comment
  (re-frame/dispatch [:set-game-id 1])
  (re-frame/dispatch [:reset-db])
  (js/isNaN (js/parseInt "---"))
)
