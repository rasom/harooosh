(ns harooosh.common
  (:require [re-frame.core :as re-frame]
            [reagent.dom :as reagent.dom]
            [clojure.edn :as clojure.edn]))

(def alert-interceptor
  (re-frame/after
   (fn [db]
     (js/localStorage.setItem "db" (prn-str db)))))

(def all-interceptors [alert-interceptor])

(defn handler [n h]
  (re-frame/reg-event-db
   n
   all-interceptors
   (fn [db [_ & params]]
     (apply h db params))))

(defn handler-fx [n h]
  (re-frame/reg-event-fx
   n
   all-interceptors
   (fn [{:keys [db] :as cofx} [_ & params]]
     (apply h cofx db params))))

(handler
 :init
 (fn [_ db]
   (clojure.edn/read-string db)))

(handler
 :reset-db
 (fn [_] {}))

(defn reg-root-sub [sub-key]
  (re-frame/reg-sub
   sub-key
   (fn [db] (get db sub-key))))

(reg-root-sub :games)

(defn get-sub [path]
  @(re-frame/subscribe path)) 

(defn render [root-view]
  (reagent.dom/render
   [root-view]
   (js/document.getElementById "harooosh-frame")))

(defn init []
  (re-frame/dispatch-sync [:init (js/localStorage.getItem "db")]))

(defn timestamp []
  (js/Date.now))

(defn to-local-time [timestamp]
  (-> timestamp
      (js/Date.)
      (.toLocaleTimeString "uk")))

(defn get-value [obj]
  (.-value (.-target obj)))

(defn new-game
  ([db] (new-game db nil))
  ([db id]
   (let [id (or
             id
             (some-> db
                     :games
                     first
                     val
                     :id
                     inc)
             1)]
     (update db :games
             (fnil assoc (sorted-map-by >))
             id
             {:id         id
              :points    {:yellow 0
                          :red    0
                          :white  0}
              :started?   false
              :created-at (timestamp)}))))

(comment

  )
