(ns harooosh.main
  (:require [re-frame.core :as re-frame]
            [harooosh.common :as common]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handlers
(common/handler :new-game common/new-game)

(common/handler
 :delete-game
 (fn [db id]
   (update db :games dissoc id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subs



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; view

(defn root-view []
  (let [games (common/get-sub [:games])]
    [:div.container-fluid
     [:div.row
      [:div.col
       [:h3 "Список игр (сбор данных)"]]]
     [:div.row
      [:div.col.col-sm-4
       [:table.table.table-striped
        [:tbody
         (for [[id _] games]
           ^{:key id }
           [:tr
            [:td [:a {:href (str "game.html?id=" id)} (str "Игровой день #" id)]]
            [:td [:button.btn.btn-danger
                  {:on-click #(re-frame/dispatch [:delete-game id])}
                  "Удалить"]]])]]]]

     [:button.btn.btn-success
      {:on-click #(re-frame/dispatch [:new-game])}
      "Добавить игру"]]))

(defn ^:dev/after-load render []
  (common/render root-view))

(defn init []
  (common/init)
  (render))

(comment
  (re-frame/dispatch [:reset-db])


  )
