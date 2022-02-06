(ns harooosh.common
  (:require [clojure.edn :as edn]))

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

(defn get-team
  ([team]
   (get-team team :max))
  ([team type]
   [:span.text-nowrap
    [:i.bi
     {:class (get team-icon team)
      :style (get team-style team)}]
    " "
    (let [n (get teams team)]
      (if (= :min type)
        (take 1 n)
        n))]))

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

(defn hms-msecs [h m s]
  (*
   (+ (* h 60 60)
      (* m 60)
      s)
   1000))

(defn data [file]
  (->
   (slurp file)
   (edn/read-string)))

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
    [:link {:rel "stylesheet"
            :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.7.2/font/bootstrap-icons.css"}]
    [:script {:src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"
              :integrity "sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p"
              :crossorigin "anonymous"}]
    [:style ".max-width {max-width: 30;}"]]
   (vec (concat [:body (nav page-name)] body))])
