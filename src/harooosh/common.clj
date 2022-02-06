(ns harooosh.common)

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
