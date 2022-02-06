(ns harooosh.site
  (:require
   [clojure.java.io :as io]
   [harooosh.common :as common]
   [harooosh.prepared-data :as prepared-data]
   [harooosh.round-page :as round-page]
   [harooosh.season-data :as season-data]
   [harooosh.season-extra :as season-extra]
   [hiccup.core :as hiccup]
   [harooosh.season-page :as season-page]))

(defn generate-season-page [season]
  (spit (if (= season common/current-season)
          (str "public/index.html")
          (str "public/" season ".html"))
        (hiccup/html (season-page/season-page season))))

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
