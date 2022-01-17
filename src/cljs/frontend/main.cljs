(ns frontend.main
(:require [reagent.core :as r]
          [reagent.dom :as rd]
          [hashp.core :include-macros true]))

;; NOTE: If mystic failures, do:
;; Stop frontend watch and backend repl
;; rm -rf node_modules
;; just clean
;; just init
;; start frontend watch and backend repl
;; connect cursive to backend repl and (reset)

;; If Clojure namespaces do not show:
;; Check in Cursive => Clojure Deps => you have the right aliases checked.
;; Try: Refresh Clojure Deps Project.




(defn home-page []
  [:section.section
   [:h1 "MY4CLOJURE FRONTEND"]])

;;; Setup ;;;

; TODO: https://clojureverse.org/t/how-to-deal-with-development-code-in-clojurescript/613
; One thing is to check for goog.DEBUG, which is what the Google Closure library also uses.
; In your ClojureScript compiler options you can add :closure-defines {goog.DEBUG false}
; for the production build, and {goog.DEBUG true} for the dev build,
; and then check for it in your code, (if goog.DEBUG ...)
; TODO: Using backend configuration to get DEBUG info.
(def debug? ^boolean goog.DEBUG)

(def debug true)

(defn dev-setup []
  (js/console.log "ENTER main dev-setup")
  (when debug
    (enable-console-print!)
    (println "dev mode")))


(defn ^:dev/after-load start []
  (js/console.log "ENTER main start")
  (rd/render [home-page] (.getElementById js/document "app")))

(defn ^:export init []
  (js/console.log "ENTER main init")
  (dev-setup)
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "ENTER main stop"))


