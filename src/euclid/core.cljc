(ns euclid.core
  (:require [euclid.handlers :as handlers]
            [ubik.core :as u]
            [ubik.hosts :as hosts]
            [ubik.interactive.core :as spray :include-macros true]
            [ubik.math :as math]))

#?(:cljs (enable-console-print!))

(defonce host (hosts/default-host {}))

;;;;; Subs

(spray/defsub shapes
  (:shapes @spray/db))

(spray/defsub control
  (handlers/detect-control-points @shapes))

(spray/defsub draw-mode
  (:draw-mode @spray/db))

;;;;; UI

(def hud [])

(defn text [t & [c]]
  (cond-> (u/scale (assoc u/text :text t) 2)
    c (u/translate c)))

(def point
  (assoc u/circle
         :radius 10
         :style {:fill    :#fdd017
                 :opacity 0.5
                 :stroke  :none}))

(def button-bg
  (-> u/rectangle
      (u/style {:fill :none :stroke :black})
       (u/scale 100)))

(def circle-button
  (u/tag [button-bg
          (assoc u/annulus :style {:stroke :none :fill :black}
                 :outer-radius 35
                 :inner-radius 33
                 :centre [50 50])]
         ::circle-button))

(def rule-button
  (-> [button-bg
       (-> u/rectangle
           (assoc :width (* 80 (math/sqrt 2)) :height 2)
           (u/style {:fill :black :stroke :none})
           (u/translate [10 9])
           (u/rotate [10 10] 45))
       (assoc point :centre [10 10] :radius 5)
       (assoc point :centre [90 90] :radius 5)]
      (u/tag ::rule-button)))

(def selected
  (assoc u/rectangle :width 100 :height 100
         :style {:fill :green :opacity 0.3}))

(spray/defsub control-panel
  [circle-button
   (-> [(-> (assoc u/text :text "pan")
            (u/scale 3)
            (u/translate [20 40]))
        button-bg]
       (u/translate [0 200])
       (u/tag ::pan))
   (u/translate rule-button [0 100])
   (condp = @draw-mode
     ::circle-button selected
     ::rule-button   (assoc selected :corner [0 100])
     ::pan (u/translate selected [0 200])
     [])])

(def problem-1
  [(text
    "Draw an equilateral triangle with the given line segment as its base."
    [0 300])

   (u/with-style {:stroke :magenta}
     (assoc u/line :from [150 0] :to [400 0]))])

(spray/defsub current-draw
  (when (handlers/valid-drag? (:current-drag @spray/db))
    (handlers/create-shape @draw-mode (:current-drag @spray/db))))



(spray/defsub world
  [(u/translate (u/translate @control-panel [0 100])
                [0 500])
   (or @shapes [])
   (or @current-draw [])
   (map #(assoc point :centre %) @control)])

(defn start-game []
  (spray/initialise!
   {:host host
    :init-db {}
    :handlers handlers/handlers
    ;; TODO: These effect handlers should be built in. I'm not exactly sure how
    ;; best to build them in though. I guess merge in default handlers.
    :effects {::handlers/undo [(fn [e] (ubik.interactive.db/undo!))]
              ::handlers/redo [(fn [e] (ubik.interactive.db/redo!))]
              ::handlers/checkpoint [(fn [e] (ubik.interactive.db/checkpoint!))]}
    :root world}))


(defn reset []
  (ubik.interactive.db/reset-db! :ubik.interactive.db/uninitialised)
  (start-game))

(defn ^:export init []
  (start-game))

(defn on-reload []
  (init))
