(ns euclid.core
  (:require [euclid.handlers :as handlers]
            [ubik.core :as u]
            [ubik.hosts :as hosts]
            [ubik.interactive.core :as spray :include-macros true]
            [ubik.math :as math]))

#?(:cljs (enable-console-print!))

(defonce host (hosts/default-host {}))

(def db @handlers/app-db)

;;;;; Subs

(def shapes
  (spray/subscription
   (:shapes @db)))

(def control
  (spray/subscription
   (handlers/detect-control-points @shapes)))

(def draw-mode
  (spray/subscription
   (:draw-mode @db)))

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

(def control-panel
  (spray/subscription
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
      [])]))

(def problem-1
  [(text
    "Draw an equilateral triangle with the given line segment as its base."
    [0 300])

   (u/with-style {:stroke :magenta}
     (assoc u/line :from [150 0] :to [400 0]))])

(def current-draw
  (spray/subscription
   (let [current (:current-drag @db)]
     (when (and (not (:complete? current)) (handlers/valid-drag? current))
       (handlers/create-shape @draw-mode current)))))

(def world
  (spray/subscription
   [(u/translate (u/translate @control-panel [0 100])
                 [0 500])
    (or @shapes [])
    (or @current-draw [])
    (map #(assoc point :centre %) @control)]))

(def undo-plugin
  (spray/undo-plugin
   {:events {:undo ::handlers/undo
             :redo ::handlers/redo
             :checkpoint ::handlers/checkpoint}
    :save-fn (fn [db] (:shapes db))
    :restore-fn (fn [db snapshot] (assoc db :shapes snapshot))
    :max-undo 50}))

(defn start-game []
  (spray/initialise!
   {:host host
    :plugins [undo-plugin]
    :handlers handlers/handlers
    ;; TODO: These effect handlers should be built in. I'm not exactly sure how
    ;; best to build them in though. I guess merge in default handlers.
    :root world}))


(defn reset []
  (ubik.interactive.db/reset-db! :ubik.interactive.db/uninitialised)
  (start-game))

(defn ^:export init []
  (start-game))

(defn on-reload []
  (init))
