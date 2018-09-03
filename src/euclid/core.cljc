(ns euclid.core
  (:require [euclid.handlers :as handlers]
            [lemonade.core :as l]
            [lemonade.hosts :as hosts]
            [ubik.core :as spray :include-macros true]
            [lemonade.math :as math]
            [ubik.process :as process]
            [ubik.rt :as rt]))

#?(:cljs (enable-console-print!))

(defonce host (hosts/default-host {}))

;;;;; Subs

(def shapes
  (spray/subscription
   (:shapes @handlers/app-db)))

(def control
  (spray/subscription
   (handlers/detect-control-points @shapes)))

(def draw-mode
  (spray/subscription
   (:draw-mode @handlers/app-db)))

;;;;; UI

(def hud [])

(defn text [t & [c]]
  (cond-> (l/scale (assoc l/text :text t) 2)
    c (l/translate c)))

(def point
  (assoc l/circle
         :radius 10
         :style {:fill    :#fdd017
                 :opacity 0.5
                 :stroke  :none}))

(def button-bg
  (-> l/rectangle
      (l/style {:fill :none :stroke :black})
       (l/scale 100)))

(def circle-button
  (l/tag [button-bg
          (assoc l/annulus :style {:stroke :none :fill :black}
                 :outer-radius 35
                 :inner-radius 33
                 :centre [50 50])]
         ::circle-button))

(def rule-button
  (-> [button-bg
       (-> l/rectangle
           (assoc :width (* 80 (math/sqrt 2)) :height 2)
           (l/style {:fill :black :stroke :none})
           (l/translate [10 9])
           (l/rotate [10 10] 45))
       (assoc point :centre [10 10] :radius 5)
       (assoc point :centre [90 90] :radius 5)]
      (l/tag ::rule-button)))

(def selected
  (assoc l/rectangle :width 100 :height 100
         :style {:fill :green :opacity 0.3}))

(def control-panel
  (spray/subscription
   [circle-button
    (-> [(-> (assoc l/text :text "pan")
             (l/scale 3)
             (l/translate [20 40]))
         button-bg]
        (l/translate [0 200])
        (l/tag ::pan))
    (l/translate rule-button [0 100])
    (condp = @draw-mode
      ::circle-button selected
      ::rule-button   (assoc selected :corner [0 100])
      ::pan (l/translate selected [0 200])
      [])]))

(def problem-1
  [(text
    "Draw an equilateral triangle with the given line segment as its base."
    [0 300])

   (l/with-style {:stroke :magenta}
     (assoc l/line :from [150 0] :to [400 0]))])

(def current-draw
  (spray/subscription
   (let [current @handlers/all-drags]
     (when (and @draw-mode
                (not (:complete? current))
                (handlers/valid-drag? current))
       (handlers/create-shape @draw-mode current)))))

(def canvas
  (spray/subscription
   [(or @shapes [])
    (map #(assoc point :centre %) @control)]))

(def window
  (handlers/window canvas))

(def world
  (spray/subscription
   [(l/translate (l/translate @control-panel [0 100])
                 [0 500])
    (or @current-draw [])
    @window]))

(defn start-game []
  (spray/initialise!
   {:host host
    :effect-roots []
    :render-root world}))

(defn ^:export init []
  (start-game))

(defn on-reload []
  (init))
