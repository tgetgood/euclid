(ns euclid.game
  (:require [euclid.handlers :as handlers]
            [ubik.core :as u]
            [ubik.hosts :as hosts]
            [ubik.interactive.core :as spray :include-macros true]
            [ubik.math :as math]))

(defonce host (hosts/default-host {}))

;;;;; Subs

;;;;; UI

(def hud [])

(defn text [t & [c]]
  (cond-> (u/scale (assoc u/text :text t) 2)
    c (u/translate c)))

(def point
  (assoc u/circle :radius 5 :style {:fill :#fdd017
                                    :opacity 0.8
                                    :stroke :none}))

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
  [button-bg
   (-> u/rectangle
       (assoc :width (* 80 (math/sqrt 2)) :height 2)
       (u/style {:fill :black :stroke :none})
       (u/translate [10 9])
       (u/rotate [10 10] 45)
       (u/tag ::rule-button))
   (assoc point :centre [10 10])
   (assoc point :centre [90 90])])

(def selected
  (assoc u/rectangle :width 100 :height 100
         :style {:fill :green :opacity 0.3}))

(def control-panel
  #_(spray/sub-form <<
   [circle-button
    (u/translate rule-button [0 100])
    (condp = (<< :game-draw)
      :circle selected
      :line (assoc selected :corner [0 100])
      [])]))

(def draw-points
  #_(spray/sub-form <<
   (map #(assoc point :centre %) (<< :points))))

(def user-drawing
  #_(spray/sub-form <<
    (into [] (<< :drawings))))

(def problem-1
  [(text
    "Draw an equilateral triangle with the given line segment as its base."
    [0 300])

   (u/with-style {:stroke :magenta}
     (assoc u/line :from [150 0] :to [400 0]))])

(def l1
  [(u/translate control-panel [30 700])
   (u/translate problem-1 [100 300])
   user-drawing
   draw-points])

(defn cp [centre]
  (assoc u/circle
         :style {:opacity 0.5
                 :fill :yellow}
         :radius 10
         :centre centre))

(spray/defsubs
  {shapes  (:shapes @spray/db)
   control (:control-points @spray/db)
   world   [circle-button
              (u/translate rule-button [0 100])
              @shapes
              (map cp @control)]})

(spray/defsub test
  (:a (:b @spray/db)))

;;;;; Init

(defn init []
  (spray/initialise!
   {:host host
    :init-db {:control-points [[250 300] [500 300]]
              :shapes [(u/line [250 300] [500 300])]}
    :handlers handlers/handlers
    :effects {}
    :subs subs
    :root ::world}))
