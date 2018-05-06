(ns euclid.game
  (:require [ubik.core :as u]
            [ubik.geometry :as geo]
            [ubik.interactive.core :as spray :include-macros true]
            [ubik.math :as math]))

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
  (spray/sub-form <<
   [circle-button
    (u/translate rule-button [0 100])
    (condp = (<< :game-draw)
      :circle selected
      :line (assoc selected :corner [0 100])
      [])]))

(def draw-points
  (spray/sub-form <<
   (map #(assoc point :centre %) (<< :points))))

(def user-drawing
  (spray/sub-form <<
    (into [] (<< :drawings))))

(def problem-1
  [(text
    "Draw an equilateral triangle with the given line segment as its base."
    [0 300])

   (u/with-style {:stroke :magenta}
     (assoc u/line :from [150 0] :to [400 0]))])

(def initial-points
  [[250 300] [500 300]])

;;;;; FIXME: Duplicates

(defn clicked-on?
  "Returns true if the shape with tag contains location."
  [tag location]
  (when-let [shape (spray/find-by-tag tag)]
    (geo/contains? shape location)))

(defn valid-click?
  "Returns true if the given down and up event are sufficiently close in space
  and time."
  [{{t1 :time [x1 y1] :location} :down
    {t2 :time [x2 y2] :location} :up}]
  (and (< (- t2 t1) 2000)
       (< (+ (math/abs (- x2 x1)) (math/abs (- y2 y1))) 100)))

(defn unify-click
  "Returns a click event corresponding to a pair of (mouse-down, mouse-up)
  events. Click time is mouse-up time and location is the midpoint."
  [{{t1 :time [x1 y1] :location} :down
                    {t2 :time [x2 y2] :location} :up}]
  {:time t2 :location [(quot (+ x1 x2) 2) (quot (+ y1 y2) 2)]})

(defn click-tx
  "Stateful transducer which takes a sequence of mouse events and emits a
  sequence of (mouse-down, mouse-up) pairs that could be clicks."
  [xf]
  (let [state (atom nil)
        down (atom nil)]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (if (:down? n)
         (do
           (reset! state :down)
           (reset! down n)
           acc)
         (let [start @down]
           (reset! down nil)
           (if (compare-and-set! state :down :up)
             (xf acc {:down start :up n})
             acc)))))))

#_(spray/stx {}
    (fn [{:keys [down]} event]
      (if (:down? event)
        {:state {:down event}}
        (when down
          {:state {}
           :emit  {:down down :up event}}))))

(defn emit [x])

(defn click-stateful [xf]
  (let [state (volatile! nil)]
    (fn [input]
      (if (:down? input)
        (vreset! state input)
        (when-let [down @state]
          (vreset! state nil)
          (xf {:down down :up input}))))))

(defn selection-at [click]
  (let [loc (:location click)]
    (cond
      (clicked-on? ::circle-button loc) :circle
      (clicked-on? ::rule-button loc)   :line
      :else                             nil)))

(defn drag-tx [xf]
  (let [start (volatile! nil)]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (if (:down? n)
         (do
           (vreset! start n)
           acc)
         (let [s @start]
           (vreset! start nil)
           (if (valid-click? {:down s :up n})
             acc
             (xf acc {:start (:location s)
                      :end (:location n)
                      :time (:time s)
                      :duration [(:time s) (:time n)]}))))))))

(defn drawings-tx [xf]
  (let [mode (volatile! nil)]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (if (:mode n)
         (do
           (vreset! mode (:mode n))
           acc)
         (if-let [m @mode]
           acc
           acc))))))

(defn last-rx
  ([] nil)
  ([a] a)
  ([_ x] x))

(defn count-rx
  ([] 0)
  ([a] a)
  ([a _] (inc a)))

(spray/defsubs subscriptions <<
  {:mouse-events (:mouse-events (<< :db))

   :clicks       (eduction (comp click-tx
                                 (filter valid-click?)
                                 (map unify-click))
                           (<< :mouse-events))

   :last-click   (reduce last-rx (<< :clicks))


   :game-draw    (selection-at (<< :last-click))

   :draw-modes   (map (fn [x] {:mode (selection-at x) :time (:time x)})
                      (<< :clicks))

   :drawings     (eduction drawings-tx (sort-by :time (concat (<< :draw-modes)
                                                              (<< :drags))))

   :points       (:points (<< :db))

   :drags        (eduction drag-tx (reverse (<< :mouse-event)))})

(def click-tx2
  (spray/simple-tx {}
    (fn [{:keys [down]} event]
      (if (:down? event)
        {:state {:down event}}
        (when down
          {:state {}
           :emit-n  [{}  {:down down :up event} 4]})))))

(def clicks
  (comp click-tx2
        (filter valid-click?)
        (map unify-click)))

(defn last-click [] @clicks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What does a signal need to do?
;;
;; You need to be able to deref the current value.
;; You need to be able to follow it so that you can update when it does.
;; It needs to follow something else and update when it does.
;;
;; There will at some point be a signal which simply emits messages. A whitehole
;; so to speak. This would be something like game time ticks, or the mouse event
;; stream from the browser.
;;
;; We also want to be able to interleave existing signals to combine them.
;;
;; Basically I'm talking about event streams and transforms on them, but only
;; the last value ever sees the light of day. Does that make any sense?
;;
;; Okay, so the first type is like a transducer. It's defined (conceptually) by
;; a transducer on an input.
;; But what does it mean to transduce? No the real question is what does it mean
;; to reduce over signals? It means nothing. Reduction isn't well defined
;; because reduction is global in time. It requires all values of a thing over
;; time, does it?

(def combine)

(def signal)

(spray/defsubs ideal <<
  {:mouse-events (combine (<< :mouse-down) (<< :mouse-up))

   :clicks       (signal (comp click-tx
                               (filter valid-click?)
                               (map unify-click))
                         (<< :mouse-events))

   ;; :last-click is just the current value of :clicks

   :game-draw    (selection-at (<< :clicks))

   :drags        (signal drag-tx (<< :mouse-events))

   :drawings     (signal drawings-tx (<< :drags) (<< :game-draw))
   })

(def l1
  [(u/translate control-panel [30 700])
   (u/translate problem-1 [100 300])
   user-drawing
   draw-points])
