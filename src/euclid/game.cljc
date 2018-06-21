(ns euclid.game
  (:require [ubik.core :as u]
            [ubik.geometry :as geo]
            [ubik.hosts :as hosts]
            [ubik.interactive.core :as spray :include-macros true]
            [ubik.interactive.events :as events]
            [ubik.interactive.signal :as signal :refer [signal]]
            [ubik.math :as math]))

(defonce host (hosts/default-host {}))

(def events (events/event-signal host))

(defn interleave [& sigs]
  sigs)

(defn tx [xform & sigs]
  (transduce xform signal/last-rf (interleave sigs)))



(defn transducer [m]
  m)

(def up (tx (filter #(= :left-mouse-up (:type %))) events))
(def down (tx (filter #(= :left-mouse-down (:type %))) events))
(def move (tx (filter #(= :mouse-move (:type %))) events))

(def ctx2
  "Demo signal combinator fn. Each signal runs in a single thread. Communication
  between threads is accomplished via messages emitted by signals."
  (transducer
   {:init-state {:down? nil}
    :finalise (fn [state] nil)
    :inputs     {down (fn [_ ev]
                        {:state {:down? ev}
                         :emit  nil})
                 up   (fn [{:keys [down?]} ev]
                        (when down?
                          {:state nil
                           :emit  [down? ev]}))}}))
(def drag-state
  (transducer
   {:init-state {:start nil
                 :current nil}
    :inputs {down (fn [_ ev]
                    {:state {:start ev}})
             move (fn [state ev]
                    (when (:start state)
                      {:emit (assoc state :current ev)}))
             up (fn [state ev]
                  (when (:start state)
                    {:emit (assoc state :end ev)
                     :state {}}))}}))

(def clicks (tx ctx2 up down))
(def drag (tx drag-state up down move))
(def drags (transduce (filter :end) conj drag))

(declare world)

(def draw-state (tx (map thing-clicked) world clicks))

(def shapes (signal (signal shapes-tx drags )))

(defn signal-shape [x])

(def hud [])

(def world
  (signal-shape
   [hud
    @shapes]))

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

(def initial-points
  [[250 300] [500 300]])

;;;;; FIXME: Duplicates

(defn clicked-on?
  "Returns true if the shape with tag contains location."
  [tag location]
  (when-let [shape [] #_(spray/find-by-tag tag)]
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

(def l1
  [(u/translate control-panel [30 700])
   (u/translate problem-1 [100 300])
   user-drawing
   draw-points])

(defn mode-clicked [world click]
  ::circle)

(def handlers
  [{:key ::state
    :stream-keys #{::clicks}
    :rf (fn [db click]
          (assoc db ::state (mode-clicked l1 click)))}
   {:key ::drags
    :stream-keys #{:mouse-down :mouse-up :mouse-move}
    :xform drag-tx}
   {:key ::clicks
    :stream-keys #{:mouse-down :mouse-up}
    :xform (comp click-tx (filter valid-click?) (map unify-click))}
   {:key ::shapes
    :stream-keys #{::drags}
    :rf (fn [db drag]
          (if-let [t (::state db)]
            (update db ::shapes conj (build-shape t drag))
            db))}])

(defn init []
  (spray/initialise!
   {:host host
    :init-db {}
    :handlers handlers
    :subs {}
    :root l1}))
