(ns euclid.handlers
  (:require [ubik.geometry :as geo]
            [ubik.interactive.core :as spray]
            [ubik.math :as math]))

;;;;; Handlers (transducers)

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
       (if (= :mouse-down (:type n))
         (do
           (reset! state :down)
           (reset! down n)
           acc)
         (let [start @down]
           (reset! down nil)
           (if (compare-and-set! state :down :up)
             (xf acc {:down start :up n})
             acc)))))))

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

(def click-path (spray/temp-key ::clicks))

(spray/defhandler click-detector ::potential-click
  [db ev]
  {:left-mouse-down (assoc-in db click-path ev)
   :left-mouse-up   (let [down    (get-in db click-path)
                          next-db (assoc-in db click-path nil)]
                      (if down
                        (spray/emit next-db {:down down :up ev})
                        next-db))})

(def click-processor
  (spray/handler ::potential-click
                 (comp (filter valid-click?) (map unify-click))
                 ::click))

(def click-registrar
  (spray/handler ::click
                 (fn [db ev]
                   (update db :control-points conj (:location ev)))))

(def handlers
  [click-detector
   click-processor
   click-registrar])
