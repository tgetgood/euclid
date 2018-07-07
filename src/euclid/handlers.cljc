(ns euclid.handlers
  (:require [ubik.core :as u]
            [ubik.geometry :as geo]
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
    {t2 :time [x2 y2] :location :as up} :up}]
  (assoc (dissoc up :type)
         :time t2
         :location [(quot (+ x1 x2) 2) (quot (+ y1 y2) 2)]))

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

(def trans-drag-path (spray/temp-key ::td))

(spray/defhandler drag-follow ::transient-drag
  [db ev]
  {:left-mouse-down (assoc-in db trans-drag-path ev)
   :mouse-move (spray/emit db {:start (get-in db trans-drag-path)
                               :end ev})
   :left-mouse-up (assoc-in db trans-drag-path nil)})

(def drag-p (spray/temp-key ::drag))

(defn maybe-add-shape [db drag]
  (if (:draw-mode db)
    (let [s (assoc u/line :to [100 1000])]
      (update db :shapes conj s)
      db)))

(spray/defhandler drag-detector ::drag
  [db ev]
  {::transient-drag (assoc-in db drag-p ev)
   :left-mouse-up (let [drag (get-in db drag-p)]
                    (println drag)
                    (-> db
                        (assoc-in drag-p nil)
                        (maybe-add-shape drag)))})

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

(def control-tags #{:euclid.game/circle-button :euclid.game/rule-button})

(defn control-click
  "Returns the unique button contained in shape. If shape contains more than one
  button returns nil. If shape contains no buttons, returns nil. If you're rude,
  returns nil."
  [shape]
  (let [tags (filter (partial contains? control-tags) (u/get-all-tags shape))]
    (when (= 1 (count tags))
      (first tags))))

(def click-registrar
  (spray/handler ::click
                 (fn [db ev]
                   (let [bs (geo/effected-branches
                             (:location ev)
                             (:ubik.interactive.events/world ev))
                         clicked-tag (control-click bs)]
                     (if clicked-tag
                       (assoc db :draw-mode clicked-tag)
                       db)))))

(def handlers
  [click-detector
   click-processor
   click-registrar
   drag-follow
   drag-detector])
