(ns euclid.handlers
  (:require [ubik.core :as u]
            [ubik.geometry :as geo]
            [ubik.interactive.core :as spray]
            [ubik.math :as math]))

(def control-tags #{:euclid.game/circle-button :euclid.game/rule-button})

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

(def trans-drag-path (spray/temp-key ::td))

(spray/defhandler drag-follow ::transient-drag
  [db ev]
  {:left-mouse-down (assoc-in db trans-drag-path ev)
   :mouse-move (spray/emit db {:start (get-in db trans-drag-path)
                               :end ev})
   :left-mouse-up (assoc-in db trans-drag-path nil)})

(def drag-p (spray/temp-key ::drag))

(defn maybe-add-shape [db drag]
  (if (contains? control-tags (:draw-mode db))
    (let [s (assoc u/line :to [100 1000])]
      (update db :shapes conj s))
    db))

(spray/defhandler drag-detector ::drag
  [db ev]
  {::transient-drag (assoc-in db drag-p ev)
   :left-mouse-up (let [drag (get-in db drag-p)]
                    (-> db
                        (assoc-in drag-p nil)
                        (maybe-add-shape drag)))})

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

(defn control-click
  "Returns the unique button contained in shape. If shape contains more than one
  button returns nil. If shape contains no buttons, returns nil. If you look
  directly at it, returns nil."
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
