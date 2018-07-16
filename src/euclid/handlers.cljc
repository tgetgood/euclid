(ns euclid.handlers
  (:refer-clojure :exclude [+ - *])
  (:require [ubik.core :as u]
            [ubik.lang :refer [+ - *]]
            [ubik.geometry :as geo]
            [ubik.interactive.core :as spray]
            [ubik.math :as math]))

;;;;; Intersections

(defn pairs [s]
  (loop [[h & t] s
         acc []]
    (if (seq t)
      (recur t (concat acc (map (fn [x] [h x]) t)))
      acc)))

(defn circle-circle-intersection [{[x1 y1] :centre r1 :radius}
                                  {[x2 y2] :centre r2 :radius}]
  (let [s  [(- x1 x2) (- y1 y2)]
        d2 (math/dot s s)
        d  (math/sqrt d2)]
    (if (< (+ r1 r2) d)
      []
      (let [[dx dy] (mapv #(/ % d) s)
            cosθ    (/ (- (* r2 r2) (* r1 r1) d2) (* 2 r1 d))
            sinθ    (math/sqrt (- 1 (* cosθ cosθ)))
            i1      [(+ x1 (* r1 (+ (* dx cosθ) (* -1 dy sinθ))))
                     (+ y1 (* r1 (+ (* dx sinθ) (* dy cosθ))))]
            i2      [(+ x1 (* r1 (+ (* dx cosθ) (* dy sinθ))))
                     (+ y1 (* r1 (+ (* -1 dx sinθ) (* dy cosθ))))]]
        [i1 i2]))))

(defn line-line-intersection [{[xp yp] :from [xp2 yp2] :to :as l1}
                              {[xq yq] :from [xq2 yq2] :to}]
  (let [[px py] [(- xp2 xp) (- yp2 yp)]
        [qx qy] [(- xq2 xq) (- yq2 yq)]
        s (/ (- (* px (- yq yp)) (* py (- xq xp))) (- (* qx py) (* qy px)))]
    (when (< 0 s 1)
      (let [i [(+ xq (* qx s)) (+ yq (* s qy))]]
        (when (geo/contains? l1 i)
          [i])))))

(defn closest-point [c l]
  (let [u (geo/unit l)
        t (u/dot (- c p) u)]
    (+ (* t u) (u/origin l))))

(defn line-circle-intersection [{[x1 y1] :from [x2 y2] :to :as l}
                                {[cx cy :as c] :centre r :radius}]
  (let [p (closest-point c l)
        cd (math/dist p c)]
    (if (= cd r)
      [p]
      (when (< cd r)
        (let [d (math/sqrt (- (* r r) (* cd cd)))
              u (math/unit l)
              du (mapv #(* d %) u)]
          (filter #(geo/contains? l %) [(mapv + p du) (mapv - p du)]))))))

(defn intersection-points* [[x y]]
  (cond
    (and (every? #(contains? % :radius) [x y]))
    (circle-circle-intersection x y)

    (and (every? #(contains? % :from) [x y]))
    (line-line-intersection x y)

    (contains? x :from)
    (line-circle-intersection x y)

    :else
    (line-circle-intersection y x)))

(def intersection-points (memoize intersection-points*))

(defn detect-control-points [shapes]
  (into
   #{}
   (concat
    (mapcat (fn [s] (when (u/segment? s) (u/endpoints s))) shapes)
    (mapcat intersection-points (pairs shapes)))))

;;;;; Handlers

(def control-tags #{:euclid.core/circle-button :euclid.core/rule-button})

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

(defn valid-drag?
  "Returns true iff the event passed in qualifies as a valid drag."
  [{{[x1 y1] :location} :start {[x2 y2] :location} :end}]
  (< 20 (+ (math/abs (- x1 x2)) (math/abs (- y1 y2)))))

(def drag-start (spray/temp-key ::drag-start))

(spray/defhandler drag-follow ::transient-drag
  [db ev]
  {:left-mouse-down (assoc-in db drag-start ev)
   :mouse-move      (if-let [start (get-in db drag-start)]
                      (assoc db :current-drag {:start start :end ev})

                      db)
   :mouse-out       (-> db
                        (assoc :current-drag nil)
                        (assoc-in drag-start nil))
   :left-mouse-up   (let [next-db (-> db
                                      (assoc :current-drag nil)
                                      (assoc-in drag-start nil))
                          drag    (:current-drag db)]
                      (if drag
                        (spray/emit next-db drag)
                        next-db))})

(def drag-p (spray/temp-key ::drag))

(defn create-shape [mode {{l1 :location} :start {l2 :location} :end}]
  (case mode
    :euclid.core/rule-button (assoc u/line :from l1 :to l2)
    :euclid.core/circle-button (assoc u/circle
                                      :centre l1
                                      :radius (math/norm (map - l1 l2)))
    ;; REVIEW: Don't error on bad shape, just return no shape
    []))

(defn maybe-add-shape [{:keys [draw-mode] :as db} drag]
  (if (contains? control-tags draw-mode)
    (do
      ;; FIXME: Side effects in user code!!!
      (spray/checkpoint!)
      (update db :shapes conj (create-shape draw-mode drag)))
    db))

(spray/defhandler snap-to-controls ::snap-drag
  [db drag]
  {::drag (let [controls (detect-control-points (:shapes db))
                start (:location (:start drag))
                end (:location (:end drag))
                [sd pstart] (first
                             (sort (map (fn [c] [(math/dist start c) c]) controls)))
                [ed pend] (first
                           (sort (map (fn [c] [(math/dist end c) c]) controls)))
                d (cond-> drag
                    (< sd 10) (assoc-in [:start :location] pstart)
                    (< ed 10) (assoc-in [:end :location] pend))]
            (spray/emit db d))})

(def drag-filter
  (spray/handler ::transient-drag (filter valid-drag?) ::drag))

(def drag-detector
  (spray/handler ::snap-drag
    (fn [db ev]
      (maybe-add-shape db ev))))

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

(defn emit-key [k {:keys [alt control]}]
  (let [c (if control "C-" "")
        a (if alt "M-" "")]
    (str c a k)))

(def key-path (spray/temp-key ::keypress))

(spray/defhandler keypress ::keypress
  [db ev]
  {:key-down (case (:key ev)
               "Control" (update-in db key-path assoc :control true)
               "Shift"   db
               "Alt"     (update-in db key-path assoc :alt true)
               db)
   :key-up   (let [k (:key ev)]
               (case k
                 "Control" (update-in db key-path assoc :control false)
                 "Shift"   db
                 "Alt"     (update-in db key-path assoc :alt false)
                 (spray/emit db {:time (:time ev)
                                 :key (emit-key k (get-in db key-path))})))})

(def undo
  (spray/handler ::keypress (filter #(= "C-z" (:key %))) ::undo))

(def redo
  (spray/handler ::keypress (filter #(= "C-r" (:key %))) ::redo))

(def undo-it
  (spray/handler ::undo
    (fn [db _]
      (spray/undo!)
      db)))

(def redo-it
  (spray/handler ::redo
    (fn [db _]
      (spray/redo!)
      db)))

(def handlers
  [click-detector
   click-processor
   click-registrar
   keypress
   undo
   undo-it
   redo
   redo-it
   drag-follow
   drag-filter
   snap-to-controls
   drag-detector])
