(ns euclid.handlers
  (:refer-clojure :exclude [+ - *])
  (:require [ubik.core :as u]
            [ubik.geometry :as geo]
            [ubik.interactive.core :as spray :include-macros true]
            [ubik.interactive.process :as process :include-macros true]
            [ubik.lang :as lang :refer [* + -]]
            [ubik.math :as math]
            [ubik.math :as math]))

;;;;; Intersections

(defn pairs [s]
  (loop [[h & t] s
         acc []]
    (if (seq t)
      (recur t (concat acc (map (fn [x] [h x]) t)))
      acc)))

(defn circle-circle-intersection [{c1 :centre r1 :radius}
                                  {c2 :centre r2 :radius}]
  (let [s  (- c1 c2)
        l2 (u/dot s s)
        l  (math/sqrt l2)]
    (if (< (+ r1 r2) l)
      []
      (let [d               (* (/ 1 l) s)
            cosθ            (/ (- (* r2 r2) (* r1 r1) l2) (* 2 r1 l))
            sinθ            (math/sqrt (- 1 (* cosθ cosθ)))
            [dxsinθ dysinθ] (* sinθ d)
            [dxcosθ dycosθ] (* cosθ d)
            i1              (+ c1 (* r1 [(+ dxcosθ (* -1 dysinθ))
                                         (+ dxsinθ dycosθ)]))
            i2              (+ c1 (* r1 [(+ dxcosθ dysinθ)
                                         (+ (* -1 dxsinθ) dycosθ)]))]
        [i1 i2]))))

(defn line-line-intersection [l m]
  (let [[px py] (u/base-vector l)
        [qx qy :as q] (u/base-vector m)
        [xp yp] (u/origin l)
        [xq yq :as q0] (u/origin m)
        ;; FIXME: The math to get this isn't terribly nice, but this equation is
        ;; just awful...
        s (/ (- (* px (- yq yp)) (* py (- xq xp))) (- (* qx py) (* qy px)))]
    (when (< 0 s 1)
      (let [intersect (+ q0 (* s q))]
        (when (geo/contains? l intersect)
          [intersect])))))

(defn closest-point [c l]
  (let [u (u/unit l)
        t (u/dot (- c (u/origin l)) u)]
    (+ (* t u) (u/origin l))))

(defn line-circle-intersection [{[x1 y1] :from [x2 y2] :to :as l}
                                {[cx cy :as c] :centre r :radius}]
  (let [p (closest-point c l)
        cd (math/dist p c)]
    (if (= cd r)
      [p]
      (when (< cd r)
        (let [d (math/sqrt (- (* r r) (* cd cd)))
              u (u/unit l)
              du (* d u)]
          (filter #(geo/contains? l %) [(+ p du) (- p du)]))))))

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
  (when shapes
    (into
     #{}
     (concat
      (mapcat (fn [s] (when (u/segment? s) (u/endpoints s))) shapes)
      (mapcat intersection-points (pairs shapes))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def control-tags #{:euclid.core/circle-button :euclid.core/rule-button
                    :euclid.core/pan})


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
  [{{start :location} :start {end :location} :end}]
  (and start end (< 20 (lang/length (- end start)))))

(spray/defprocess all-drags
  [{:keys [start] :as state} ev]
  {:mouse-down {:start ev}
   :mouse-move (when start
                 (spray/emit state {:complete? false :start start :end ev}))
   :mouse-out  (when start
                 (spray/emit {} ::dropped))
   :mouse-up   (when start
                 (spray/emit {} {:complete? true :start start :end ev}))})

(def drag
  (spray/process {all-drags (comp (filter valid-drag?) (filter :complete?))}))

(spray/defprocess snap-drag
  [db ev]
  {drag
   (let [controls (detect-control-points (:shapes db))]
     (if (empty? controls)
       (spray/emit db ev)
       (let [start       (:location (:start ev))
             end         (:location (:end ev))
             [sd pstart] (first
                          (sort (map (fn [c] [(math/dist start c) c]) controls)))
             [ed pend]   (first
                          (sort (map (fn [c] [(math/dist end c) c]) controls)))
             d           (cond-> ev
                           (< sd 20) (assoc-in [:start :location] pstart)
                           (< ed 20) (assoc-in [:end :location] pend))]
         (spray/emit db d))))})

(defn create-shape [mode {{l1 :location} :start {l2 :location} :end}]
  (case mode
    :euclid.core/rule-button (assoc u/line :from l1 :to l2)
    :euclid.core/circle-button (assoc u/circle
                                      :centre l1
                                      :radius (math/norm (map - l1 l2)))
    ;; REVIEW: Don't error on bad shape, just return no shape
    []))

(defn maybe-add-shape [{:keys [draw-mode] :as db} drag]
  (if (contains? (disj control-tags :euclid.core/pan) draw-mode)
    (update db :shapes conj (create-shape draw-mode drag))
    db))

(spray/defprocess potential-clicks
  [{:keys [down]} ev]
  {:mouse-down {:down ev}
   :mouse-up   (when down
                 (spray/emit {} {:down down :up ev}))})

(def click-processor
  (spray/process
   {potential-clicks (comp (filter valid-click?) (map unify-click))}))

(defn control-click
  "Returns the unique button contained in shape. If shape contains more than one
  button returns nil. If shape contains no buttons, returns nil. If you look
  directly at it, returns nil."
  [shape]
  (let [tags (filter (partial contains? control-tags) (u/get-all-tags shape))]
    (when (= 1 (count tags))
      (first tags))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; App DB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare undo-manager)

(def init-db
  {:shapes []})

(defn wrap-db-handler [f]
   (fn [db e]
     (let [db' (f db e)]
       (when-not (or (nil? db') (identical? db db'))
         (spray/emit db' db')))))


(spray/defprocess app-db
  "Single source of truth for the UI. Acts like the reduced app-db in
  re-frame."
  {:init-state init-db :reloaded? true :wrap-body wrap-db-handler}
  [db ev]
  {undo-manager ev
   snap-drag       (maybe-add-shape db ev)
   click-processor (let [bs          (geo/effected-branches
                                      (:location ev)
                                      (::spray/render-tree ev))
                         clicked-tag (control-click bs)]
                     (if clicked-tag
                       (assoc db :draw-mode clicked-tag)
                       db))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn emit-key [k {:keys [alt control]}]
  (let [c (if control "C-" "")
        a (if alt "M-" "")]
    (str c a k)))

(spray/defprocess keypress
  [state ev]
  {:key-down (case (:key ev)
               "Control" (assoc state :control true)
               "Alt"     (assoc state :alt true)
               nil)
   :key-up   (let [k (:key ev)]
               (case k
                 "Control" (assoc state :control false)
                 "Alt"     (assoc state  :alt false)
                 (spray/emit state
                             {:time (:time ev)
                              :key  (emit-key k state)})))})

(def undo
  (spray/process {keypress (filter #(= "C-z" (:key %)))}))

(def redo
  (spray/process {keypress (filter #(= "C-r" (:key %)))}))

(defn add-to-queue [{:keys [queue index max-revisions] :as state} snapshot]
  (cond
    (= index max-revisions)
    (assoc state :queue (conj (into [] (rest queue)) snapshot))

    (not= (inc index) (count queue))
    (-> state
        (update :index inc)
        (assoc :queue (conj (into [] (take (inc index) queue)) snapshot)))

    :else
    (-> state
        (update :index inc)
        (update :queue conj snapshot))))


(defn- undo* [save-fn restore-fn {:keys [undo redo] :as state} db]
  (if (empty? undo)
    state
    (let [snapshot (save-fn db)
          s' (if (= (peek undo) snapshot)
               (assoc state :undo (pop undo) :redo (conj redo snapshot))
               (assoc state :redo (conj redo snapshot)))]
      (spray/emit s' (restore-fn (peek (:undo s')) db)))))

(defn- redo* [restore-fn {:keys [redo undo] :as state} db]
  (when-let [next (peek redo)]
    (spray/emit (assoc state :undo (conj undo next) :redo (pop redo))
                (restore-fn next db))))

(defn restrict [n s]
  (if (< (count s) n)
    s
    (into (empty s) (take n s))))

(defn push-state [save-fn {:keys [undo redo max] :as state} db]
  (let [snapshot (save-fn db)]
    (when-not (identical? snapshot (peek undo))
      (assoc state :undo (restrict max (conj undo snapshot)) :redo '()))))

(defn- restore-fn
  [snapshot db]
  (assoc db :shapes snapshot))

(defn- save-fn
  [db]
  (:shapes db))

(spray/defprocess undo-manager
  {:init-state {:max 50 :undo '() :redo '()} :reloaded? true}
  [state ev]
  {app-db (push-state save-fn state @app-db)
   undo   (undo* save-fn restore-fn state @app-db)
   redo   (redo* restore-fn state @app-db)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Infinite Canvas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn normalise-zoom [dz]
  (let [scale 100]
    (math/exp (/ (- dz) scale))))

(defn zoom-c [dz origin centre]
  (let [dz (normalise-zoom dz)]
    (+ (* dz origin) (* (- 1 dz) centre))))

(defn update-zoom [{:keys [zoom offset] :as w} centre dz]
  {:zoom (max -8000 (min 8000 (+ zoom dz)))
   :offset (zoom-c dz offset centre)})

(defn update-offset [w delta]
  (update w :offset - delta))

(spray/defprocess window-drag
  [start ev]
  {all-drags (when (= :euclid.core/pan (:draw-mode @app-db))
               (if (or (keyword? ev) (:complete? ev))
                 false
                 (let [start (or start (:location (:start ev)))
                       end   (:location (:end ev))]
                   (spray/emit end (- start end)))))})

(spray/defprocess windower
  {:init-state {:zoom 1 :offset [0 0]} :reloaded? true :wrap-body wrap-db-handler}
  [state ev]
  {:wheel      (let [{:keys [location dy]} ev]
                 (update-zoom state location dy))
   window-drag (update-offset state ev)})

(defn window [shape]
  (spray/subscription
   (let [{:keys [zoom offset]} @windower]
     (-> @shape
         (u/scale (normalise-zoom zoom))
         (u/translate offset)))))
