(ns euclid.window)

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
  {ctrl? nil
   all-drags (when (or (= :euclid.core/pan (:draw-mode @app-db))
                       @ctrl?)
               (if (or (keyword? ev) (:complete? ev))
                 false
                 (let [start (or start (:location (:start ev)))
                       end   (:location (:end ev))]
                   (when (and start end)
                     (spray/emit end (- start end))))))})

(spray/defprocess window
  {:init-state {:zoom 1 :offset [0 0]} :reloaded? true :wrap-body wrap-db-handler}
  [state ev]
  {:wheel      (let [{:keys [location dy]} ev]
                 (update-zoom state location dy))
   window-drag (update-offset state ev)})

(defn txp [loc {:keys [zoom offset]}]
  (let [z (/ 1 (normalise-zoom zoom))]
    (math/apply-atx (math/comp-atx
                     (u/scaling [z z])
                     (u/translation (- offset)))
                    loc)))

(def windowed-drag
  (spray/transducer
   {:init-state @window}
   [{:keys [zoom offset] :as win} ev]
   {:window ev
    :drag   (when (and zoom offset)
              (spray/emit win (-> ev
                                  (update-in [:start :location] txp win)
                                  (update-in [:end :location] txp win))))}))

(defn window [shape]
  (spray/subscription
   (let [{:keys [zoom offset]} @windower]
     (-> @shape
         (u/scale (normalise-zoom zoom))
         (u/translate offset)))))
