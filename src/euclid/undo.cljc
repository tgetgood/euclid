(ns euclid.undo)


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
  {app-db (push-state save-fn state ev)
   undo   (undo* save-fn restore-fn state @app-db)
   redo   (redo* restore-fn state @app-db)})
