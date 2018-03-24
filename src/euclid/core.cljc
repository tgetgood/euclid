(ns euclid.core
    (:require [ubik.core :as core]
              [ubik.hosts :as hosts]))

#?(:cljs (enable-console-print!))

(def app-state {:text "Almost Useless"
                :count 3})
(def ex
  [(-> [(-> core/polyline
            (assoc :points [[0 0] [100 100] [300 100] [100 300] [0 0]]
                   :style {:stroke :green
                           :fill   :purple})
            (core/tag ::poly))
        (assoc core/text :text "ASDsadasdasd" :corner [300 100])]
       (core/scale 2)
       (core/rotate 20)
       (core/translate [300 40])
       (core/tag ::translate))
   (assoc core/line :from [800 100] :to [900 100])
   (assoc core/arc :centre [0 0] :radius 200 :style {:stroke :red}
          :from 0 :to (/ 3.14159 4))
   (assoc core/circle :centre [0 0] :radius 200 :style
          {:fill :blue :stroke :none :opacity 0.8})
   (core/with-style {:fill :pink
                     :stroke :blue
                     :opacity 0.5}
     (-> core/annulus
         (assoc :outer-radius 30
                :inner-radius 20
                :style {:fill   :red
                        :stroke :blue})
         (core/scale 10)
         (core/translate [500 500])))

   (core/scale core/circle [4000 500])])

(defonce host (hosts/default-host {}))

(defn ^:export init []
  (core/draw! ex host))

(defn on-reload []
  (init))

#?(:clj (core/draw! ex host))
