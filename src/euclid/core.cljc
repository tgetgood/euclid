(ns euclid.core
  (:require [euclid.game :as game]))

#?(:cljs (enable-console-print!))

(defn ^:export init []
  (game/init))

(defn on-reload []
  (init))
