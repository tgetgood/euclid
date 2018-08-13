(ns euclid.macros
  (:require [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.interactive.core :as spray :include-macros true]))

(macros/usetime
 (defn wrap-db-handler [f]
   (fn [db e]
     (let [db' (f db e)]
       (when-not (identical? db db')
         (spray/emit db' db'))))))

(macros/deftime

  (defn maybe-var [x]
    (if (symbol? x)
      `(var ~x)
      x))

  (defmacro defreload
    "Creates a new process and permanent (defonced) vars to store its state. On
    figwheel reload, the current state of the process is saved and then restored
    when the new var binding is created."
    [n init rf-map]
    (let [temp (symbol (str (name n) "-state"))
          listener (symbol (str (name n) "-listener"))]
      `(do
         (defonce ~temp (atom ~init))
         (def ~n (spray/stateful-process @~temp @~temp ~rf-map))
         (macros/case :cljs
           (defonce ~listener
             (.addEventListener
              js/document.body "figwheel.before-js-reload"
              (fn [e#]
                (reset! ~temp (ubik.interactive.process/get-state ~n)))))))))

  (defmacro defdb [n init bindings rf-map]
    `(defreload ~n ~init
       ~(into {} (map (fn [[k v]]
                        `[~(maybe-var k) (wrap-db-handler (fn ~bindings ~v))]))
              rf-map)))

  (defmacro defundo [n bindings rf-map]
    `(defreload ~n {}
       ~(into {} (map (fn [[k v]]
                        `[~(maybe-var k) (fn ~bindings ~v)]))
              rf-map))))
