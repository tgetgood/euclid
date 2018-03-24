(defproject euclid "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [#_[macroexpanse/ubik "0.4.0"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]]

  :plugins [[lein-figwheel "0.5.14"]
            [lein-cljsbuild "1.1.7" :exclusions [org.clojure/clojure]]]

  :source-paths ["src" "dev" "../ubik/src"]

  :main euclid.core

  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["src" "../ubik/src"]

     :figwheel     {:on-jsload "euclid.core/on-reload"}

     :compiler     {:main                 euclid.core
                    :asset-path           "js/compiled/out"
                    :output-to            "resources/public/js/compiled/euclid.js"
                    :output-dir           "resources/public/js/compiled/out"
                    :parallel-build       true
                    :checked-arrays       :warn
                    :source-map-timestamp true
                    :preloads             [devtools.preload]}}

    {:id           "min"
     :source-paths ["src"]
     :compiler     {:output-to      "resources/public/js/compiled/euclid.js"
                    :asset-path     "js/compiled/out"
                    :main           euclid.core
                    :parallel-build true
                    :optimizations  :advanced
                    :pretty-print   false}}]}

  :profiles
  {:dev  {:dependencies  [[binaryage/devtools "0.9.9"]
                          [com.cemerick/piggieback "0.2.2"]
                          [figwheel-sidecar "0.5.14"
                           :exclusions [org.clojure/core.async]]
                          [org.clojure/core.async "0.3.465"]
                          [org.clojure/spec.alpha "0.1.134"]
                          [org.clojure/test.check "0.9.0"]
                          [org.clojure/tools.namespace "0.2.11"]

                          [net.cgrand/macrovich "0.2.1"]
                          [quil "2.6.0" :exclusions [[org.clojure/clojure]]]]
          ;; need to add dev source path here to get user.clj loaded
          :source-paths  ["src" "dev" "../ubik/src"]

          :repl-options  {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
          ;; need to add the compliled assets to the :clean-targets
          :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                            :target-path]}})