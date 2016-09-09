(defproject impi/example "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [impi "0.0.6"]
                 [binaryage/devtools "0.6.1"]]
  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-figwheel "0.5.4-3"]]
  :cljsbuild
  {:builds
   {:main
    {:figwheel     true
     :source-paths ["src" "../src"]
     :compiler     {:main example.core
                    :asset-path "js/out"
                    :output-to "resources/public/js/main.js"
                    :output-dir "resources/public/js/out"
                    :optimizations :none}}}}
  :figwheel
  {:http-server-root "public"
   :server-port      3001
   :css-dirs         ["resources/public/css"]})
