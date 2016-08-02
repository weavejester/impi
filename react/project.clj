(defproject impi/react "0.0.1"
  :description "Library for using Impi with React"
  :url "https://github.com/weavejester/impi"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [cljsjs/react "15.1.0-0"]
                 [impi "0.0.1"]]
  :plugins [[lein-cljsbuild "1.1.3"]]
  :cljsbuild
  {:builds
   {:main
    {:source-paths ["src"]
     :compiler     {:output-to "target/main.js"}}}})
