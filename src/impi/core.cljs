(ns impi.core
  (:require cljsjs.pixi))

(defn renderer [[w h]]
  (js/PIXI.autoDetectRenderer w h))

(defn mount [renderer element]
  (.appendChild element (.-view renderer)))
