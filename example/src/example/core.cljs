(ns example.core
  (:require [impi.core :as impi]))

(defonce renderer
  (impi/renderer [400 300]))

(def data
  {:pixi/type            :pixi.type/sprite
   :pixi.object/position [200 150]
   :pixi.sprite/anchor   [0.5 0.5]
   :pixi.sprite/texture  "img/bunny.png"})

(let [element (.getElementById js/document "app")]
  (impi/mount renderer element)
  (impi/render renderer data))
