(ns example.core
  (:require [impi.core :as impi]))

(defonce renderer
  (impi/renderer [400 300]))

(def state
  (atom
   {:impi/key  :stage
    :pixi/type :pixi.type/container
    :pixi.container/children
    [{:impi/key  :bunny
      :pixi/type :pixi.type/sprite
      :pixi.object/position [200 150]
      :pixi.object/rotation 0.0
      :pixi.sprite/anchor   [0.5 0.5]
      :pixi.sprite/texture  "img/bunny.png"}]}))

(defn animate [state]
  (swap! state update :pixi.object/rotation + 0.01)
  (js/setTimeout #(animate state) 16))

(let [element (.getElementById js/document "app")]
  (impi/mount renderer element)
  (impi/render renderer @state)
  (add-watch state ::render (fn [_ _ _ s] (impi/render renderer s)))
  (animate state))
