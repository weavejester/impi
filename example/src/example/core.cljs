(ns example.core
  (:require [impi.core :as impi]))

(enable-console-print!)

(defonce renderer
  (impi/renderer [400 300] {:background-color 0xbbbbbb}))

(defonce state
  (atom {}))

(reset! state
        {:impi/key  :stage
         :pixi/type :pixi.type/container
         :pixi.container/children
         [{:impi/key  :bunny1
           :pixi/type :pixi.type/sprite
           :pixi.object/position [200 150]
           :pixi.object/rotation 0.0
           :pixi.sprite/anchor   [0.5 0.5]
           :pixi.sprite/texture
           {:pixi.texture/scale-mode :pixi.texture.scale-mode/linear
            :pixi.texture/source     "img/bunny.png"}}
          {:impi/key  :bunny2
           :pixi/type :pixi.type/sprite
           :pixi.object/position [100 100]
           :pixi.object/scale    [5 5]
           :pixi.object/rotation 0.0
           :pixi.sprite/anchor   [0.5 0.5]
           :pixi.sprite/texture
           {:pixi.texture/scale-mode :pixi.texture.scale-mode/nearest
            :pixi.texture/source     "img/bunny.png"}}
          {:impi/key  :bunny3
           :pixi/type :pixi.type/sprite
           :pixi.object/position [150 140]
           :pixi.object/rotation 0.0
           :pixi.sprite/anchor   [0.5 0.5]
           :pixi.sprite/texture
           {:pixi.texture/scale-mode :pixi.texture.scale-mode/nearest
            :pixi.texture/source     "img/bunny.png"}}]})

(defn animate [state]
  (swap! state update-in [:pixi.container/children 0 :pixi.object/rotation] + 0.1)
  (js/setTimeout #(animate state) 16))

(let [element (.getElementById js/document "app")]
  (impi/mount renderer element)
  (impi/render renderer @state)
  (add-watch state ::render (fn [_ _ _ s] (impi/render renderer s))))

(defonce x
  (animate state))
