(ns example.core
  (:require [impi.core :as impi]))

(enable-console-print!)

(defonce renderer
  (impi/renderer [400 300]))

(defonce state
  (atom {}))

(reset! state
        {:impi/textures
         {:bunny
          {:pixi.texture/scale-mode :pixi.texture.scale-mode/nearest
           :pixi.texture/source
           {:pixi.asset/type :pixi.asset.type/image
            :pixi.asset/uri  "img/bunny.png"}}}
         :impi/root
         {:impi/key  :stage
          :pixi/type :pixi.type/container
          :pixi.container/children
          [{:impi/key  :bunny1
            :pixi/type :pixi.type/sprite
            :pixi.object/position [200 150]
            :pixi.object/rotation 0.0
            :pixi.sprite/anchor   [0.5 0.5]
            :pixi.sprite/texture  :bunny}
           {:impi/key  :bunny2
            :pixi/type :pixi.type/sprite
            :pixi.object/position [100 100]
            :pixi.object/scale    [10 10]
            :pixi.object/rotation 0.0
            :pixi.sprite/anchor   [0.5 0.5]
            :pixi.sprite/texture  :bunny}
           {:impi/key  :bunny3
            :pixi/type :pixi.type/sprite
            :pixi.object/position [110 100]
            :pixi.object/rotation 0.0
            :pixi.sprite/anchor   [0.5 0.5]
            :pixi.sprite/texture  :bunny}]}})

(defn animate [state]
  (swap! state update-in
         [:impi/root :pixi.container/children 0 :pixi.object/rotation]
         + 0.1)
  (js/setTimeout #(animate state) 16))

(let [element (.getElementById js/document "app")]
  (impi/mount renderer element)
  (impi/render renderer @state)
  (add-watch state ::render (fn [_ _ _ s] (impi/render renderer s))))

(defonce x
  (animate state))
