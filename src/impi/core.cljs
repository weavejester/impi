(ns impi.core
  (:require cljsjs.pixi))

(defn renderer [[w h]]
  (js/PIXI.autoDetectRenderer w h))

(defn mount [renderer element]
  (set! (.-innerHTML element) "")
  (.appendChild element (.-view renderer)))

(defn build [scene]
  (let [stage   (js/PIXI.Container.)
        texture (js/PIXI.Texture.fromImage "img/bunny.png")
        sprite  (js/PIXI.Sprite. texture)]
    (set! (-> sprite .-anchor .-x) 0.5)
    (set! (-> sprite .-anchor .-y) 0.5)
    (set! (-> sprite .-position .-x) 200)
    (set! (-> sprite .-position .-y) 150)
    (.addChild stage sprite)
    stage))

(def ^:private loader
  (js/PIXI.loaders.Loader.))

(defn preload [assets callback]
  (run! #(.add loader %) assets)
  (.load loader callback))

(defn render [renderer scene]
  (let [render #(.render renderer (build scene))]
    (preload ["img/bunny.png"] #(js/requestAnimationFrame render))))
