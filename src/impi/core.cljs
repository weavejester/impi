(ns impi.core
  (:refer-clojure :exclude [load])
  (:require cljsjs.pixi))

(defn renderer [[w h]]
  (js/PIXI.autoDetectRenderer w h))

(defn mount [renderer element]
  (set! (.-innerHTML element) "")
  (.appendChild element (.-view renderer)))

(def ^:private loader
  (js/PIXI.loaders.Loader.))

(defn load [assets callback]
  (run! #(.add loader %) assets)
  (.load loader callback))

(defn get-texture [asset]
  (.-texture (aget (.-resources loader) asset)))

(defn build [scene]
  (let [stage   (js/PIXI.Container.)
        sprite  (js/PIXI.Sprite. (get-texture "img/bunny.png"))]
    (set! (-> sprite .-anchor .-x) 0.5)
    (set! (-> sprite .-anchor .-y) 0.5)
    (set! (-> sprite .-position .-x) 200)
    (set! (-> sprite .-position .-y) 150)
    (.addChild stage sprite)
    stage))

(defn render [renderer scene]
  (let [render #(.render renderer (build scene))]
    (load ["img/bunny.png"] #(js/requestAnimationFrame render))))
