(ns impi.core
  (:refer-clojure :exclude [load update])
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

(defmulti create :pixi/type)

(defmethod create :pixi.type/sprite [{:keys [pixi.sprite/texture]}]
  (js/PIXI.Sprite. (get-texture texture)))

(defmulti update-kv (fn [object k v] k))

(defmethod update-kv :default [object _ _] object)

(defmethod update-kv :pixi.object/position [object _ [x y]]
  (set! (-> object .-position .-x) x)
  (set! (-> object .-position .-y) y)
  object)

(defmethod update-kv :pixi.sprite/anchor [sprite _ [x y]]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y)
  sprite)

(defmethod update-kv :pixi.sprite/texture [sprite _ texture]
  (set! (.-texture sprite) (get-texture texture))
  sprite)

(defn update [object data]
  (reduce-kv update-kv object data))

(defn build [data]
  (update (create data) data))

(defn render [renderer scene]
  (let [render #(.render renderer (build scene))]
    (load ["img/bunny.png"] #(js/requestAnimationFrame render))))
