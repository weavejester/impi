(ns impi.core
  (:refer-clojure :exclude [load update])
  (:require cljsjs.pixi))

(defn renderer [[w h]]
  (js/PIXI.autoDetectRenderer w h))

(defn mounted? [renderer element]
  (and (identical? (.-firstChild element) (.-view renderer))
       (= (-> element .-childNodes .-length) 1)))

(defn mount [renderer element]
  (when-not (mounted? renderer element)
    (set! (.-innerHTML element) "")
    (.appendChild element (.-view renderer))))

(def loader (js/PIXI.loaders.Loader.))

(defn get-resource [name]
  (aget (.-resources loader) name))

(defn load [assets callback]
  (let [unloaded-assets (remove get-resource assets)]
    (if (empty? unloaded-assets)
      (callback)
      (doto loader
        (.add (clj->js unloaded-assets))
        (.load (fn [_ _] (callback)))))))

(defmulti create :pixi/type)

(defmethod create :pixi.type/sprite [_]
  (js/PIXI.Sprite.))

(defmulti update-key! (fn [object k v] k))

(defmethod update-key! :default [object _ _] object)

(defmethod update-key! :pixi.object/position [object _ [x y]]
  (set! (-> object .-position .-x) x)
  (set! (-> object .-position .-y) y)
  object)

(defmethod update-key! :pixi.object/rotation [object _ angle]
  (set! (.-rotation object) angle)
  object)

(defmethod update-key! :pixi.sprite/anchor [sprite _ [x y]]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y)
  sprite)

(defmethod update-key! :pixi.sprite/texture [sprite _ texture]
  (set! (.-texture sprite) (.-texture (get-resource texture)))
  sprite)

(defn update! [object old-def new-def]
  (reduce-kv (fn [o k v] (cond-> o (not= v (old-def k)) (update-key! k v)))
             object
             new-def))

(defonce cache (atom {}))

(defn- cache-key [parent-key data]
  (conj parent-key (:impi/key data (:pixi/type data))))

(defn- cache! [object key definition]
  (swap! cache assoc key {:obj object :def definition}))

(defn build
  ([definition]
   (build [] definition))
  ([parent-key definition]
   (let [key (cache-key parent-key definition)]
     (if-let [cached (@cache key)]
       (if (= (:def cached) definition)
         (:obj cached)
         (-> (:obj cached)
             (update! (:def cached) definition)
             (doto (cache! key definition))))
       (-> (create definition)
           (update! {} definition)
           (doto (cache! key definition)))))))

(defn render [renderer scene]
  (let [render #(.render renderer (build scene))]
    (load ["img/bunny.png"] #(js/requestAnimationFrame render))))
