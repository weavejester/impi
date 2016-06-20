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

(defn loaded? [asset]
  (boolean (aget (.-resources loader) asset)))

(defn load [assets callback]
  (let [unloaded-assets (remove loaded? assets)]
    (if (empty? unloaded-assets)
      (callback)
      (doto loader
        (.add (clj->js unloaded-assets))
        (.load (fn [_ _] (callback)))))))

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

(defn update [object old-def new-def]
  (reduce-kv (fn [o k v] (cond-> o (not= v (old-def k)) (update-kv k v)))
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
             (update (:def cached) definition)
             (doto (cache! key definition))))
       (-> (create definition)
           (update {} definition)
           (doto (cache! key definition)))))))

(defn render [renderer scene]
  (let [render #(.render renderer (build scene))]
    (load ["img/bunny.png"] #(js/requestAnimationFrame render))))
