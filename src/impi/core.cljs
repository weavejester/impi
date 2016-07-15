(ns impi.core
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

(defn- update-count [child f]
  (set! (.-impiCount child) (f (.-impiCount child))))

(defn- replace-child [container child i]
  (let [old-child (aget (.-children container) i)]
    (when-not (identical? child old-child)
      (aset (.-children container) i child)
      (update-count old-child (fnil dec 1))
      (update-count child (fnil inc 0))
      old-child)))

(defn- append-child [container child]
  (.push (.-children container) child)
  (update-count child (fnil inc 0)))

(defn- overwrite-children [container children]
  (let [length (-> container .-children .-length)]
    (loop [i 0, children children, replaced ()]
      (if (seq children)
        (let [child (first children)]
          (if (< i length)
            (if-let [old-child (replace-child container child i)]
              (recur (inc i) (rest children) (cons old-child replaced))
              (recur (inc i) (rest children) replaced))
            (do (append-child container child)
                (recur (inc i) (rest children) replaced))))
        replaced))))

(defn- trim-children [container index]
  (let [children (.-children container)]
    (if (< index (.-length children))
      (let [removed (.slice children index)]
        (.splice children index)
        removed))))

(defn- set-parent [child parent]
  (set! (.-parent child) parent)
  (js-delete child "impiCount"))

(defn- clear-parent [child]
  (when (zero? (.-impiCount child))
    (set-parent child nil)))

(defn- replace-children [container children]
  (let [length   (-> container .-children .-length)
        replaced (overwrite-children container children)
        removed  (trim-children container (count children))
        changed? (or (seq replaced) (not= length (-> container .-children .-length)))]
    (when changed?
      (run! clear-parent replaced)
      (run! clear-parent removed)
      (run! #(set-parent % container) (.-children container)))))

(defn- image [src]
  (let [image (js/Image.)]
    (set! (.-src image) src)
    image))

(def ^:private scale-modes
  {:pixi.texture.scale-mode/linear  js/PIXI.SCALE_MODES.LINEAR
   :pixi.texture.scale-mode/nearest js/PIXI.SCALE_MODES.NEAREST})

(def cache (atom {}))

(defn- cached-texture [key]
  (:obj (@cache [:impi/textures key])))

(declare build!)

(defmulti create :pixi/type)

(defmethod create :pixi.type/sprite [_]
  (js/PIXI.Sprite.))

(defmethod create :pixi.type/container [_]
  (js/PIXI.Container.))

(defmethod create :pixi.type/texture [texture]
  (let [source (-> texture :pixi.texture/source image)
        mode   (-> texture :pixi.texture/scale-mode scale-modes)]
    (js/PIXI.Texture. (js/PIXI.BaseTexture. source mode))))

(defmulti update-key! (fn [object cache-key key value] key))

(defmethod update-key! :default [object _ _ _] object)

(defmethod update-key! :pixi.object/position [object _ _ [x y]]
  (set! (-> object .-position .-x) x)
  (set! (-> object .-position .-y) y)
  object)

(defmethod update-key! :pixi.object/rotation [object _ _ angle]
  (set! (.-rotation object) angle)
  object)

(defmethod update-key! :pixi.object/scale [object _ _ [x y]]
  (set! (-> object .-scale .-x) x)
  (set! (-> object .-scale .-y) y)
  object)

(defmethod update-key! :pixi.container/children [container cache-key key children]
  (replace-children container (map #(build! % (conj cache-key key)) children))
  container)

(defmethod update-key! :pixi.sprite/anchor [sprite _ _ [x y]]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y)
  sprite)

(defmethod update-key! :pixi.sprite/texture [sprite cache-key _ texture]
  (set! (.-texture sprite) (build! texture (conj cache-key key)))
  sprite)

(defmethod update-key! :pixi.texture/scale-mode [texture _ _ mode]
  (set! (.-scaleMode texture) (scale-modes mode))
  texture)

(defn- update! [cached key definition]
  {:def definition
   :obj (let [old-def (:def cached)]
          (reduce-kv
           (fn [o k v] (if (= v (old-def k)) o (update-key! o key k v)))
           (:obj cached)
           definition))})

(defn- build!
  ([definition]
   (build! definition []))
  ([definition parent-key]
   (let [key    (conj parent-key (:impi/key definition))
         cache! #(swap! cache assoc key %)]
     (:obj (if-let [cached (@cache key)]
             (if (= (:def cached) definition)
               cached
               (-> cached
                   (update! key definition)
                   (doto cache!)))
             (-> {:def {}, :obj (create definition)}
                 (update! key definition)
                 (doto cache!)))))))

(defn render [renderer scene]
  (js/requestAnimationFrame #(.render renderer (build! scene))))
