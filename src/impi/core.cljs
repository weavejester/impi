(ns impi.core
  (:require cljsjs.pixi))

(defn renderer [[w h] {:keys [background-color]}]
  (let [r (js/PIXI.autoDetectRenderer w h)]
    (set! (.-backgroundColor r) background-color)
    r))

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

(def texture-cache    (atom {}))
(def pending-textures (atom #{}))

(defn- create-texture [texture]
  (let [source (-> texture :pixi.texture/source image)
        mode   (-> texture :pixi.texture/scale-mode scale-modes)]
    (js/PIXI.Texture. (js/PIXI.BaseTexture. source mode))))

(defn- create-or-fetch-texture [texture]
  (or (@texture-cache texture)
      (let [object (create-texture texture)
            base   (.-baseTexture object)]
        (swap! texture-cache assoc texture object)
        (swap! pending-textures conj base)
        (.on base "loaded" #(swap! pending-textures disj base))
        object)))

(defn- on-loaded-textures [f]
  (doseq [texture @pending-textures]
    (.on texture "loaded" f)))

(declare build!)

(defn- render-texture [renderer object definition]
  (let [source (build! (:pixi.texture/source definition) renderer)]
    (.render renderer source object)
    (on-loaded-textures #(.render renderer source object))))

(derive :pixi.type/sprite    :pixi.type/object)
(derive :pixi.type/container :pixi.type/object)

(defmulti create
  (fn [definition renderer] (:pixi/type definition)))

(defmethod create :pixi.type/sprite [_ _]
  {:def {}, :obj (js/PIXI.Sprite.)})

(defmethod create :pixi.type/container [_ _]
  {:def {}, :obj (js/PIXI.Container.)})

(defmethod create :pixi.type/texture [texture _]
  {:def texture, :obj (create-or-fetch-texture texture)})

(defmethod create :pixi.type/render-texture [texture renderer]
  (let [mode   (-> texture :pixi.texture/scale-mode scale-modes)
        [w h]  (:pixi.texture/size texture)
        object (.create js/PIXI.RenderTexture w h mode)]
    (render-texture renderer object texture)
    {:def (dissoc texture :pixi.texture/source)
     :obj object}))

(defmulti update-prop! (fn [object key value renderer cache-key] key))

(defmethod update-prop! :default [object _ _ _ _])

(defmethod update-prop! :pixi.object/position [object _ [x y] _ _]
  (set! (-> object .-position .-x) x)
  (set! (-> object .-position .-y) y))

(defmethod update-prop! :pixi.object/rotation [object _ angle _ _]
  (set! (.-rotation object) angle))

(defmethod update-prop! :pixi.object/scale [object _ [x y] _ _]
  (set! (-> object .-scale .-x) x)
  (set! (-> object .-scale .-y) y))

(defmethod update-prop! :pixi.container/children [container _ children renderer cache-key]
  (replace-children container (map #(build! % renderer cache-key) children)))

(defmethod update-prop! :pixi.sprite/anchor [sprite _ [x y] _ _]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y))

(defmethod update-prop! :pixi.sprite/texture [sprite _ texture renderer cache-key]
  (set! (.-texture sprite) (build! texture renderer cache-key)))

(defn- update-properties! [object kvs renderer cache-key]
  (doseq [[k v] kvs]
    (update-prop! object k v renderer cache-key)))

(defn- changed-kvs [before after]
  (filter #(not= (val %) (before (key %))) after))

(defmulti update!
  (fn [cached definition renderer key] (:pixi/type definition)))

(defmethod update! :pixi.type/object [{obj :obj old-def :def} new-def renderer key]
  (update-properties! obj (changed-kvs old-def new-def) renderer key)
  {:obj obj, :def new-def})

(defmethod update! :pixi.type/texture [_ definition renderer _]
  (create definition renderer))

(defmethod update! :pixi.type/render-texture
  [{obj :obj old-def :def :as cached} new-def renderer key]
  (let [changed (changed-kvs old-def new-def)]
    (if (= (keys changed) [:pixi.texture/source])
      (do (render-texture renderer obj new-def) cached)
      (create new-def renderer))))

(defmulti object-key :pixi/type)

(defmethod object-key :default [definition]
  (:impi/key definition))

(defmethod object-key :pixi.type/texture [_]
  :pixi.sprite/texture)

(def object-cache (atom {}))

(defn- build!
  ([definition renderer]
   (build! definition renderer []))
  ([definition renderer parent-key]
   (let [key    (conj parent-key (object-key definition))
         cache! #(swap! object-cache assoc key %)]
     (:obj (if-let [cached (@object-cache key)]
             (if (= (:def cached) definition)
               cached
               (-> cached
                   (update! definition renderer key)
                   (doto cache!)))
             (-> (create definition renderer)
                 (update! definition renderer key)
                 (doto cache!)))))))

(defn render [renderer scene]
  (let [scene-object (build! scene renderer)
        render-frame (fn [] (js/requestAnimationFrame #(.render renderer scene-object)))]
    (render-frame)
    (on-loaded-textures render-frame)))
