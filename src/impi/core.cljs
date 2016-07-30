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

(defn- replace-listener [object event listener]
  (doto object
    (.removeAllListeners event)
    (.on event listener)))

(defn- rectangle [[x y w h]]
  (js/PIXI.Rectangle. x y w h))

(defn- image [src]
  (let [image (js/Image.)]
    (set! (.-src image) src)
    image))

(def ^:private scale-modes
  {:pixi.texture.scale-mode/linear  js/PIXI.SCALE_MODES.LINEAR
   :pixi.texture.scale-mode/nearest js/PIXI.SCALE_MODES.NEAREST})

(def base-texture-cache    (atom {}))
(def pending-base-textures (atom #{}))

(defn- base-texture-key [texture]
  [(:pixi.texture/source texture)
   (:pixi.texture/scale-mode texture)])

(defn- create-base-texture [texture]
  (let [source (-> texture :pixi.texture/source image)
        mode   (-> texture :pixi.texture/scale-mode scale-modes)]
    (js/PIXI.BaseTexture. source mode)))

(defn- get-base-texture [texture]
  (let [key (base-texture-key texture)]
    (or (@base-texture-cache key)
        (let [object (create-base-texture texture)]
          (swap! base-texture-cache assoc key object)
          (swap! pending-base-textures conj object)
          (.on object "loaded" #(swap! pending-base-textures disj object))
          object))))

(defn- on-loaded-textures [f]
  (doseq [texture @pending-base-textures]
    (.on texture "loaded" f)))

(def texture-cache (atom {}))

(defn- create-texture [texture]
  (js/PIXI.Texture.
   (get-base-texture texture)
   (some-> texture :pixi.texture/frame rectangle)
   (some-> texture :pixi.texture/crop rectangle)
   (some-> texture :pixi.texture/trim rectangle)
   (some-> texture :pixi.texture/rotate?)))

(defn- get-texture [texture]
  (or (@texture-cache texture)
      (let [object (create-texture texture)]
        (swap! texture-cache assoc texture object)
        object)))

(defn- create-filter [filter]
  (js/PIXI.Filter.
   (:pixi.filter/vertex filter)
   (:pixi.filter/fragment filter)
   (clj->js (:pixi.filter/uniforms filter))))

(declare build!)

(defmulti create
  (fn [definition renderer] (:pixi/type definition)))

(defmethod create :pixi.type/sprite [_ _]
  {:def {}, :obj (js/PIXI.Sprite.)})

(defmethod create :pixi.type/container [_ _]
  {:def {}, :obj (js/PIXI.Container.)})

(defmethod create :pixi.type/texture [texture _]
  {:def texture, :obj (get-texture texture)})

(defmethod create :pixi.type/render-texture [texture renderer]
  (let [mode   (-> texture :pixi.texture/scale-mode scale-modes)
        [w h]  (:pixi.render-texture/size texture)
        object (.create js/PIXI.RenderTexture w h mode)]
    {:def (dissoc texture :pixi.render-texture/source)
     :obj object}))

(defmulti update-prop! (fn [object key value renderer cache-key] key))

(defmethod update-prop! :default [object _ _ _ _])

(defmethod update-prop! :pixi.object/alpha [object _ alpha _ _]
  (set! (.-alpha object) (or alpha 1.0)))

(defmethod update-prop! :pixi.object/position [object _ [x y] _ _]
  (set! (-> object .-position .-x) x)
  (set! (-> object .-position .-y) y))

(defmethod update-prop! :pixi.object/rotation [object _ angle _ _]
  (set! (.-rotation object) angle))

(defmethod update-prop! :pixi.object/scale [object _ [x y] _ _]
  (set! (-> object .-scale .-x) (or x 1))
  (set! (-> object .-scale .-y) (or y 1)))

(defmethod update-prop! :pixi.object/filters [object _ filters _ _]
  (set! (.-filters object) (some->> filters (map create-filter) (apply array))))

(defmethod update-prop! :pixi.object/interactive? [object _ interactive? _ _]
  (set! (.-interactive object) interactive?))

(defmethod update-prop! :pixi.event/mouse-down [object _ listener _ _]
  (replace-listener object "mousedown" listener))

(defmethod update-prop! :pixi.event/mouse-up [object _ listener _ _]
  (replace-listener object "mouseup" listener))

(defmethod update-prop! :pixi.container/children [container _ children renderer cache-key]
  (replace-children container (map #(build! % renderer cache-key) children)))

(defmethod update-prop! :pixi.sprite/anchor [sprite _ [x y] _ _]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y))

(defmethod update-prop! :pixi.sprite/texture [sprite _ texture renderer cache-key]
  (set! (.-texture sprite) (build! texture renderer cache-key)))

(defmethod update-prop! :pixi.render-texture/source [texture _ scene renderer cache-key]
  (let [source (build! scene renderer)]
    (.render renderer source texture)
    (on-loaded-textures #(.render renderer source texture))))

(defmethod update-prop! :pixi.render-texture/size [texture _ [w h] _ _]
  (.resize texture w h true))

(defn- some-kv [pred m]
  (reduce-kv (fn [r k v] (if-let [x (pred k v)] (reduced x) r)) false m))

(defn- run-kv! [proc m]
  (reduce-kv (fn [_ k v] (proc k v) nil) nil m)
  nil)

(defn- update-changed-prop! [object old-def k v renderer cache-key]
  (when-not (= v (old-def k)) (update-prop! object k v renderer cache-key)))

(defn- update-removed-prop! [object new-def k renderer cache-key]
  (when-not (contains? new-def k) (update-prop! object k nil renderer cache-key)))

(defn- update! [{object :obj old-def :def} new-def renderer key]
  (run-kv! (fn [k v] (update-changed-prop! object old-def k v renderer key)) new-def)
  (run-kv! (fn [k _] (update-removed-prop! object new-def k renderer key)) old-def)
  {:def new-def, :obj object})

(defn- changed-keys? [pred old-def new-def]
  (or (some-kv (fn [k v] (and (pred k) (not= v (old-def k)))) new-def)
      (some-kv (fn [k v] (and (pred k) (not (contains? new-def k)))) old-def)))

(defmulti should-recreate?
  (fn [old-def new-def] (:pixi/type new-def)))

(defmethod should-recreate? :default [old-def new-def]
  (not= (:pixi/type old-def) (:pixi/type new-def)))

(defmethod should-recreate? :pixi.type/texture [old-def new-def]
  true)

(defmethod should-recreate? :pixi.type/render-texture [old-def new-def]
  (let [update-keys #{:pixi.render-texture/size :pixi.render-texture/source}]
    (changed-keys? (complement update-keys) old-def new-def)))

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
             (let [cached-def (:def cached)]
               (if (= cached-def definition)
                 cached
                 (-> (if (should-recreate? cached-def definition)
                       (create definition renderer)
                       cached)
                     (update! definition renderer key)
                     (doto cache!))))
             (-> (create definition renderer)
                 (update! definition renderer key)
                 (doto cache!)))))))

(defn render [renderer scene]
  (let [scene-object (build! scene renderer)
        render-frame (fn [] (js/requestAnimationFrame #(.render renderer scene-object)))]
    (render-frame)
    (on-loaded-textures render-frame)))
