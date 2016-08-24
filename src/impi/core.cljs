(ns impi.core
  (:require cljsjs.pixi))

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

(def listeners (atom {}))

(defn- replace-listener [object event index [key & args]]
  (let [listener ((@listeners (first index)) key)]
    (doto object
      (.removeAllListeners event)
      (.on event #(apply listener % args)))))

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

(defn- create-render-texture [texture]
  (let [mode  (-> texture :pixi.texture/scale-mode scale-modes)
        [w h] (:pixi.render-texture/size texture)]
    (.create js/PIXI.RenderTexture w h mode)))

(defn- create-filter [filter]
  (js/PIXI.Filter.
   (:pixi.filter/vertex filter)
   (:pixi.filter/fragment filter)
   (clj->js (:pixi.filter/uniforms filter))))

(declare build!)

(declare ^:dynamic *renderer*)

(defmulti create-object :pixi.object/type)

(defmethod create-object :pixi.object.type/sprite [_]
  {:val {}, :obj (js/PIXI.Sprite.)})

(defmethod create-object :pixi.object.type/container [_]
  {:val {}, :obj (js/PIXI.Container.)})

(defmulti create
  (fn [attr value] attr))

(defmethod create :pixi/renderer
  [_ {[w h] :pixi.renderer/size, transparent? :pixi.renderer/transparent? :as options}]
  {:val (select-keys options [:pixi.renderer/size :pixi.renderer/transparent?])
   :obj (js/PIXI.autoDetectRenderer w h #js {:transparent transparent?})})

(defmethod create :pixi/stage [_ value]
  (create-object value))

(defmethod create :pixi.container/children [_ value]
  (create-object value))

(defmethod create :pixi.render-texture/source [_ value]
  (create-object value))

(defmethod create :pixi.sprite/texture [_ value]
  (if (contains? value :pixi.render-texture/source)
    {:val (dissoc value :pixi.render-texture/source)
     :obj (create-render-texture value)}
    {:val value, :obj (get-texture value)}))

(defmulti update-prop! (fn [object index attr value] attr))

(defmethod update-prop! :default [object _ _ _])

(defmethod update-prop! :pixi.object/alpha [object _ _ alpha]
  (set! (.-alpha object) (or alpha 1.0)))

(defmethod update-prop! :pixi.object/position [object _ _ [x y]]
  (set! (-> object .-position .-x) x)
  (set! (-> object .-position .-y) y))

(defmethod update-prop! :pixi.object/rotation [object _ _ angle]
  (set! (.-rotation object) angle))

(defmethod update-prop! :pixi.object/scale [object _ _ [x y]]
  (set! (-> object .-scale .-x) (or x 1))
  (set! (-> object .-scale .-y) (or y 1)))

(defmethod update-prop! :pixi.object/filters [object _ _ filters]
  (set! (.-filters object) (some->> filters (map create-filter) (apply array))))

(defmethod update-prop! :pixi.object/interactive? [object _ _ interactive?]
  (set! (.-interactive object) interactive?))

(defmethod update-prop! :pixi.event/click [object index _ listener]
  (replace-listener object "click" index listener))

(defmethod update-prop! :pixi.event/mouse-down [object index _ listener]
  (replace-listener object "mousedown" index listener))

(defmethod update-prop! :pixi.event/mouse-up [object index _ listener]
  (replace-listener object "mouseup" index listener))

(defmethod update-prop! :pixi.event/mouse-over [object index _ listener]
  (replace-listener object "mouseover" index listener))

(defmethod update-prop! :pixi.event/mouse-out [object index _ listener]
  (replace-listener object "mouseout" index listener))

(defmethod update-prop! :pixi.container/children [container index attr children]
  (->> (if (map? children) (vals children) children)
       (map #(build! index attr %))
       (replace-children container)))

(defmethod update-prop! :pixi.sprite/anchor [sprite _ _ [x y]]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y))

(defmethod update-prop! :pixi.sprite/texture [sprite index attr texture]
  (set! (.-texture sprite) (build! index attr texture)))

(defmethod update-prop! :pixi.render-texture/source [texture index attr scene]
  (let [source   (build! index attr scene)
        renderer *renderer*]
    (.render renderer source texture)
    (on-loaded-textures #(.render renderer source texture))))

(defmethod update-prop! :pixi.render-texture/size [texture _ _ [w h]]
  (.resize texture w h true))

(defmethod update-prop! :pixi.renderer/size [renderer _ _ [w h]]
  (.resize renderer w h))

(defmethod update-prop! :pixi.renderer/background-color [renderer _ _ color]
  (set! (.-backgroundColor renderer) color))

(defn- run-kv! [proc m]
  (reduce-kv (fn [_ k v] (proc k v) nil) nil m)
  nil)

(defn- update-changed-prop! [object index old-value k v]
  (when-not (= v (old-value k)) (update-prop! object index k v)))

(defn- update-removed-prop! [object index new-value k]
  (when-not (contains? new-value k) (update-prop! object index k nil)))

(defn- update! [{object :obj old-value :val} index attr new-value]
  (run-kv! (fn [k v] (update-changed-prop! object index old-value k v)) new-value)
  (run-kv! (fn [k _] (update-removed-prop! object index new-value k)) old-value)
  {:val new-value, :obj object})

(def recreate-keys
  #{:pixi.renderer/transparent?
    :pixi.object/type
    :pixi.texture/scale-mode
    :pixi.texture/source
    :pixi.texture/frame
    :pixi.texture/crop
    :pixi.texture/trim
    :pixi.texture/rotate})

(defn- should-recreate? [old-value new-value]
  (not= (select-keys old-value recreate-keys)
        (select-keys new-value recreate-keys)))

(def build-cache (atom {}))

(defn- build! [index attr value]
  (let [key    (:impi/key value)
        index  (-> index (conj attr) (cond-> key (conj key)))
        cache! #(swap! build-cache assoc index %)]
    (:obj (if-let [cached (@build-cache index)]
            (let [cached-val (:val cached)]
              (if (= value cached-val)
                cached
                (-> (if (should-recreate? cached-val value)
                      (create attr value)
                      cached)
                    (update! index attr value)
                    (doto cache!))))
            (-> (create attr value)
                (update! index attr value)
                (doto cache!))))))

(defn- renderer-mounted? [renderer element]
  (identical? element (.. renderer -view -parentNode)))

(defn- mount-view [renderer element]
  (when-not (renderer-mounted? renderer element)
    (set! (.-innerHTML element) "")
    (.appendChild element (.-view renderer))))

(defn- build-listeners! [key scene]
  (swap! listeners assoc key (:pixi/listeners scene {})))

(defn- build-renderer! [key scene]
  (when-let [renderer (:pixi/renderer scene)]
    (build! [key] :pixi/renderer renderer)))

(defn- build-stage! [renderer key scene]
  (when-let [stage (:pixi/stage scene)]
    (binding [*renderer* renderer]
      (build! [key] :pixi/stage stage))))

(defn- render-view [renderer stage]
  (letfn [(render [] (.render renderer stage))]
    (render)
    (on-loaded-textures #(js/requestAnimationFrame render))))

(defn mount [key scene element]
  (build-listeners! key scene)
  (when-let [renderer (build-renderer! key scene)]
    (mount-view renderer element)
    (when-let [stage (build-stage! renderer key scene)]
      (render-view renderer stage))))

(defn unmount [key]
  (when-let [renderer (:obj (@build-cache [key :pixi/renderer]))]
    (let [view (.-view renderer)]
      (when-let [parent (.-parentNode view)]
        (.removeChild parent view)))))
