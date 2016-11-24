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

(def ^:private blend-modes
  {:pixi.object.blend-mode/normal   js/PIXI.BLEND_MODES.NORMAL
   :pixi.object.blend-mode/add      js/PIXI.BLEND_MODES.ADD
   :pixi.object.blend-mode/multiply js/PIXI.BLEND_MODES.MULTIPLY
   :pixi.object.blend-mode/screen   js/PIXI.BLEND_MODES.SCREEN})

(def ^:private text-properties
  {:pixi.text.style/align "align"
   :pixi.text.style/break-words "breakWords"
   :pixi.text.style/drop-shadow "dropShadow"
   :pixi.text.style/drop-shadow-angle "dropShadowAngle"
   :pixi.text.style/drop-shadow-blur "dropShadowBlur"
   :pixi.text.style/drop-shadow-color "dropShadowSolor"
   :pixi.text.style/drop-shadow-distance "dropShadowDistance"
   :pixi.text.style/fill "fill"
   :pixi.text.style/font-family "fontFamily"
   :pixi.text.style/font-size "fontSize"
   :pixi.text.style/font-style "fontStyle"
   :pixi.text.style/font-variant "fontVariant"
   :pixi.text.style/font-weight "fontWeight"
   :pixi.text.style/letter-spacing "letterSpacing"
   :pixi.text.style/line-height "lineHeight"
   :pixi.text.style/line-join "lineJoin"
   :pixi.text.style/miter-limit "miterLimit"
   :pixi.text.style/padding "padding"
   :pixi.text.style/stroke "stroke"
   :pixi.text.style/stroke-thickness "strokeThickness"
   :pixi.text.style/word-wrap "wordWrap"
   :pixi.text.style/word-wrap-width "wordWrapWidth"})

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

(defmulti draw-shape! (fn [_ v] (v :pixi.shape/type)))

(defmethod draw-shape! :pixi.shape.type/circle 
  [graphics {[x y]  :pixi.shape/position
             radius :pixi.circle/radius}]
  (.drawCircle graphics x y radius))

(defmethod draw-shape! :pixi.shape.type/ellipse 
  [graphics {[x y]               :pixi.shape/position
             [x-radius y-radius] :pixi.ellipse/radius}]
  (.drawEllipse graphics x y x-radius y-radius))

(defmethod draw-shape! :pixi.shape.type/polygon 
  [graphics {path :pixi.polygon/path}]
  (.drawPolygon graphics (clj->js path)))

(defmethod draw-shape! :pixi.shape.type/rectangle
  [graphics {[x y]          :pixi.shape/position
             [width height] :pixi.shape/size}]
  (.drawRect graphics x y width height))

(defmethod draw-shape! :pixi.shape.type/rounded-rectangle
  [graphics {[x y]          :pixi.shape/position
             [width height] :pixi.shape/size
             radius         :pixi.rounded-rectangle/radius}]
  (.drawRoundedRect graphics x y width height radius))

(defn- create-filter [filter]
  (js/PIXI.Filter.
   (:pixi.filter/vertex filter)
   (:pixi.filter/fragment filter)
   (clj->js (:pixi.filter/uniforms filter))))

(defn- render-texture? [texture]
  (contains? texture :pixi.render-texture/source))

(defn- create-frame-array [frames]
  (reduce
   (fn [arr {:keys [pixi.frame/duration, pixi.frame/texture]}]
     (assert (not (render-texture? texture)) "Movie clip frame cannot be a render texture")
     (.push arr #js {:time duration, :texture (get-texture texture)})
     arr)
   #js []
   frames))

(declare build!)

(def render-cache (atom {}))

(defn- cache-index [index attr {:keys [impi/key]}]
  (-> index (conj attr) (cond-> key (conj key))))

(defn- cache-render-texture! [index texture source]
  (swap! render-cache update (first index) assoc index #(.render % source texture)))

(defn- uncache-render-texture! [index]
  (swap! render-cache update (first index) dissoc index))

(defmulti create-object :pixi.object/type)

(defmethod create-object :pixi.object.type/sprite [_]
  {:val {}, :obj (js/PIXI.Sprite.)})

(defmethod create-object :pixi.object.type/movie-clip
  [{:keys [pixi.movie-clip/frames pixi.movie-clip/paused?]}]
  (let [movie-clip (js/PIXI.extras.MovieClip. (create-frame-array frames))]
    (when-not paused? (.play movie-clip))
    {:val {}, :obj movie-clip}))

(defmethod create-object :pixi.object.type/container [_]
  {:val {}, :obj (js/PIXI.Container.)})

(defmethod create-object :pixi.object.type/graphics [_]
  {:val {}, :obj (js/PIXI.Graphics.)})

(defmethod create-object :pixi.object.type/text [_]
  {:val {}, :obj (js/PIXI.Text.)})

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
  (if (render-texture? value)
    {:val (dissoc value :pixi.render-texture/source)
     :obj (create-render-texture value)}
    {:val value, :obj (get-texture value)}))

(defmethod create :pixi.graphics/shapes [graphics value]
  {:val value, :obj #js {}})

(defmethod create :pixi.text/style [_ value]
  {:val {}, :obj (js/PIXI.TextStyle.)})

(defmethod create :pixi.object/filters [_ value]
  {:val value, :obj (create-filter value)})

(defmulti update-prop! (fn [object index attr value] attr))

(defmethod update-prop! :default [object _ _ _])

(defmethod update-prop! :pixi.object/alpha [object _ _ alpha]
  (set! (.-alpha object) (or alpha 1.0)))

(defmethod update-prop! :pixi.object/blend-mode [object _ _ mode]
  (set! (.-blendMode object) (blend-modes mode js/PIXI.BLEND_MODES.NORMAL)))

(defmethod update-prop! :pixi.object/position [object _ _ [x y]]
  (set! (-> object .-position .-x) x)
  (set! (-> object .-position .-y) y))

(defmethod update-prop! :pixi.object/rotation [object _ _ angle]
  (set! (.-rotation object) angle))

(defmethod update-prop! :pixi.object/scale [object _ _ [x y]]
  (set! (-> object .-scale .-x) (or x 1))
  (set! (-> object .-scale .-y) (or y 1)))

(defmethod update-prop! :pixi.object/filters [object index attr filters]
  (set! (.-filters object) (apply array (map #(build! index attr %) filters))))

(defmethod update-prop! :pixi.object/interactive? [object _ _ interactive?]
  (set! (.-interactive object) interactive?))

(defmethod update-prop! :pixi.object/hit-area [object _ _ rect]
  (if (some? rect)
    (set! (.-hitArea object) (rectangle rect))
    (js-delete object "hitArea")))

(defmethod update-prop! :pixi.object/contains-point [object _ _ pred]
  (if (some? pred)
    (set! (.-containsPoint object) pred)
    (js-delete object "containsPoint")))

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

(defmethod update-prop! :pixi.graphics/shapes 
  [graphics-obj _ _ shapes]
  (.clear graphics-obj)
  (doseq [{:keys [pixi.shape/fill pixi.shape/line] :as shape}
          (if (map? shapes) (vals shapes) shapes)]
    (.lineStyle graphics-obj
                (or (:pixi.line/width line) 0)
                (:pixi.line/color line)
                (or (:pixi.line/alpha line) 1))
    (when fill 
      (.beginFill graphics-obj
                  (:pixi.fill/color fill)
                  (or (:pixi.fill/alpha fill) 1)))
    (draw-shape! graphics-obj shape)
    (when fill (.endFill graphics-obj))))

(defmethod update-prop! :pixi.text/text  [object _ _ text]
  (set! (.-text object) text))

(defmethod update-prop! :pixi.text/style [object index attr text-style]
  (set! (.-style object) (build! index attr text-style)))

(doseq [attr (keys text-properties)]
  (derive attr :pixi.text.style/property))

(defmethod update-prop! :pixi.text.style/property [object _ attr value]
  (aset object (text-properties attr) value))

(defmethod update-prop! :pixi.sprite/anchor [sprite _ _ [x y]]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y))

(defmethod update-prop! :pixi.sprite/texture [sprite index attr texture]
  (set! (.-texture sprite) (build! index attr texture)))

(defmethod update-prop! :pixi.movie-clip/paused? [movie-clip _ _ paused?]
  (set! (.-impiPaused movie-clip) paused?)
  (cond
    (and paused? (.-playing movie-clip))
    (.stop movie-clip)
    (and (not paused?) (not (.-playing movie-clip)))
    (.play movie-clip)))

(defmethod update-prop! :pixi.movie-clip/loop? [movie-clip _ _ loop?]
  (set! (.-loop movie-clip) loop?)
  (when (and loop? (not (.-playing movie-clip)) (not (.-impiPaused movie-clip)))
    (.play movie-clip)))

(defmethod update-prop! :pixi.movie-clip/animation-speed [movie-clip _ _ speed]
  (set! (.-animationSpeed movie-clip) speed))

(defmethod update-prop! :pixi.render-texture/source [texture index attr source]
  (let [index (cache-index index attr source)]
    (if (some? source)
      (cache-render-texture! index texture (build! index attr source))
      (uncache-render-texture! index))))

(defmethod update-prop! :pixi.render-texture/size [texture _ _ [w h]]
  (.resize texture w h true))

(defmethod update-prop! :pixi.renderer/size [renderer _ _ [w h]]
  (.resize renderer w h))

(defmethod update-prop! :pixi.renderer/background-color [renderer _ _ color]
  (set! (.-backgroundColor renderer) color))

(defmethod update-prop! :pixi.filter/padding [filter _ _ padding]
  (set! (.-padding filter) padding))

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
    :pixi.movie-clip/frames
    :pixi.texture/scale-mode
    :pixi.texture/source
    :pixi.texture/frame
    :pixi.texture/crop
    :pixi.texture/trim
    :pixi.texture/rotate
    :pixi.filter/vertex
    :pixi.filter/fragment
    :pixi.filter/uniforms})

(defn- should-recreate? [old-value new-value]
  (not= (select-keys old-value recreate-keys)
        (select-keys new-value recreate-keys)))

(def build-cache (atom {}))

(defn- build! [index attr value]
  (let [index  (cache-index index attr value)
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
    (build! [key] :pixi/stage stage)))

(defn- render-view [key renderer stage]
  (letfn [(render []
            (doseq [f (vals (@render-cache key))]
              (f renderer))
            (.render renderer stage))]
    (render)
    (on-loaded-textures #(js/requestAnimationFrame render))))

(defn mount [key scene element]
  (build-listeners! key scene)
  (when-let [renderer (build-renderer! key scene)]
    (mount-view renderer element)
    (when-let [stage (build-stage! renderer key scene)]
      (render-view key renderer stage))))

(defn unmount [key]
  (when-let [renderer (:obj (@build-cache [key :pixi/renderer]))]
    (let [view (.-view renderer)]
      (when-let [parent (.-parentNode view)]
        (.removeChild parent view)))))
