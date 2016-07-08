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

(defonce cache (atom {}))

(defn- updater [updatef]
  (fn [cached index definition]
    {:def definition
     :obj (let [old-def (:def cached)]
            (reduce-kv
             (fn [o k v] (if (= v (old-def k)) o (updatef o index k v)))
             (:obj cached)
             definition))}))

(defn- builder [keyf createf updatef]
  (let [update! (updater updatef)]
    (fn build
      ([definition]
       (build [] definition))
      ([index definition]
       (let [index   (conj index (keyf definition))
             cache!  (fn [value] (swap! cache assoc index value))]
         (if-let [cached (@cache index)]
           (if (= (:def cached) definition)
             cached
             (-> cached
                 (update! index definition)
                 (doto cache!)))
           (-> {:def {}, :obj (createf definition)}
               (update! index definition)
               (doto cache!))))))))

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

(declare build-object!)
(declare build-texture!)

(defmulti create-texture (comp :pixi.asset/type :pixi.texture/source))

(defmethod create-texture :pixi.asset.type/image [texture]
  (js/PIXI.Texture.fromImage (-> texture :pixi.texture/source :pixi.asset/uri)))

(defmulti create-object :pixi/type)

(defmethod create-object :pixi.type/sprite [_]
  (js/PIXI.Sprite.))

(defmethod create-object :pixi.type/container [_]
  (js/PIXI.Container.))

(defmulti update-key! (fn [object index k v] k))

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

(defmethod update-key! :pixi.container/children [container index _ children]
  (replace-children container (map #(:obj (build-object! index %)) children))
  container)

(defmethod update-key! :pixi.sprite/anchor [sprite _ _ [x y]]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y)
  sprite)

(defmethod update-key! :pixi.sprite/texture [sprite index _ texture]
  (set! (.-texture sprite) (:obj (build-texture! index texture)))
  sprite)

(def build-texture!
  (builder (constantly :pixi.sprite/texture) create-texture update-key!))

(def build-object!
  (builder :impi/key create-object update-key!))

(defn render [renderer scene]
  (js/requestAnimationFrame #(.render renderer (:obj (build-object! scene)))))
