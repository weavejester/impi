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

(defmulti create-texture (comp :pixi.asset/type :pixi.texture/source))

(defmethod create-texture :pixi.asset.type/image [texture]
  (js/PIXI.Texture.fromImage (-> texture :pixi.texture/source :pixi.asset/uri)))

(declare build!)

(defmulti create-object :pixi/type)

(defmethod create-object :pixi.type/sprite [_]
  (js/PIXI.Sprite.))

(defmethod create-object :pixi.type/container [_]
  (js/PIXI.Container.))

(defmulti update-prop! (fn [object index k v] k))

(defmethod update-prop! :default [object _ _ _] object)

(defmethod update-prop! :pixi.object/position [object _ _ [x y]]
  (set! (-> object .-position .-x) x)
  (set! (-> object .-position .-y) y)
  object)

(defmethod update-prop! :pixi.object/rotation [object _ _ angle]
  (set! (.-rotation object) angle)
  object)

(defmethod update-prop! :pixi.container/children [container index _ children]
  (replace-children container (map #(:obj (build! index %)) children))
  container)

(defmethod update-prop! :pixi.sprite/anchor [sprite _ _ [x y]]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y)
  sprite)

(defmethod update-prop! :pixi.sprite/texture [sprite _ _ texture]
  (set! (.-texture sprite) (create-texture texture))
  sprite)

(defn update-object! [object index old-def new-def]
  (reduce-kv (fn [o k v] (if (= v (old-def k)) o (update-prop! o index k v)))
             object
             new-def))

(defn create [definition]
  {:def {}, :obj (create-object definition)})

(defn update! [cached index definition]
  {:def definition
   :obj (update-object! (:obj cached) index (:def cached) definition)})

(defonce cache (atom {}))

(defn build!
  ([definition]
   (build! [] definition))
  ([parent-index definition]
   {:pre (:impi/key definition)}
   (let [index  (conj parent-index (:impi/key definition))
         cache! (fn [value] (swap! cache assoc index value))]
     (if-let [cached (@cache index)]
       (if (= (:def cached) definition)
         cached
         (-> cached
             (update! index definition)
             (doto cache!)))
       (-> (create definition)
           (update! index definition)
           (doto cache!))))))

(defn render [renderer scene]
  (js/requestAnimationFrame #(.render renderer (:obj (build! scene)))))
