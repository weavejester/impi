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

(defn- mark-child-added [child index]
  (set! (.-__impiAdded child) true)
  (set! (.-__impiIndex child) index))

(defn- mark-child-removed [child index]
  (set! (.-__impiRemoved child) true)
  (set! (.-__impiIndex child) index))

(defn- clean-child [child]
  (js-delete child "__impiRemoved")
  (js-delete child "__impiAdded")
  (js-delete child "__impiIndex"))

(defn- remove-child [container child index]
  (set! (.-parent child) nil)
  (.onChildrenChange container index)
  (.emit child "removed" container))

(defn- add-child [container child index]
  (when-let [parent (.-parent child)]
    (.removeChild parent child))
  (set! (.-parent child) container)
  (.onChildrenChange container index)
  (.emit child "added" container))

(defn- remove-children-after [container index]
  (let [container-children (.-children container)]
    (loop [i index]
      (when (< i container-size)
        (remove-child container (aget container-children i) i)))
    (.splice container-children index)))

(defn- replace-children [container children]
  (let [container-children (.-children container)
        container-size     (.-length container-children)]
    (loop [i 0, children children, changed ()]
      (if (seq children)
        (let [child (first children)]
          (if (< i container-size)
            (let [old-child (aget container-children i)]
              (if (identical? child old-child)
                (recur (inc i) (rest children) changed)
                (do (aset container-children i child)
                    (mark-child-removed old-child i)
                    (mark-child-added child i)
                    (recur (inc i)
                           (rest children)
                           (-> changed (conj child) (conj old-child))))))
            (do (.push container-children child)
                (mark-child-added child i)
                (recur (inc i) (rest children) (conj changed child)))))
        (do (remove-children-after container i)
            (doseq [child changed]
              (let [added?   (.-__impiAdded child)
                    removed? (.-__impiRemoved child)
                    index    (.-__impiIndex child)]
                (clean-child child)
                (when-not (and added? removed?)
                  (if added?
                    (add-child container child index)
                    (remove-child container child index))))))))))

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
  (set! (.-texture sprite) (js/PIXI.Texture.fromImage texture))
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
