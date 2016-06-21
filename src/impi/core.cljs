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

(defmulti create-object :pixi/type)

(defmethod create-object :pixi.type/sprite [_]
  (js/PIXI.Sprite.))

(defmulti update-prop! (fn [object k v] k))

(defmethod update-prop! :default [object _ _] object)

(defmethod update-prop! :pixi.object/position [object _ [x y]]
  (set! (-> object .-position .-x) x)
  (set! (-> object .-position .-y) y)
  object)

(defmethod update-prop! :pixi.object/rotation [object _ angle]
  (set! (.-rotation object) angle)
  object)

(defmethod update-prop! :pixi.sprite/anchor [sprite _ [x y]]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y)
  sprite)

(defmethod update-prop! :pixi.sprite/texture [sprite _ texture]
  (set! (.-texture sprite) (js/PIXI.Texture.fromImage texture))
  sprite)

(defn update-object! [object old-def new-def]
  (reduce-kv (fn [o k v] (cond-> o (not= v (old-def k)) (update-prop! k v)))
             object
             new-def))

(defonce cache (atom {}))

(defn- cache! [object key definition]
  (swap! cache assoc key {:obj object :def definition}))

(defn build
  ([definition]
   (build [] definition))
  ([parent-key definition]
   {:pre (:impi/key definition)}
   (let [key (conj parent-key (:impi/key definition))]
     (if-let [cached (@cache key)]
       (if (= (:def cached) definition)
         (:obj cached)
         (-> (:obj cached)
             (update-object! (:def cached) definition)
             (doto (cache! key definition))))
       (-> (create-object definition)
           (update-object! {} definition)
           (doto (cache! key definition)))))))

(defn render [renderer scene]
  (js/requestAnimationFrame #(.render renderer (build scene))))
