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
  (reduce-kv (fn [o k v] (if (= v (old-def k)) o (update-prop! o k v)))
             object
             new-def))

(defn create [definition]
  {:def {}, :obj (create-object definition)})

(defn update! [cached definition]
  {:def definition, :obj (update-object! (:obj cached) (:def cached) definition)})

(defonce cache (atom {}))

(defn build
  ([definition]
   (build [] definition))
  ([parent-index definition]
   {:pre (:impi/key definition)}
   (let [index  (conj parent-index (:impi/key definition))
         cache! (fn [value] (swap! cache assoc index value))]
     (if-let [cached (@cache index)]
       (if (= (:def cached) definition)
         cached
         (-> cached
             (update! definition)
             (doto cache!)))
       (-> (create definition)
           (update! definition)
           (doto cache!))))))

(defn render [renderer scene]
  (js/requestAnimationFrame #(.render renderer (:obj (build scene)))))
