(ns example.core
  (:require [impi.core :as impi]
            [devtools.core :as devtools]))

(enable-console-print!)
(devtools/install!)

(def outline-shader
  "precision mediump float;

   varying vec2 vTextureCoord;
   uniform sampler2D uSampler;
   uniform vec2 dimensions;

   void main(void) {
       vec2 pixelSize  = vec2(4.0) / dimensions;
       vec4 pixel      = texture2D(uSampler, vTextureCoord);
       vec4 pixelUp    = texture2D(uSampler, vTextureCoord - vec2(0.0, pixelSize.y));
       vec4 pixelDown  = texture2D(uSampler, vTextureCoord + vec2(0.0, pixelSize.y));
       vec4 pixelLeft  = texture2D(uSampler, vTextureCoord - vec2(pixelSize.x, 0.0));
       vec4 pixelRight = texture2D(uSampler, vTextureCoord + vec2(pixelSize.x, 0.0));

       if (pixel.a == 0.0 && (pixelUp.a    > 0.0 ||
                              pixelDown.a  > 0.0 ||
                              pixelLeft.a  > 0.0 ||
                              pixelRight.a > 0.0)) {
           pixel = vec4(1.0, 0.0, 0.0, 1.0);
       }
       else {
           pixel = vec4(0.0, 0.0, 0.0, 0.0);
       }

       gl_FragColor = pixel;
   }")

(defonce state (atom {}))

(reset!
 state
 {:pixi/renderer
  {:pixi.renderer/size             [400 300]
   :pixi.renderer/background-color 0xbbbbbb}
  :pixi/stage
  {:impi/key         :stage
   :pixi.object/type :pixi.object.type/container
   :pixi.container/children
   (sorted-map
    :a
    {:impi/key :performance
     :pixi.object/type :pixi.object.type/container
     :pixi.container/children
     (vec (for [i (range 5), j (range 5)]
            {:impi/key             (keyword (str "bunny" i "_" j))
             :pixi.object/type     :pixi.object.type/sprite
             :pixi.object/position [(+ 200 (* 30 i)) (+ 40 (* 40 j))]
             :pixi.object/rotation 0.0
             :pixi.sprite/anchor   [0.5 0.5]
             :pixi.sprite/texture  {:pixi.texture/source "img/bunny.png"}}))}
    :b
    {:impi/key                 :bunny2
     :pixi.object/type         :pixi.object.type/sprite
     :pixi.object/position     [100 100]
     :pixi.object/scale        [5 5]
     :pixi.object/interactive? true
     :pixi.event/click         (fn [_] (prn :click))
     :pixi.event/mouse-down    (fn [_] (prn :mouse-down))
     :pixi.event/mouse-up      (fn [_] (prn :mouse-up))
     :pixi.event/mouse-over    (fn [_] (prn :mouse-over))
     :pixi.event/mouse-out     (fn [_] (prn :mouse-out))
     :pixi.sprite/anchor       [0.5 0.5]
     :pixi.sprite/texture
     {:pixi.texture/scale-mode :pixi.texture.scale-mode/nearest
      :pixi.texture/source     "img/bunny.png"}}
    :c
    {:impi/key             :rendered
     :pixi.object/type     :pixi.object.type/sprite
     :pixi.object/position [0 0]
     :pixi.object/scale    [3 3]
     :pixi.object/alpha    0.8
     :pixi.sprite/texture
     {:pixi.texture/scale-mode :pixi.texture.scale-mode/nearest
      :pixi.render-texture/size [100 100]
      :pixi.render-texture/source
      {:impi/key             :bunny3
       :pixi.object/type     :pixi.object.type/sprite
       :pixi.object/position [50 50]
       :pixi.sprite/anchor   [0.5 0.5]
       :pixi.sprite/texture
       {:pixi.texture/scale-mode :pixi.texture.scale-mode/nearest
        :pixi.texture/source     "img/bunny.png"}
       :pixi.object/filters
       [{:pixi.filter/fragment outline-shader
         :pixi.filter/uniforms {:dimensions {:type "2f" :value [400.0 300.0]}}}]}}})}})

(defn- rotate-children [children]
  (for [child children]
    (update child :pixi.object/rotation + 0.1)))

(defn animate [state]
  (swap! state
         update-in [:pixi/stage :pixi.container/children :a :pixi.container/children]
         rotate-children)
  (js/setTimeout #(animate state) 16))

(let [element (.getElementById js/document "app")]
  (impi/mount :example @state element)
  (add-watch state ::mount (fn [_ _ _ s] (impi/mount :example s element))))

(defonce x
  (animate state))
