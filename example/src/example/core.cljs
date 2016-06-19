(ns example.core
  (:require [impi.core :as impi]))

(let [renderer (impi/renderer [400 300])
      element  (.getElementById js/document "app")]
  (impi/mount renderer element)
  (impi/render renderer {}))
