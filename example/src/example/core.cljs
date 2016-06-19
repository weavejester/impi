(ns example.core
  (:require [impi.core :as impi]))

(defonce renderer
  (impi/renderer [400 300]))

(let [element (.getElementById js/document "app")]
  (impi/mount renderer element)
  (impi/render renderer {}))
