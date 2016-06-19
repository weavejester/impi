(ns example.core
  (:require [impi.core :as impi]))

(let [renderer (impi/renderer [400 300])]
  (impi/mount renderer (.getElementById js/document "app")))
