# Impi

Impi is **Im**utable **Pi**xi. It's a ClojureScript library for using
[Pixi.js][] through an immutable data structure.

This library is **experimental**. Experimental means the API is
unstable, and may change significantly.

[pixi.js]: http://www.pixijs.com/


## Installation

To install, add the following to your project `:dependencies`:

    [impi "0.0.1"]


## Usage

Start by creating a `renderer` of a specific size:

```clojure
(require '[impi.core :as impi])

(def renderer
  (impi/renderer [400 300]))
```

We can `mount` or `unmount` this renderer onto a DOM node:

```clojure
(impi/mount renderer (.getElementById js/document "app"))
```

Once mounted, we can `render` a scene definition:

```clojure
(impi/render
 renderer
 {:impi/key             :bunny
  :pixi/type            :pixi.type/sprite
  :pixi.object/position [200 150]
  :pixi.sprite/anchor   [0.5 0.5]
  :pixi.sprite/texture
  {:pixi/type           :pixi.type/texture
   :pixi.texture/source "img/bunny.png"}})
```

Behind the curtain, Impi converts the immutable scene definition into
a mutable object tree. The objects are cached, so incremental changes
can be efficiently processed.


## License

Copyright © 2016 James Reeves

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
