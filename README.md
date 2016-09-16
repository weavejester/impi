# Impi

Impi is **Im**utable **Pi**xi. It's a ClojureScript library for using
[Pixi.js][] through an immutable data structure.

This library is **experimental**. Experimental means the API is
unstable, and may change significantly. Documentation will be sparse
until the library is in a more usable state.

[pixi.js]: http://www.pixijs.com/


## Installation

To install, add the following to your project `:dependencies`:

    [impi "0.0.6"]


## Usage

Impi has just two functions: `mount` and `unmount`.

We can `mount` a data structure that represents a Pixi scene onto a
DOM node:

```clojure
(impi/mount
 :example-scene
 {:pixi/renderer
  {:pixi.renderer/size [400 300]}
  :pixi/stage
  {:impi/key             :bunny
   :pixi.object/type     :pixi.object.type/sprite
   :pixi.object/position [200 150]
   :pixi.sprite/anchor   [0.5 0.5]
   :pixi.sprite/texture  {:pixi.texture/source "img/bunny.png"}}}
 (.getElementById js/document "app"))
```

When we've finished, we can `unmount` using the key we supplied:

```clojure
(impi/unmount :example-scene)
```

Behind the curtain, Impi converts the immutable scene definition into
a mutable object tree. The objects are cached, so incremental changes
can be efficiently processed.


## License

Copyright © 2016 James Reeves

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
