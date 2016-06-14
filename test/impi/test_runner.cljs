(ns impi.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [impi.core-test]))

(doo-tests 'impi.core-test)
