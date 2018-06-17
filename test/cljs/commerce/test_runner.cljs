(ns realty.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [realty.core-test]
   [realty.common-test]))

(enable-console-print!)

(doo-tests 'realty.core-test
           'realty.common-test)
