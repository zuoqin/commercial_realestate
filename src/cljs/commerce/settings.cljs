(ns commerce.settings
  (:require [om.core :as om :include-macros true]
            [om-tools.dom :as dom :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [secretary.core :as sec :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
  )
  (:import goog.History)
)

(enable-console-print!)


(def apipath "http://5.189.157.176:5000/")
;(def screenpath "http://screenshots.eliz.site/")
(def screenpath "https://omnia.f-case.ru/getimg/")

