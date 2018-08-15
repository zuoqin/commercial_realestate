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


(def apipath "http://api.commerce.eliz.site/")
(def screenpath "http://snapshots.eliz.site/")
;(def screenpath "https://omnia.f-case.ru/getimg/")

