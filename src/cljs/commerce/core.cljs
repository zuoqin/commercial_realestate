(ns commerce.core
  (:use [net.unit8.tower :only [t]])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om-tools.dom :as dom :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [secretary.core :as sec :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [ajax.core :refer [GET PUT POST]]
            [om.dom :as omdom :include-macros true]
            [cljs-time.core :as tc]
            [cljs-time.format :as tf]
            [cljs-time.coerce :as te]
            [cljs-time.local :as tl]
            [cljs.core.async :refer [put! dropping-buffer chan take! <! >! timeout close!]]
            [om-bootstrap.button :as b]
            [commerce.settings :as settings]
            [clojure.string :as str]
            [goog.string :as gstring]
            [goog.string.format]
  )
  (:import goog.History)
)

(enable-console-print!)


(def ch (chan (dropping-buffer 2)))




(defn error-handler [{:keys [status status-text]}]
  (.log js/console (str "something bad happened: " status " " status-text))
)

(def allparams ["id",  "RoomsNum", "Storey", "StoreysNum",  "RawAddress", "MicroDistrict", "RepairRaw","BuildingYear", "LivingSpaceArea", "KitchenArea", "SubwayTime"])


(defonce app-state (atom {:showcoeff true :state 0 :sort-list 1 :isloading false :selectedimage "r01-cian.ru/Snapshots/25.png" :object {:koeff1 0 :koeff2 0 :koeff3 0 :koeff4 0 :koeff5 0 :koeff6 0 :koeff7 0 :koeff8 0 :koeff9 0 :koeff10 0 :koeff11 0 :koeff12 0 :koeff13 0 :koeff14 0 :koeff15 0 :koeff16 0 :approach 0 :hasshopwindows true :isbuildingliving true :houseline true :isentrance true :param ["id",  "RoomsNum", "Storey", "StoreysNum",  "RawAddress", "MicroDistrict", "RepairRaw","BuildingYear", "LivingSpaceArea", "KitchenArea", "SubwayTime"] :repair "стандарт" :analogs [] :foundation "" :project "" :data 0.0 :lat 55.751244 :lon 37.618423 :objecttype "Помещение" :leavingsquare 73.5 :kitchensquare 18.9 :totalsquare 40.7 :city "Москва" :assignment "псн" :analogscount 5 :buildingyear 2000 :ceilingheight 2.6 :storey 1 :storeysnum 9 :pricePerMetr 0.0 :metrodistance 15 :houseAvrgPrice 0.0 :regionAvrgPrice 0.0 :cityAvrgPrice 0.0}}))





(def jquery (js* "$"))





(defn map-user-node [user]
  {:text (:firstname user) :id (:id user) :icon "fa fa-hdd-o" :selectedIcon "glyphicon glyphicon-ok" :selectable true :state {:checked false :disabled false :expanded true :selected false} }
)


(defn map-dev-node [dev]
  {:text (:name dev) :unitid (:id dev) :icon "fa fa-hdd-o" :selectedIcon "glyphicon glyphicon-ok" :selectable true :state {:checked false :disabled false :expanded true :selected false} }
)

(defn comp-units [unit1 unit2]
  (if (< (compare (:text unit1) (:text unit2)) 0)
    true false
  )
)

(defn buildUnits [id]
  (let [
    devices (if (> (count (:devices @app-state)) 0) (filter (fn [x] (if (and (not (nil? (:groups x))) (> (.indexOf (:groups x) id) -1))  true false)) (:devices @app-state)) []) 
    nodes (into [] (sort (comp comp-units) (map map-dev-node devices)) )
    ;tr1 (.log js/console nodes)
    ]
    nodes
  )
)

(defn buildUsers [id]
  (let [
    users (if (> (count (:users @app-state)) 0) (filter (fn [x] (if (and (not (nil? (:groups x))) (> (.indexOf (:groups x) id) -1))  true false)) (:users @app-state)) []) 
    nodes (into [] (map map-user-node users))
    ;tr1 (.log js/console nodes)
    ]
    nodes
  )
)





(defn setVersionInfo [info]
  (swap! app-state assoc-in [:verinfo] 
    (:info info)
  )

  (swap! app-state assoc-in [:versionTitle]
    (str "Информация о текущей версии")
  ) 

  (swap! app-state assoc-in [:versionText]
    (str (:info info))
  ) 

  ;;(.log js/console (str  "In setLoginError" (:error error) ))
  (jquery
    (fn []
      (-> (jquery "#versioninfoModal")
        (.modal)
      )
    )
  )
)

(defn handleChange [e]
  (swap! app-state assoc-in [(keyword (.. e -nativeEvent -target -id))] (.. e -nativeEvent -target -value))
)



(defn onVersionInfo []
  (let [     
      newdata { :info "Global Asset Management System пользовательский интерфейс обновлен 01.09.2017 09:28" }
    ]
   
    (setVersionInfo newdata)
  )
  ;(.log js/console (str  response ))
)

(defn addVersionInfo []
  (dom/div
    (dom/div {:id "versioninfoModal" :className "modal fade" :role "dialog"}
      (dom/div {:className "modal-dialog"} 
        ;;Modal content
        (dom/div {:className "modal-content"} 
          (dom/div {:className "modal-header"} 
                   (b/button {:type "button" :className "close" :data-dismiss "modal"})
                   (dom/h4 {:className "modal-title"} (:versionTitle @app-state) )
                   )
          (dom/div {:className "modal-body"}
                   (dom/p (:versionText @app-state))
                   )
          (dom/div {:className "modal-footer"}
                   (b/button {:type "button" :className "btn btn-default" :data-dismiss "modal"} "Close")
          )
        )
      )
    )
  )
)

(defn doswaps []
  (let [a (rand-int 26)
        b (rand-int 26)
        c (rand-int 26)
    ]
    (swap! app-state assoc-in [:fake] (str a b c))
  )
)

(defn split-thousands [n-str]
  (let [index (str/index-of n-str ".")
        lstr (subs n-str 0 (if (nil? index) (count n-str) index))
        rstr (if (nil? index) "" (subs n-str index)) 
        splitstr (->> lstr
          reverse
          (partition 3 3 [])
          (map reverse)
          reverse
          (map #(apply str %))
          (str/join " "))
    ]
    (str splitstr rstr)
  )
)


(defn doLogout []
  ;(swap! app-state assoc-in [:view] 0)

)


 
(defcomponent logout-view [_ _]
  (render
    [_]
    (let [style {:style {:margin "10px"}}]
      (dom/div style (dom/a (assoc style :href "#/login") "Login"))
    )
  )
)


(defn handle-chkb-change [e]
  ;(.log js/console (.. e -target -id) )  
  ;(.log js/console "The change ....")
  (.stopPropagation e)
  (.stopImmediatePropagation (.. e -nativeEvent) )
  (swap! app-state assoc-in [(keyword  (.. e -currentTarget -id) )] 
    (if (= true (.. e -currentTarget -checked)  ) 1 0)
  )
  ;(CheckCalcLeave)
  ;(set! (.-checked (.. e -currentTarget)) false)
  ;(dominalib/remove-attr!  (.. e -currentTarget) :checked)
  ;;(dominalib/set-attr!  (.. e -currentTarget) :checked true)
)

(defn handle-change [e owner]
  
  (swap! app-state assoc-in [:form (keyword (.. e -target -id))] 
    (.. e -target -value)
  ) 
)

(defn handle-change-currency [e owner]
  
  (swap! app-state assoc-in [:form (keyword (.. e -target -id))] 
    (.. e -target -value)
  ) 
)










(defn onDidUpdate [data]
  ;(setClientsDropDown)
    ;; (jquery
    ;;   (fn []
    ;;     (-> (jquery "#side-menu")
    ;;       (.metisMenu)
    ;;     )
    ;;   )
    ;; )

)

(defn onMount [data]
  ;(.log js/console "Mount core happened")

  (if (or (:isalert @app-state) (:isnotification @app-state))
    (go
         (<! (timeout 500))
         (put! ch 42)
    )
  )
)



(defn handleCheck [e]
  (.stopPropagation e)
  (.stopImmediatePropagation (.. e -nativeEvent) )
  (swap! app-state assoc-in [(keyword (.. e -nativeEvent -target -id))] (.. e -nativeEvent -target -checked))
)

(defn printMonth []
  (.print js/window)
)


(defn onDropDownChange [id value]
  (let [
        ;tr1 (.log js/console "id=" id " value=" value)
    ]
    (if (= id "statuses")
      (swap! app-state assoc-in [:selectedstatus] (js/parseInt value))
    )
  )
  ;;(.log js/console value)  

)


(defn setStatusesDropDown []
       (-> (jquery "#statuses" )
         (.selectpicker)
       )
  ;; (jquery
  ;;    (fn []

  ;;    )
  ;;  )
   (jquery
     (fn []
       (-> (jquery "#statuses" )
         (.selectpicker "val" (:selectedstatus @app-state))
         (.on "change"
           (fn [e]
             (
               onDropDownChange (.. e -target -id) (.. e -target -value)
             )
           )
         )
       )
     )
   )

  (let [
    els (.getElementsByClassName js/document "filter-option pull-left")
    ]
    ;(doall (map (fn [x] (set! (.-textAlign (.-style x)) (str "right"))) els))
    
    (set! (.-textAlign (.-style (aget els 0))) (str "right"))
    ;(set! (.-textAlign (.-style (aget els 2))) (str "right"))
  )
)


(defcomponent main-navigation-view [data owner]  
  (did-mount [this]
    (let [
      ;tr1 (.log js/console "did mount")
      ]
      (onMount data)
    )    
  )
  (render [_]
    (let [
      ;user (first (filter (fn [x] (if (= (:userid x) (:userid (:token @app-state))) true false)) (:users @app-state)))
      ;tr1 (.log js/console (str "in map navigation"))
      ;role (:id (:role user))
      ;fullname (str (:firstname user) " " (:lastname user))
      ;role (:name (:role (first (filter (fn[x] (if (= (:userid (:token @data)) (:userid x)) true false)) (:users @data)))))

      ;tr1 (.log js/console (str "role=" role))

      ]
      (dom/div {:className "navbar navbar-inverse navbar-fixed-top" :role "navigation" :style {:width "100%" :height "70px" :margin-left "15px" :border-top "solid 3px steelblue" :border-bottom "solid 3px steelblue"}}
        (dom/div {:className "navbar-header"}
          (dom/button {:type "button" :className "navbar-toggle"
            :data-toggle "collapse" :data-target ".navbar-collapse"}
            (dom/span {:className "sr-only"} "Toggle navigation")
            (dom/span {:className "icon-bar"})
            (dom/span {:className "icon-bar"})
            (dom/span {:className "icon-bar"})
          )
          (dom/a {:className "navbar-brand" :style {:padding-top "5px" :padding-left "5px" :padding-right "5px" :margin-left "10px"}}
            (dom/img {:src "images/loginbackground_black.png" :className "img-responsive company-logo-logon" :style {:width "120px" :height "50px"}})
            ;;(dom/span {:id "pageTitle"} "Beeper")
          )          
        )

        (dom/div {:className "collapse navbar-collapse navbar-ex1-collapse" :id "bs-example-navbar-collapse-1" :style {
            :font-size "larger"
          }}

          (dom/ul {:className "nav navbar-nav" :style {:padding-top "10px"}}

            (dom/li
              (dom/a {:className "navbara" :href "#/map" :style {:padding-left "15px" :padding-right "0px" :color (if (str/includes? (.-href (.-location js/window)) "/#/map") "rgb(51, 122, 183)" "#9d9d9d")}}
                (dom/i {:className "fa fa-map-o" :style {:margin-left "5px"}})
                "מפה"
              )
            )
            (dom/li
              (dom/a {:className "navbara" :href "#/dashboard" :style {:padding-left "15px" :padding-right "15px" :color (if (str/includes? (.-href (.-location js/window)) "/#/dashboard") "rgb(51, 122, 183)" "#9d9d9d")} :onMouseOver (fn [x]
                                                                                        (set! (.-display (.-style (js/document.getElementById "navbarulmanage")) ) "none")
                                                                                        (set! (.-display (.-style (js/document.getElementById "navbarulreports")) ) "none"))}
                (dom/i {:className "fa fa-dashboard" :style {:margin-left "5px"}})
                "Dashboard"
              )
            )

            ;; (dom/li
            ;;   (dom/a {:className "navbara" :href "#/devslist" :onMouseOver (fn [x] (set! (.-display (.-style (js/document.getElementById "navbarulmanage")) ) "none"))}
            ;;     (dom/i {:className "fa fa-building"})
            ;;     "רשימה יחידות"
            ;;   )
            ;; )




            (dom/li {:className "dropdown" :style {:min-width "230px"}}
              (dom/a { :href "#" :className "navbarareports" :style {:padding-left "0px" :padding-right "0px" :color (if (or (str/includes? (.-href (.-location js/window)) "/#/reportalerts") (str/includes? (.-href (.-location js/window)) "/#/reportsensors") (str/includes? (.-href (.-location js/window)) "/#/report"))  "rgb(51, 122, 183)" "#9d9d9d")}
                :onMouseOver (fn [x]
                  (set! (.-display (.-style (js/document.getElementById "navbarulreports")) ) "block")
                  (set! (.-display (.-style (js/document.getElementById "navbarulmanage")) ) "none")
                )
                ;:onMouseLeave (fn [x] (set! (.-display (.-style (js/document.getElementById "navbarulreports")) ) "none"))
                }
                (dom/span {:className "caret"})
                (dom/i {:className "fa fa-archive" :style {:margin-left "5px"}})
                "דו״חות"
              )
              (dom/div { :id "navbarulreports" :className "navbarulreports" :style {:display "none" :background-color "#f9f9f9" :border-left "2px solid #337ab7" :border-bottom "2px solid #337ab7" :border-right "2px solid #337ab7" :border-top "1px solid #337ab7"} :onMouseLeave (fn [x] (set! (.-display (.-style (js/document.getElementById "navbarulreports")) ) "none"))}
                (dom/div {:className "row" :style {:margin-top "5px" :margin-left "0px" :margin-right "0px"}}
                  (dom/div {:className "col-md-12" :style {:padding-left "5px" :padding-right "15px"}}
                    (dom/a {:href "#/reportalerts" :className "menu_item" :style {:padding-left "0px" :padding-right "0px"}}
                      (dom/i {:className "fa fa-line-chart" :style {:padding-left "5px"}})
                      "דו''ח תקלות"
                    )
                  )
                )
                (dom/div {:className "row" :style {:margin-top "5px" :margin-left "0px" :margin-right "0px"}}
                  (dom/div {:className "col-md-12" :style {:padding-left "5px" :padding-right "15px"}}
                    (dom/a {:href "#/reportsensors" :className "menu_item" :style {:padding-left "0px" :padding-right "0px"}}
                      (dom/i {:className "fa fa-bullhorn" :style {:padding-left "5px"}})
                      "דו''ח חיווים"
                    )
                  )
                )
                (dom/div {:className "row" :style {:margin-top "5px" :margin-left "0px" :margin-right "0px"}}
                  (dom/div {:className "col-md-12" :style {:padding-left "5px" :padding-right "15px"}}
                    (dom/a {:href "#/reportcomands" :className "menu_item" :style {:padding-left "0px" :padding-right "0px"}}
                      (dom/i {:className "fa fa-envelope-o" :style {:padding-left "5px"}})
                      "דו''ח שליחת פקודות הפעלה"
                    )
                  )
                )
              )
            )


          )

          (dom/ul {:className "nav navbar-nav navbar-left"}
            (dom/li {:style {:margin-right "0px" :margin-top "10px" :text-align "center"} :onMouseOver (fn [x] (set! (.-display (.-style (js/document.getElementById "navbarulexit")) ) "none"))}
              (dom/div {:style {:padding-top "10px" :color "#337ab7" :margin-left "30px"}} (str "שירות לקוחות" " " "03-6100066"))
              ;(dom/h5 {:style {:padding-top "0px" :color "#337ab7" }} "03-123-456-789")
            )
            (dom/li {:className "dropdown"}
              (dom/a { :href "#" :className "navbaraexit" :style {:padding-left "10px" :padding-right "0px" :max-width "150px" :text-align "left"}
                :onMouseOver (fn [x]
                  (set! (.-display (.-style (js/document.getElementById "navbarulexit")) ) "block")
                )
                ;:onMouseLeave (fn [x] (set! (.-display (.-style (js/document.getElementById "navbarulreports")) ) "none"))
                }
                (dom/span {:className "caret"})
                (dom/i {:className "fa fa-user-circle" :style {:margin-left "3px" :margin-right "3px"}})
                  (:name (:token @app-state))
                  (dom/p {:style {:margin "0px"}}
                    (:rolename (:token @app-state))
                  )
              )

              (dom/div { :id "navbarulexit" :className "navbarulexit" :style {:display "none" :background-color "#f9f9f9" :text-align "left"} :onMouseLeave (fn [x] (set! (.-display (.-style (js/document.getElementById "navbarulexit")) ) "none"))}
                (dom/div {:className "row"}
                  (dom/div {:className "col-md-12"}
                    (dom/a {:href "#/login" :className "menu_item" :style {:padding-left "10px" :padding-right "0px"} :onClick (fn [e](doLogout))}
                      (dom/i {:className "fa fa-sign-out" :style {:margin-left "3px" :margin-right "3px"}})
                      "יציאה"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)



(defmulti website-view
  (
    fn [data _]   
      (:view (if (= data nil) @app-state @data ))
      ;;(:view @app-state )
  )
)

(defmethod website-view 0
  [data owner] 
  ;(.log js/console "zero found in view")
  (logout-view data owner)
)



(defmethod website-view 2
  [data owner] 
  ;(.log js/console "Two is found in view")
  (main-navigation-view data owner)
)

(defmethod website-view 3
  [data owner] 
  ;(.log js/console "Two is found in view")
  (main-navigation-view data owner)
)


(defn setcontrols [value]
  (case value
    42 ( let [] 
         (setStatusesDropDown)
       )
    49 (js/alert "הפקודה נשלחה בהצלחה")
  )
)

(defn initqueue []
  (doseq [n (range 1000)]
    (go ;(while true)
      (take! ch(
        fn [v] (
           setcontrols v
          )
        )
      )
    )
  )
)

(initqueue)
