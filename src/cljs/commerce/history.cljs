(ns commerce.history
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om-tools.dom :as dom :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [secretary.core :as sec :refer-macros [defroute]]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [commerce.core :as commerce]
            [commerce.settings :as settings]
            [om.dom :as omdom :include-macros true]
            [cljs-time.format :as tf]
            [cljs-time.core :as tc]
            [cljs-time.coerce :as te]
            [cljs-time.local :as tl]
            [clojure.string :as str]
            [ajax.core :refer [GET POST]]
            [om-bootstrap.input :as i]
            [om-bootstrap.button :as b]
            [om-bootstrap.panel :as p]
            [goog.string :as gstring]
            [goog.string.format]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [put! dropping-buffer chan take! <! >! timeout close!]]
  )
  (:import goog.History)
)

(def jquery (js* "$"))

(enable-console-print!)

(def ch (chan (dropping-buffer 2)))

(def iconBase "images/")

(def assignments [{:id 1 :type "псн"} {:id 2 :type "торговые"}])

(def objecttypes [{:id 1 :name "Помещение"}])

(def params [{:id "RoomsNum" :name "Количество комнат"} {:id "Storey" :name "Этаж"} {:id "StoreysNum" :name "Количество этажей"} {:id "RawAddress" :name "Адрес"} {:id "MicroDistrict" :name "Район"} {:id "RepairRaw" :name "Ремонт"} {:id "BuildingYear" :name "Год постройки"} {:id "LivingSpaceArea" :name "Жилая площадь"} {:id "KitchenArea" :name "Площадь кухни"} {:id "SubwayTime" :name "Расстояние до метро"}])

(def repairs [{:id 1 :name "без отделки"} {:id 2 :name "косметический"} {:id 3 :name "стандарт"} {:id 4 :name "люкс"} {:id 5 :name "требует капитального ремонта"}])


(defn error-handler [{:keys [status status-text]}]
  (swap! commerce/app-state assoc-in [:state] 0)
  (.log js/console (str "something bad happened: " status " " status-text))
)

(defn error-handler-zkh [{:keys [status status-text]}]
  ;(.log js/console (str "something bad happened: " status " " status-text))
  (swap! commerce/app-state assoc-in [:state] 0)
  (swap! commerce/app-state assoc-in [:object :address] "Ошибка получения данных по квартире")
  
)

(defn comp-analogs
  [analog1 analog2]
  ;(.log js/console group1)
  ;(.log js/console group2)
  (case (:sort-list @commerce/app-state)
    1  (if (> (compare (:address analog1) (:address analog2)) 0)
      false
      true
    )
    2  (if (> (compare (:address analog1) (:address analog2)) 0)
      true
      false
    )
    3  (if (> (:totalsquare analog1) (:totalsquare analog2))
      false
      true
    )
    4  (if (> (:totalsquare analog1) (:totalsquare analog2))
      true
      false
    )
    5  (if (> (:price analog1) (:price analog2))
      false
      true
    )
    6  (if (> (:price analog1) (:price analog2))
      true
      false
    )
    7  (if (> (:pricepermetr analog1) (:pricepermetr analog2))
      false
      true
    )
    8  (if (> (:pricepermetr analog1) (:pricepermetr analog2))
      true
      false
    )
    9  (if (or (> (:index analog1) (:index analog2))
        (and (= (:index analog1) (:index analog2)) (> (:pricepermetr analog1) (:pricepermetr analog2)))
      )
      false
      true
    )
    10  (if (or (> (:index analog1) (:index analog2))
        (and (= (:index analog1) (:index analog2)) (> (:pricepermetr analog1) (:pricepermetr analog2)))
        )
      true
      false
    )
  )
)

(defn drop-nth [n coll]
   (keep-indexed #(if (not= %1 n) %2) coll))


(defn openreportdialog []
  (let [
    ;tr1 (.log js/console (:device @dev-state))
    ]
    (jquery
      (fn []
        (-> (jquery "#reportModal")
          (.modal)
        )
      )
    )
  )
)

(defn gotomain [item]
  (swap! commerce/app-state assoc-in [:object :approach] (:approach item))
  (swap! commerce/app-state assoc-in [:object :address] (:address item))
  (swap! commerce/app-state assoc-in [:object :totalsquare] (:totalsquare item))
  (swap! commerce/app-state assoc-in [:object :storey] (:floor item))
  (swap! commerce/app-state assoc-in [:object :repair] (:repair item))
  (swap! commerce/app-state assoc-in [:object :isentrance] (:entrance item))
  (swap! commerce/app-state assoc-in [:object :houseline] (:houseline item))
  (swap! commerce/app-state assoc-in [:object :hasshopwindows] (:hasshopwindows item))
  (swap! commerce/app-state assoc-in [:object :isbuildingliving] (:isbuildingliving item))
  (swap! commerce/app-state assoc-in [:object :lat] (:lat item))
  (swap! commerce/app-state assoc-in [:object :lon] (:lon item))
  (put! ch 49)
)

(defn handleChange [e]
  (.log js/console (.. e -nativeEvent -target)  )  
  ;(.log js/console (.. e -nativeEvent -target -step))
  (swap! commerce/app-state assoc-in [:object (keyword (.. e -nativeEvent -target -id))] (if (= "" (.. e -nativeEvent -target -step)) (.. e -nativeEvent -target -value) (js/parseFloat (.. e -nativeEvent -target -value))))
)



(defn downloadreport []
  (aset js/window "location" (str "/report"))
  (put! ch 57)
)





(defn setNewUnitValue [key val]
  (swap! commerce/app-state assoc-in [(keyword key)] val)
)

(defn onDropDownChange [id value]
  (let [
    ;value (if (= id "unit") )
    ]
    (case id
      "param" (let [
       oldvalue (:param (:object @commerce/app-state))


       newvalues (loop [result [] nums (range 9)]
                (if (seq nums)
                  (let [
                        num (first nums)
                        ;tr1 (.log js/console (str "num: " num "selected: " (.-selected (aget (.-options (js/document.getElementById id)) num))))
                        ]
                    (recur (if (= (.-selected (aget (.-options (js/document.getElementById id)) num)) true) (conj result (nth commerce/allparams num)) result)
                         (rest nums))
                  )
                  result)
        )

        tr1 (.log js/console (clj->js newvalues))
       ;newvalue (doall (map (fn [x] (if (= (.-selected (aget (.-options (js/document.getElementById id)) x)) true) (conj oldvalue) )) (range 9))) 
          ]
         ;(remove (fn [y] (if (= y value) true false)) oldvalue)
         (swap! commerce/app-state assoc-in [:object (keyword id)] newvalues)
      )
      (swap! commerce/app-state assoc-in [:object (keyword id)] value)
    )
    
  )
  ;(.log js/console (str "id=" id "; value=" value))
)

(defn setDropDowns []
  (jquery
     (fn []
       (-> (jquery "#assignment" )
         (.selectpicker {})
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#param" )
         (.selectpicker {})
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#repair")
         (.selectpicker {})
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#objecttype")
         (.selectpicker {})
       )
     )
   )


   (jquery
     (fn []
       (-> (jquery "#param")
         (.selectpicker "val" (clj->js (:param (:object @commerce/app-state))))
         (.on "change"
           (fn [e]
             (let []
               (onDropDownChange (.. e -target -id) (.. e -target -value))
               (.log js/console (str (.. e -target -id) "val: " (.. e -target -value)))
             )
             
               ;(.log js/console e)
           )
         )
       )
     )
   )


   (jquery
     (fn []
       (-> (jquery "#assignment")
         (.selectpicker "val" (:assignment (:object @commerce/app-state)))
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )

   (jquery
     (fn []
       (-> (jquery "#repair")
         (.selectpicker "val" (:repair (:object @commerce/app-state)))
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )

   (jquery
     (fn []
       (-> (jquery "#objecttype")
         (.selectpicker "val" (:objecttype (:object @commerce/app-state)))
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )
)

(defn array-to-string [element]
  (let [
      newdata {:empname (get element "empname") } 
    ]
    (:empname newdata)
  )
)



(defn OnError [response]
  (let [     
      newdata { :error (get (:response response)  "error") }
    ]
    (.log js/console (str  response )) 
    
  )
  
  
)

(defn handleFromChange [e]
  ;;(.log js/console e  )  
  ;(.log js/console "The change ....")

)



(defn handle-change [e owner]
  ;(.log js/console e)
  (swap! commerce/app-state assoc-in [:object (keyword (.. e -target -id))] 
    (.. e -target -value)
  )

  (.log js/console "jhghghghj")
)


(defn handle-chkb-change [e]
  (let [
    id (js/parseInt (subs (.. e -currentTarget -id) 8))
    isinclude (.. e -currentTarget -checked)
    newanalogs (map (fn [x] (if (= id (:id x)) (assoc-in x [:isinclude] isinclude) x)) (:calcanalogs (:object @commerce/app-state)))
    ]
    ;(.log js/console (str "id:" id ";" isinclude))
    (swap! commerce/app-state assoc-in [:object :calcanalogs] newanalogs)
  )
)


(defcomponent showanalogs-view [data owner]
  (render
    [_
      
    ]
    (let [
      ;tr1 (.log js/console  (:key (om/get-state owner)))
      ;tr1 (.log js/console (count ((keyword (:key (om/get-state owner))) (:object @commerce/app-state))))
      ;tr1 (.log js.console (first ((keyword (:key (om/get-state owner))) (:object @commerce/app-state))) )
      ]
      (if (> (count (:history (:object @commerce/app-state))) 0)
        (dom/div {:className "panel panel-info" :style {:margin-top "20px"}}
          (dom/div {:className "panel panel-heading" :onClick (fn [e] (.log js/console "Clicked001")) :style {:margin-bottom "0px"}}

            (dom/div {:className "row"} 
              ;; (dom/div {:className "col-xs-5  col-xs-offset-0" :style {:text-align "center"}}
              ;;   "Адрес"
              ;; )
              (dom/div {:id "address" :className "col-xs-5" :style {:cursor "pointer" :padding-left "0px" :padding-right "3px" :padding-top "5px" :text-align "center" :background-image (case (:sort-list @data) 1 "url(images/sort_asc.png" 2 "url(images/sort_desc.png" "url(images/sort_both.png") :background-repeat "no-repeat" :background-position "right center"} :onClick (fn [e] (swap! commerce/app-state assoc-in [:sort-list] (case (:sort-list @data) 1 2 1)))}
                          "Адрес"
                        )

              (dom/div {:className "col-xs-1" :style {:cursor "pointer" :padding-left "0px" :padding-right "3px" :padding-top "5px" :text-align "center" :background-image (case (:sort-list @data) 3 "url(images/sort_asc.png" 4 "url(images/sort_desc.png" "url(images/sort_both.png") :background-repeat "no-repeat" :background-position "right center"} :onClick (fn [e] (swap! commerce/app-state assoc-in [:sort-list] (case (:sort-list @data) 3 4 3)))}
                          "Подход"
              )
              (dom/div {:className "col-xs-1" :style {:padding-top "5px" :text-align "center"}}
                "Тип ремонта"
              )

              (dom/div {:className (if (:showcoeff @commerce/app-state) "col-xs-1" "col-xs-1") :style {:padding-left "0px" :padding-right "0px" :padding-top "5px" :text-align "center"}}
                "Вход"
              )

              (dom/div {:className (if (:showcoeff @commerce/app-state) "col-xs-1" "col-xs-1") :style {:padding-left "0px" :padding-right "0px" :padding-top "5px" :text-align "center"}}
                "Линия застройки"
              )
              (dom/div {:className "col-xs-2"}
                (dom/div {:className "row"}
                  (dom/div {:className (if (:showcoeff @commerce/app-state) "col-xs-5" "col-xs-5") :style {:padding-left "0px" :padding-right "0px" :padding-top "5px" :text-align "center"}}
                    "Наличие витринных окон"
                   )
                   (dom/div {:className (if (:showcoeff @commerce/app-state) "col-xs-4" "col-xs-4") :style {:padding-left "0px" :padding-right "0px" :padding-top "5px" :text-align "center"}}
                         "Тип здания"
                   )


                   (dom/div {:className "col-xs-3" :style {:padding-left "0px" :padding-right "0px" :padding-top "5px" :text-align "center"}}
                      "Этаж"
                   )
                )
              )

              (dom/div {:className (if (:showcoeff @commerce/app-state) "col-xs-1" "col-xs-1") :style {:cursor "pointer" :padding-left "0px" :padding-right "3px" :padding-top "5px" :text-align "center" :background-image (case (:sort-list @data) 3 "url(images/sort_asc.png" 4 "url(images/sort_desc.png" "url(images/sort_both.png") :background-repeat "no-repeat" :background-position "right center"} :onClick (fn [e] (swap! commerce/app-state assoc-in [:sort-list] (case (:sort-list @data) 3 4 3)))}
                          "Общая площадь"
              )


            )
          )
          (dom/div {:className "panel panel-body" :style {:padding "0px"}}
            (map-indexed (fn [idx item]
              (let [ 
                square (str (get item "totalsquare"))
                ;tr1 (.log js/console (str item))
                rowstyle {:margin-right "0px" :margin-left "0px" }
                ]
                (dom/div {:className "row tablerow" :style rowstyle :onClick (fn [e] (gotomain item))}
                  (dom/div {:className "col-xs-5":style {:text-align "left" :border "1px solid lightgrey" :padding-left "0px" :padding-right "0px" :padding-top "6px" :overflow "hidden" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap"}} (dom/a {:className "list-group-item" :style {:padding "0px" :border "none" :background "transparent"}}
                    (str (+ idx 1) ". "   (:address item))
                    ))
                  )
                  (dom/div {:className "col-xs-1" :style {:text-align "center" :padding-left "0px" :border "1px solid lightgrey" :padding-top "6px" :overflow "hidden" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap"}} (dom/a {:className "list-group-item" :style {:padding "0px" :border "none" :background "transparent"}}
                    (case (:approach item) 0 "Сравнительный" 1 "Доходный" "Арендный")
                    ))
                  )
                  (dom/div {:className "col-xs-1" :style {:text-align "center" :padding-left "0px" :border "1px solid lightgrey" :padding-top "6px" :overflow "hidden" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap"}} (dom/a {:className "list-group-item" :style {:padding "0px" :border "none" :background "transparent"}}
                    (:repair item)
                    ))
                  )

                  (dom/div {:className (if (:showcoeff @commerce/app-state) "col-xs-1" "col-xs-1") :style {:text-align "center" :padding-left "0px" :border "1px solid lightgrey" :padding-top "6px" :overflow "hidden" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap" :padding-bottom "0px"}} (dom/a {:className "list-group-item" :style {:padding "0px" :border "none" :background "transparent"}}
                    (if (:entrance item) "отдельный" "общий")
                    ))
                  )

                  (dom/div {:className (if (:showcoeff @commerce/app-state) "col-xs-1" "col-xs-1") :style {:text-align "center" :border "1px solid lightgrey" :padding-top "6px" :padding-left "0px" :padding-right "0px" :overflow "hidden" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap" :padding-bottom "0px"}} (dom/a {:className "list-group-item" :style {:padding "0px" :border "none" :background "transparent"}}
                    (if (:houseline item) "первая" "внутриквартальная")
                    ))
                  )

                  (dom/div {:className "col-xs-2"}
                     (dom/div {:className "row"}
                        (dom/div {:className (if (:showcoeff @commerce/app-state) "col-xs-5" "col-xs-5") :style {:text-align "left" :border "1px solid lightgrey" :padding-top "6px" :overflow "hidden" :padding-bottom "6px"}}
                          (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap" :padding-bottom "0px"}} (dom/a {:className "list-group-item" :style {:padding "0px" :border "none" :background "transparent"}}
                          (if (:hasshopwindows item) "есть" "нет")
                          ))
                        )
                        (dom/div {:className (if (:showcoeff @commerce/app-state) "col-xs-4" "col-xs-4") :style {:text-align "left" :border "1px solid lightgrey" :padding-top "6px" :overflow "hidden" :padding-bottom "6px"}}
                          (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap" :padding-bottom "0px"}} (dom/a {:className "list-group-item" :style {:padding "0px" :border "none" :background "transparent"}}
                          (if (:isbuildingliving item) "Жилой" "Нежилой")
                          ))
                        )
                        (dom/div {:className "col-xs-3" :style {:text-align "center" :border "1px solid lightgrey" :padding-top "6px" :overflow "hidden" :padding-bottom "6px"}}
                          (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap" :padding-bottom (if (> (count (str (:floor item)) ) 0) "0px" "14px")}} (dom/a {:className "list-group-item" :style {:padding "0px" :border "none" :background "transparent"}}
                            (:floor item)
                           ))
                        )

                     )
                  )

                  (dom/div {:className "col-xs-1" :style {:text-align "right" :border "1px solid lightgrey" :padding-top "6px" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap"}} (dom/a {:className "list-group-item" :style {:padding "0px" :border "none" :background "transparent"}}
                    (:totalsquare item)
                    ))
                  )                 
                )
              )
              ) ((keyword (:key (om/get-state owner))) (:object @commerce/app-state))
            )
          )

        )
      )
    )
  )
)
(defn map-analog [idx analog]
  (let [
    lat (get analog "lat")
    lon (get analog "lon")
    address (if (or (nil? (get analog "address")) (= 0 (count (get analog "address")))) (str "(" lat ", " lon ")") (get analog "address")) 
    approach (get analog "approach")
    totalsquare (get analog "totalarea")
    repair (get analog "conditiontype")
    assignment (get analog "assignment")
    entrance (get analog "entrance")
    isbuildingliving (get analog "buildingtype")
    hasshopwindows (get analog "shopwindows")
    houseline (get analog "houseline")
    floor (get analog "storey")
    res {:id idx :index idx :approach approach :totalsquare totalsquare :address address :repair repair :assignment assignment :lat lat :lon lon :isbuildingliving isbuildingliving :hasshopwindows hasshopwindows :entrance entrance :houseline houseline :floor floor}
    ]
    (.log js/console (str res))
    res
  )
)



(defn average
  [numbers]
  (let [
      ;tr1 (.log js/console (nth numbers 0))
    ]
    (/ (apply + numbers) (count numbers))
  )
)


(defn OnGetData [response]
  (let[
    data (get response "data")
    records (map-indexed map-analog data)
    ]
    (swap! commerce/app-state assoc-in [:state] 0)
    (swap! commerce/app-state assoc-in [:object :history] records)
    (.log js/console  (first records) )
  )
)


(defn OnClarifySuccess [response]
  (let[]
    ;(swap! commerce/app-state assoc-in [:object :pricePerMetr] (get response "pricePerMetr"))
    ;(swap! commerce/app-state assoc-in [:object :houseAvrgPrice] (get response "houseAvrgPrice"))
    ;(swap! commerce/app-state assoc-in [:object :regionAvrgPrice] (get response "regionAvrgPrice"))
    ;(swap! commerce/app-state assoc-in [:object :cityAvrgPrice] (get response "cityAvrgPrice"))
    (swap! commerce/app-state assoc-in [:object :data] (get response "data"))
    ;(swap! commerce/app-state assoc-in [:object :analogs] (map-indexed map-analog (get response "analogs")))
    (swap! commerce/app-state assoc-in [:object :calcanalogs] (map-indexed map-analog (get response "calcanalogs")))
    (swap! commerce/app-state assoc-in [:state] 0)
    ;;(.log js/console response)
  )
)

(defn de-map-analog [analog]
   [(:housetype analog) (:price analog) (:totalsquare analog) (:city analog) (:lat analog) (:lon analog) (:roomsnum analog) (:floor analog) (:floors analog) (:address analog) (:district analog) (:repair analog) (:buildyear analog) (:livingsquare analog) (:kitchensquare analog) (:metrodistance analog) (:status analog) (:index analog)]
)



(defn getdata []
  (let [
    ;status (js/parseInt (:statuses (:filter @app-state)))
    ;user (:user (:filter @app-state))
    ]
    (swap! commerce/app-state assoc-in [:state] 1)
    (swap! commerce/app-state assoc-in [:object :history] [])
    ;; (if (not (:approach (:object @commerce/app-state)))
    ;;   (swap! commerce/app-state assoc-in [:object :approach] "аренда")
    ;; )
    (GET (str settings/apipath "history") {
      :handler OnGetData
      :error-handler error-handler
      :format :json
      :response-format :json
      ;:params (:object @commerce/app-state)
    })
  )
)


(defn buildObjectTypeList [data owner]
  (map
    (fn [item]
      (dom/option {:key (:id item)  :value (:name item)} (:name item))
    )
    objecttypes
  )
)

(defn buildParams [data owner]
  (map
    (fn [param]
      (dom/option {:key (:id param)  :value (:id param)} (:name param))
    )
    params
  )
)


(defn buildRepairsList [data owner]
  (map
    (fn [item]
      (dom/option {:key (:id item)  :value (:name item)} (:name item))
    )
    repairs
  )
)

(defn buildAssignmentsList [data owner]
  (map
    (fn [text]
      (let [
        ;tr1 (.log js/console (str  "name=" (:name text) ))
        ]
        (dom/option {:key (:id text) :data-width "100px" :value (:type text)} (:type text))
      )
    )
    assignments
  )
)





(defn setcontrols [value]
  (case value
    46 (setDropDowns)
    49 (go
         (<! (timeout 10))
         (sec/dispatch! "/main")
       )
    44 (swap! commerce/app-state assoc-in [:showmap] -1)
    56 (go
         (<! (timeout 5000))
         (downloadreport)
       )
    57 (go
         (<! (timeout 500))
         (swap! commerce/app-state assoc-in [:isloading] false)
       )
  )
)


(defn initqueue []
  (doseq [n (range 1000)]
    (go ;(while true)
      (take! ch(
        fn [v] (
           ;.log js/console v
           ;(setcalculatedfields) 
           setcontrols v
           
           ;(.log js/console v)  
          )
        )
      )
    )
  )
)


(initqueue)

(defn onMount [data]
  (swap! commerce/app-state assoc-in [:current] 
    "Object detail"
  )
  (set! (.-title js/document) "Price calculation")
  (setcontrols 46)
  (getdata)
  (swap! commerce/app-state assoc-in [:view] 1)
  (set! (.-title js/document) (str "Независимая оценка объектов" (if (> (count (:address (:object @commerce/app-state))) 0) ": ") (:address (:object @commerce/app-state))))
)

(defcomponent addmodalreport [data owner]
  (render [_]
    (dom/div
      (dom/div {:id "reportModal" :className "modal fade" :role "dialog"}
        (dom/div {:className "modal-dialog modal-lg"} 
          ;;Modal content
          (dom/div {:className "modal-content"} 
            (dom/div {:className "modal-header"} 
              (b/button {:type "button" :className "close" :data-dismiss "modal"})
              (dom/h4 {:className "modal-title" :style {:text-align "center"}} "Отчет" )
            )
            (dom/div {:className "modal-body"}

              (dom/div {:className "panel panel-primary"}


                ;(dom/embed {:src "http://5.189.157.176:81/report_valuation.pdf" :width "300" :height "715"})
                (dom/iframe {:src "http://5.189.157.176:81/report_valuation.pdf#zoom=100" :style {:width "870px" :height "610px"} :frameBorder "0"})
              )
            )
            (dom/div {:className "modal-footer"}
              (dom/div {:className "row"}
                (dom/div {:className "col-xs-12" :style {:text-align "center"}}
                  (b/button {:type "button" :className "btn btn-default" :data-dismiss "modal"} "Закрыть")
                )
              )
            )
          )
        )
      )
    )
  )
)



(defn readurl [input]
  (if (not (nil? (.-files input)))
    (let [
      size (.-size (aget (.-files input) 0))
        
      ]
      (.log js/console (str "file size=" (.-size (aget (.-files input) 0))))
      (if (> size 200000)
        (let []
          (swap! commerce/app-state assoc-in [:modalTitle]
            (str "Upload file error")
          )
          ;(swap! socialcore/app-state assoc-in [:modalText] "File size should be less than 200KBytes")
          ;(showmessage)
        )
        (let []
          (swap! commerce/app-state assoc-in [:isloading] true)
          (put! ch 56)
        )  
      )
    )
  )
)


(defcomponent devdetail-page-view [data owner]
  (did-mount [_]
    (let [

      ]

      (onMount data)
      (put! ch 44)
      (jquery
        (fn []
          (let [
            map (:map @commerce/app-state)
            ]
            (-> map
              (.addListener "dblclick"
                (fn [e]
                  (let [
                    size (js/google.maps.Size. 48 48)
                    image (clj->js {:url (str iconBase "green_point.ico") :scaledSize size})

                    marker-options (clj->js {"position" (google.maps.LatLng. (.lat (.. e -latLng)), (.lng (.. e -latLng))) "map" map})
                    marker (js/google.maps.Marker. marker-options)
                    ]
                    (if (not (nil? (:marker @commerce/app-state))) (.setMap (:marker @commerce/app-state) nil))
                    ;(.log js/console (str "LatLng=" (.. e -latLng)))

                    (swap! commerce/app-state assoc-in [:object :lat] (.lat (.. e -latLng)))
                    (swap! commerce/app-state assoc-in [:object :lon] (.lng (.. e -latLng)))
                    (swap! commerce/app-state assoc-in [:marker] marker)
                    (.stopPropagation (.. js/window -event))
                    (.stopImmediatePropagation (.. js/window -event))
                  )
                )
              )
            )
          )
        )
      )
    )
  )



  (did-update [this prev-props prev-state]
    ;(.log js/console "Update happened") 

    ;(put! ch 46)
  )
  (render
    [_]
    (let [
        ;tr1 (.log js/console (clj->js (:param (:object @data))))
      ]
      (dom/div {:style {:padding-top "10px"}}
        ;(om/build commerce/website-view commerce/app-state {})
        (dom/div {:id "header"}
          (dom/div {:className "header_ramka"}
            (dom/table {:className "tab_header" :cellPadding "0" :cellSpacing "0"}
              (dom/tbody
                (dom/tr
                  (dom/td {:style {:text-align "left" :width "300px"}}
                    (dom/div {:id "logo"}
                      (dom/a {:href "https://f-case.ru/"}
                        (dom/img {:src "https://f-case.ru/content/photo/full/20140610101509.png" :alt "Компания «FinCase»" :title "Компания «FinCase»"})
                      )
                    )
                  )
                  (dom/td
                    (dom/div {:style {:border-left "1px solid #ccc" :padding-left "35px" :width "200px" :font-size "18px" :color "#666" :line-height "16px" :position "relative" :top "4px"}} "Инновации на страже Ваших Интересов")
                  )
                  (dom/td {:style {:text-align "right"}}
                    (dom/div {:className "tel_top"}
                      "7 "
                      (dom/span "(499)")
                      " 653-61-53"
                    )
                  )
                )
              )
            )
          )

        )
        (dom/h3 {:style {:text-align "center"}}
          (dom/i {:className "fa fa-cube"})
          (str "История запросов")
        )

        (dom/div {:className "row" :style {:padding-top "10px" :display "block"}}
          ;; (dom/div {:className "col-xs-4" :style {:text-align "center"}}
          ;;   (b/button {:className (if (= (:state @data) 0) "btn btn-primary" "btn btn-primary m-progress") :onClick (fn [e] (getdata))} "Задать фильтр")
          ;; )
          (dom/div {:className "col-xs-4" :style {:text-align "center"}}
            (b/button {:className (if (= (:state @data) 0) "btn btn-primary" "btn btn-primary m-progress") :onClick (fn [e] (sec/dispatch! "/main"))} "На главную")
          )

          ;; (dom/div {:className "col-xs-4" :style {:text-align "center"}}
          ;;   (b/button {:className (if (= (:state @data) 0) "btn btn-primary" "btn btn-primary m-progress") :onClick (fn [e] (openreportdialog))} "Аналитическая отчетность")
          ;; )
        )

        (if (> (count (:history (:object @data))) 0)

          (dom/div {:className "panel panel-primary"}
            (dom/div {:className "panel panel-heading" :style {:text-align "center" :margin-bottom "0px"}}
                "Последние запросы"
            )
            (dom/div {:className "panel panel-body" :style {:padding-top "0px"}}
                (om/build showanalogs-view  data {:state {:key "history"}})
            )
          )
        )

        

        (dom/div {:id "footer" :style {:margin-top "20px"}}
          (dom/div {:id "footer2"}
            (dom/table {:className "footer_tab" :cellSpacing "0" :cellPadding "0" :style {:font "20px 'PT Sans Narrow', sans-serif"}}
              (dom/tbody
                (dom/tr
                  (dom/td {:style {:width "500px" :padding-top "20px"}} "2018 ©" 

                    (dom/b "Компания «FinCase»")
                  )
                )
                (dom/tr
                  (dom/td {:style {:width "500px"}}
                    "Адрес: Москва, ул. Большая Полянка, 2/10 стр1"

                  )
                )
                (dom/tr
                  (dom/td {:style {:width "500px"}}
                    "Тел.: 7 (499) 653-61-53"
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





(sec/defroute devdetail-page "/history" []
  (let[

    ]

    (swap! commerce/app-state assoc-in [:view] 1)
    (om/root devdetail-page-view
             commerce/app-state
             {:target (. js/document (getElementById "app"))})

  )
)


