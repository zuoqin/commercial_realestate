(ns commerce.server
  (use (incanter core charts excel))
  (use dk.ative.docjure.spreadsheet)
  (:require [clojure.java.io :as io]
            [compojure.core :refer [ANY GET PUT POST DELETE defroutes]]
            [compojure.route :refer [resources]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.middleware.logger :refer [wrap-with-logger]]
            [environ.core :refer [env]]
            [clj-http.client :as client]
            [clojure.data.json :as json]

            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clj-time.core :as t]

            [clojure.java.io :as io]
            
            [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

(def custom-formatter (f/formatter "dd/MM/yyyy"))
;;(def apipath "http://10.20.35.21:3000/")
(def apipath "http://localhost:3000/")
;(def xlsdir "/root/dev/FinCase/")
(def xlsdir "/mnt/admin/")

(def bloombergportdir "c:/DEV/output/")
;(def imagepath "C:/DEV/clojure/sberpb/sberstatic/resources/public/img/tradeidea/")

;; (defn copy-file [source-path dest-path]
;;   (io/copy (io/file source-path) (io/file dest-path)))


;; (defn on-upload-image [request]
;;   (let [tr1 (println request)

;;       filepath (.getAbsolutePath (:tempfile (:file (:params request))))
;;       newfilename (:filename (:file (:params request)))
;;       ;tr1 (println (str "Path to file: " filepath " File name: " newfilename))

;;       tr1 (copy-file (.getAbsolutePath (:tempfile (:file (:params request)))) (str imagepath newfilename))

;;       tr1 (io/delete-file filepath)
;;     ]
;;     {:body { :location (str "/" newfilename)  }}
;;   )
;; )


;; (defn on-save-html [request]
;;   (let [;tr1 (println request)
;;         ;url (str apipath "api/syssetting")
;;         tr1 1 ;(tradeidea/update-tradeidea request)
;;     ]
;;      (response/found "/tradeidea/1")
;;   )
;; )


(defn sort-portfs-by-name [portf1 portf2] 
  (let [
        name1 (first portf1)
        name2 (first portf2)
        ]
    
    (if (<  (compare name1  name2) 0)
    true
    false)
  )
)

(defn comp-positions [position1 position2]
  (let [
    ;tr1 (println (str "pos1= " position1 " pos2= " position2))
    ]
    (if (or
         (> (compare (:assettype position1) (:assettype position2))  0)
         (and (= (compare (:assettype position1) (:assettype position2)) 0) (> (compare (:currency position1) (:currency position2)) 0))
         (and (= (compare (:assettype position1) (:assettype position2)) 0 ) (= (:currency position1) (:currency position2)) (> (:usdvalue position1) (:usdvalue position2)))
      )        
        true
        false
    )
  )
)

(defn comp-deals
  [deal1 deal2]
  (let [;tr1 (println (str "deal1= " deal1 " deal2= " deal2))
            ]
    (if (or
         (> (:date deal1) (:date deal2))
         (and (= (:date deal1) (:date deal2)) (> (compare (:security deal1) (:security deal2)) 0 ))
          ;(> (c/to-long (f/parse custom-formatter (:date deal1))) (c/to-long (f/parse custom-formatter (:date deal2))))
          ;(and (= (c/to-long (f/parse custom-formatter (:date deal1))) (c/to-long (f/parse custom-formatter (:date deal2)))) (> (compare (:security deal1) (:security deal2)) 0 ))
          )
          true
          false
        )
  )
)

(defn map-deal [deal]
  (let [
        ;tr1 (println (str deal))
        trans (loop [result [] trans (get deal "transactions") ]
                (if (seq trans) 
                  (let [
                        tran (first trans)
                        ;tr1 (.log js/console (str "tran: " tran ))
                        ]
                    (recur (conj result {:security (get deal "security") :date (get tran "date" ) :direction (get tran "direction") :nominal (get tran "nominal") :wap (get tran "wap") :wapusd (get tran "wapusd") :waprub (get tran "waprub")})
                         (rest trans))
                  )
                  result)
        )        
        result trans
        
    ]
    ;

    result
  )
)


(defn get-analogs [params]
  (let [

    newpositions []
     


    ]

    "Success"
  )
)

(defn create-excel-report []
  (let [

    newpositions [{:address "address 1" :area 50.1 :floor 3 :floors 5 :year 1987 :price 14300000}
      {:address "address 2" :area 45.1 :floor 7 :floors 9 :year 1989  :price 10890789}
     {:address "address 3" :area 56.9 :floor 12 :floors 23 :year 2007 :price 21098560}
     ]
    ]
    (save-xls ["sheet1" (dataset [:address :area :floor :floors :year :price] newpositions)] (str xlsdir "report.xlsx"))
    "Success"
    ;(first newpositions)
  )
)


(defroutes routes
  (GET "/" _
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (io/input-stream (io/resource "public/index.html"))})


  (GET "/calcprice/:area" [area]
    (let [
          analogs  (->> (load-workbook-from-resource "book.xlsx")
     (select-sheet "sell")
     (select-columns {:A :city, :B :district, :E :address, :P :totalsquare, :Q :parent, :X :repair, :Y :assignment, :AC :price, :AG :source}))
        s (read-string area)
        filtered (filter (fn [x] (if (and (> (+ (:parent x) 0.1) s) (< (- (:parent x) 0.1) s)) true false)) analogs)
        price (case (int (Math/floor s))  233 55757000 23 1888000 171 21906000 85 14249000 22 2165000 351 49585000 35 9665000 40 2705000 2705000)
      
    ]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (json/write-str {:analogs filtered :data price})}
    )
  )
  (GET "/report" []
    (let [
          file (create-excel-report)
    ]
    {:status 200 :headers {"Content-Type" "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", "Content-Disposition" (str "attachment;filename=" "report" ".xlsx") } :body (io/input-stream (str xlsdir "report" ".xlsx") )}
    )
  )
  ;; (GET "/clientexcel/:client" [client]
  ;;   (let [
  ;;         file (create-client-report client)
  ;;   ]
  ;;   {:status 200 :headers {"Content-Type" "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", "Content-Disposition" (str "attachment;filename=" client ".xlsx")} :body (io/input-stream (str xlsdir client ".xlsx") )}
  ;;   )
  ;; )
  (GET "/clientbloombergportf/:client" [client]
    (let [
      url (str apipath "api/bloomberg_portf?portf=" client)
      ;tr1 (println (str "Trying to get securities"))
      response (client/get url {:headers {"authorization" (str "Bearer " (env :token))}})


    ]
    {:status 200 :headers {"Content-Type" "text/plain;charset=utf-8", "Content-Disposition" (str "attachment;filename=" client ".txt")} :body (io/input-stream (str bloombergportdir client ".txt") )}
    )
  )
  (GET "/clientbloombergtrans/:client" [client]
    (let [
      url (str apipath "api/bloomberg_trans?portf=" client)
      response (client/get url {:headers {"authorization" (str "Bearer " (env :token))}})
    ]
    {:status 200 :headers {"Content-Type" "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;", "Content-Disposition" (str "attachment;filename=" client "_trans.xlsx") } :body (io/input-stream (str xlsdir client "_trans.xlsx") )}
    )
  )



  ;;(POST "/tradeidea/imageupload" request (on-upload-image request) )
  ;;(POST "/tradeidea/" request (on-save-html request) )


  (resources "/")
)


(def http-handler
  (-> routes
      (wrap-defaults api-defaults)
      wrap-with-logger
      wrap-gzip))

(defn -main [& [port]]
  (let [
    port (Integer. (or port (env :port) 10555))
    ;tr1 (println (str "token=" (env :token)))
    ]
    (run-jetty http-handler {:port port :join? false})
  )
)
