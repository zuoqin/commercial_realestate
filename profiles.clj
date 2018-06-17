{
 :uberjar  {
   :env { :token "eyJhbGciOiJub25lIiwidHlwIjoiSldUIn0.eyJpc3MiOiJ6dW9xaW4iLCJleHAiOjE1MDM0NzQ4NzQsImlhdCI6MTUwMzM4ODQ3NH0."}
   :source-paths ^:replace ["src/clj" "src/cljc"]
   :prep-tasks ["compile" ["cljsbuild" "once" "min"]]
   :hooks []
   :omit-source true
   :aot :all
 }
 :dev  {
   :env { :token "eyJhbGciOiJub25lIiwidHlwIjoiSldUIn0.eyJpc3MiOiJ6dW9xaW4iLCJleHAiOjE1MDM0NzQ4NzQsImlhdCI6MTUwMzM4ODQ3NH0."}
   :dependencies [[figwheel "0.5.4-4"]
                             [figwheel-sidecar "0.5.4-4"]
                             [com.cemerick/piggieback "0.2.1"]
                             [org.clojure/tools.nrepl "0.2.12"]]

   :plugins [[lein-figwheel "0.5.4-4"]
             [lein-doo "0.1.6"]
             [cider/cider-nrepl "0.17.0"]
             ]

   :source-paths ["dev"]
   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
 }
 :test {:env {:token "eyJhbGciOiJub25lIiwidHlwIjoiSldUIn0.eyJpc3MiOiJ6dW9xaW4iLCJleHAiOjE1MDM0NzQ4NzQsImlhdCI6MTUwMzM4ODQ3NH0."}}
}
