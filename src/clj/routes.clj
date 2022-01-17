(ns routes
  (:require
    [reitit.ring :as reitit-ring]
    [reitit.coercion.malli]
    [reitit.swagger :as reitit-swagger]
    [reitit.swagger-ui :as reitit-swagger-ui]
    [reitit.ring.malli]
    [reitit.ring.coercion :as reitit-coercion]
    [reitit.ring.middleware.muuntaja :as reitit-muuntaja]
    [reitit.ring.middleware.exception :as reitit-exception]
    [reitit.ring.middleware.parameters :as reitit-parameters]
    [reitit.ring.middleware.dev]
    [muuntaja.core :as mu-core]
    [ring.util.http-response :as r]
    ))

(def my-version {:version 0.1 :date "2021-11-21"})

(defn handler
  "Handler."
  [routes]
  (->
    (reitit-ring/ring-handler
      (reitit-ring/router routes {
                                  ; Use this to debug middleware handling:
                                  ;:reitit.middleware/transform reitit.ring.middleware.dev/print-request-diffs
                                  :data {:muuntaja mu-core/instance
                                         :coercion (reitit.coercion.malli/create
                                                     {;; set of keys to include in error messages
                                                      :error-keys #{:type :coercion :in #_:schema #_:value #_:errors :humanized #_:transformed}
                                                      ;; validate request & response
                                                      :validate true
                                                      ;; top-level short-circuit to disable request & response coercion
                                                      :enabled true
                                                      ;; strip-extra-keys (effects only predefined transformers)
                                                      :strip-extra-keys true
                                                      ;; add/set default values
                                                      :default-values true
                                                      ;; malli options
                                                      :options nil}) ;; malli
                                         :middleware [;; swagger feature
                                                      reitit-swagger/swagger-feature
                                                      ;; query-params & form-params
                                                      reitit-parameters/parameters-middleware
                                                      ;; content-negotiation
                                                      reitit-muuntaja/format-negotiate-middleware
                                                      ;; encoding response body
                                                      reitit-muuntaja/format-response-middleware
                                                      ;; exception handling
                                                      (reitit-exception/create-exception-middleware
                                                        (merge
                                                          reitit-exception/default-handlers
                                                          {::reitit-exception/wrap (fn [handler ^Exception e request]
                                                                                     (handler e request))}))
                                                      ;; decoding request body
                                                      reitit-muuntaja/format-request-middleware
                                                      ;; coercing response bodys
                                                      reitit-coercion/coerce-response-middleware
                                                      ;; coercing request parameters
                                                      reitit-coercion/coerce-request-middleware]}})
      (reitit-ring/routes
        (reitit-ring/redirect-trailing-slash-handler)
        (reitit-ring/create-file-handler {:path "/", :root "target/shadow/dev/resources/public"})
        (reitit-ring/create-resource-handler {:path "/"})
        (reitit-ring/create-default-handler)))))

(defn routes [env]
  ["/my4clojure"
   [["/swagger.json"
     {:get {:no-doc true
            :swagger {:info {:title "todo-api"
                             :description "todo"}
                      :tags [{:name "todo", :description "todo"}
                             {:name "health" :description "health api"}]}
            :handler (reitit-swagger/create-swagger-handler)}}]
    ["/api-docs/*"
     {:get {:no-doc true
            :handler (reitit-swagger-ui/create-swagger-ui-handler
                       {:config {:validatorUrl nil}
                        :url "/vega/swagger.json"})}}]
    ["/api"
     ["/version" {:swagger {:tags ["version"]}}
      ["" {:get {:summary "get version info"
                 :responses {200 {:description "version info"}}
                 :handler (constantly {:status 200, :body my-version})}}]]
     ]]])
