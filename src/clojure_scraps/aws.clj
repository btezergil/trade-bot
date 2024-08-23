(ns clojure-scraps.aws
  (:require [environ.core :refer [env]]
            [clojure.tools.logging :as log]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]))

(def shared-client-params {:region (env :aws-region)
                           :credentials-provider (credentials/basic-credentials-provider {:access-key-id (env :aws-access-key)
                                                                                          :secret-access-key (env :aws-secret-key)})})

(def sns (aws/client (merge {:api :sns} shared-client-params)))

(def lambda (aws/client (merge {:api :lambda} shared-client-params)))

(defn list-sns-actions
  []
  (aws/ops sns))

(defn list-lambda-actions
  []
  (aws/ops lambda))

(defn send-sns
  "Sends a SNS notification"
  [message]
  (log/info "sending an SNS")
  (aws/invoke sns {:op :Publish :request {:Message message :TopicArn "arn:aws:sns:eu-central-1:994976387571:Sns-deneme-topic"}}))