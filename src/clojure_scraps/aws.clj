(ns clojure-scraps.aws
  (:require [environ.core :refer [env]]
            [clojure.tools.logging :as log]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]))

(def sns (aws/client {:api :sns
                      :region (env :aws-region)
                      :credentials-provider (credentials/basic-credentials-provider {:access-key-id (env :aws-access-key)
                                                                                     :secret-access-key (env :aws-secret-key)
                                                                                     })}))

(defn list-sns-actions
  []
  (aws/ops sns))

(defn send-sns
  "Sends a SNS notification"
  [message]
  (log/info "sending a SNS")
  (aws/invoke sns {:op :Publish :request {:Message message :TopicArn "arn:aws:sns:eu-central-1:994976387571:Sns-deneme-topic"}}))