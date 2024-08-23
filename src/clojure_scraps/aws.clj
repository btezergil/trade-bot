(ns clojure-scraps.aws
  (:require [envvar.core :as envvar :refer [env]]
            [clojure.tools.logging :as log]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]
            [cheshire.core :as cheshire]))

(def shared-client-params {:region (:aws-region @env)
                           :credentials-provider (credentials/basic-credentials-provider {:access-key-id (:aws-access-key @env)
                                                                                          :secret-access-key (:aws-secret-key @env)})})

(def sns (aws/client (merge {:api :sns} shared-client-params)))
(def lambda (aws/client (merge {:api :lambda} shared-client-params)))
(def sec-man (aws/client (merge {:api :secretsmanager} shared-client-params)))

(defn list-secrets [] (aws/invoke sec-man {:op :ListSecrets}))

(defn get-secrets
  "Get secrets from AWS Secrets Manager"
  []
  (log/info (str sec-man))
  (let [response (aws/invoke sec-man {:op :GetSecretValue, :request {:SecretId "arn:aws:secretsmanager:eu-central-1:994976387571:secret:trade-bot-secrets-qNBE6o"}})
        secrets (:SecretString response)]
    (cheshire/parse-string secrets true)))

(defn send-sns
  "Sends a SNS notification"
  [message]
  (log/info "sending an SNS")
  (aws/invoke sns {:op :Publish, :request {:Message message, :TopicArn "arn:aws:sns:eu-central-1:994976387571:Sns-deneme-topic"}}))

