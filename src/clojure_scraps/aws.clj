(ns clojure-scraps.aws
  (:require [environ.core :refer [env]]
            [clojure.tools.logging :as log]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]
            [cheshire.core :as cheshire]))

(def shared-client-params {:region (env :aws-region), :credentials-provider (credentials/basic-credentials-provider {:access-key-id (env :aws-access-key), :secret-access-key (env :aws-secret-key)})})

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

