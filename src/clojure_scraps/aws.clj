(ns clojure-scraps.aws
  (:require [environ.core :refer [env]]
            [clojure.tools.logging :as log]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]
            [cheshire.core :as cheshire]))

(def shared-client-params {:region (env :aws-region), :credentials-provider (credentials/basic-credentials-provider {:access-key-id (env :aws-access-key), :secret-access-key (env :aws-secret-key)})})

(def sns (aws/client (merge {:api :sns} shared-client-params)))
(def lambda (aws/client (merge {:api :lambda} shared-client-params)))
(def ddb (aws/client (merge {:api :dynamodb} shared-client-params)))
;(def ddb (aws/client {:api :dynamodb, :endpoint-override {:protocol :http, :hostname "localhost", :port 8000}}))
(def sec-man (aws/client (merge {:api :secretsmanager} shared-client-params)))

(aws/validate-requests ddb true)

;(aws/invoke ddb
;            {:op :CreateTable,
;             :request {:TableName "evolution-v1",
;                       :KeySchema [{:AttributeName "id", :KeyType "HASH"}],
;                       :AttributeDefinitions [{:AttributeName "id", :AttributeType :S}],
;                       :ProvisionedThroughput {:ReadCapacityUnits 1, :WriteCapacityUnits 1}}})
;
;(aws/invoke ddb {:op :Scan :request {:TableName "evolution-v1"}})


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

(defn write-to-table
  "Writes the given item to given DynamoDB table"
  [table-name data]
  (log/debug (format "writing %s to table %s" data table-name))
  (let [response (aws/invoke ddb {:op :PutItem, :request {:TableName table-name, :Item data}})]
    (log/debug (format "write response: %s" response))))

(defn read-from-table
  "Reads the item with id from given table"
  [table-name table-key id]
  (log/debug (format "reading from table %s" table-name))
  (let [response (aws/invoke ddb {:op :GetItem, :request {:TableName table-name, :Key {table-key {:S id}}}})]
    (log/debug (format "read response: %s" response))
    response))
