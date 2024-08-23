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

(def ddb (aws/client (merge {:api :dynamodb} shared-client-params)))

(defn list-sns-actions
  []
  (aws/ops sns))

(defn list-lambda-actions
  []
  (aws/ops lambda))

(defn list-dynamodb-actions
  []
  (aws/ops ddb))

(defn send-sns
  "Sends a SNS notification"
  [message]
  (log/info "sending an SNS")
  (aws/invoke sns {:op :Publish
                   :request {:Message message
                             :TopicArn "arn:aws:sns:eu-central-1:994976387571:Sns-deneme-topic"}}))

(defn write-to-table
  "Writes the given item to given DynamoDB table"
  [table-name data]

  (log/info (format "writing to table %s" table-name))
  (aws/invoke ddb {:op :PutItem
                   :request {:TableName table-name
                             :Item data}})
  (log/info "write is completed"))

(defn read-from-table
  "Reads the item with id from given table"
  [table-name table-key id]

  (log/info (format "reading from table %s" table-name))
  (aws/invoke ddb {:op :GetItem
                   :request {:TableName table-name
                             :Key {table-key {:S id}}}}))