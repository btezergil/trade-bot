(ns clojure-scraps.algolab
  (:require [clojure-scraps.bot :as tb]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.tools.logging :as log]
            [envvar.core :as envvar :refer [env]]
            [clj-http.client :as client])
  (:import (javax.crypto Cipher)
           (javax.crypto.spec SecretKeySpec)
           (java.security MessageDigest)
           (org.apache.commons.codec.binary Base64 Hex)))

(def apikey (:algolab-apikey @env))
(def username (:algolab-username @env))
(def password (:algolab-password @env))
(def aes-key (second (str/split apikey #"-")))
(def api-hostname "https://www.algolab.com.tr/api")
(def token (atom nil))
(def checker-hash (atom nil))

(defn encrypt
  [text]
  (let  [text-bytes (.getBytes text "UTF-8")
         key-spec (SecretKeySpec. (Base64/decodeBase64 (.getBytes aes-key "UTF-8")) "AES")
         iv (byte-array 16)
         cipher (Cipher/getInstance "AES/CBC/PKCS5Padding")]
    (.init cipher Cipher/ENCRYPT_MODE key-spec (javax.crypto.spec.IvParameterSpec. iv))
    (Base64/encodeBase64String (.doFinal cipher text-bytes))))

(defn generate-checker
  [endpoint payload]
  (let [data (str apikey api-hostname endpoint payload)
        digest (MessageDigest/getInstance "SHA256")]
    (Hex/encodeHexString (.digest digest (.getBytes data "UTF-8")))))

(defn login
  []
  (let [encrypted-username (encrypt username)
        encrypted-passwd (encrypt password)
        payload (json/write-str {:Username encrypted-username
                                 :Password encrypted-passwd})
        response (client/post (str api-hostname "/api/LoginUser")
                              {:content-type :json
                               :body payload
                               :headers {"APIKEY" apikey}})
        body (json/read-str (:body response) :key-fn keyword)]
    (if (:success body)
      (do (reset! token (-> body :content :token))
          (log/info "Login to ALGOLAB succeeded, token received, please insert SMS code next.")
          (tb/message-to-me "Please send the SMS code via /algolab-sms command."))
      (log/warn "Login to ALGOLAB failed, no token received. Response: " response))))

(defn login-sms-code
  [sms-code]
  (let [encrypted-token (encrypt @token)
        encrypted-passwd (encrypt sms-code)
        payload (json/write-str {:token encrypted-token
                                 :Password encrypted-passwd})
        response (client/post (str api-hostname "/api/LoginUserControl")
                              {:content-type :json
                               :body payload
                               :headers {"APIKEY" apikey}})
        body (json/read-str (:body response) :key-fn keyword)]
    (if (:success body)
      (do (reset! checker-hash (-> body :content :hash))
          (log/info "Login to ALGOLAB succeeded, hash received.")
          (tb/message-to-me "Login to ALGOLAB succeeded."))
      (log/warn "Login to ALGOLAB failed, no hash received. Response: " response))))

(defn session-refresh
  []
  (let [checker (generate-checker "/SessionRefresh" "")
        response (client/post (str api-hostname "/api/SessionRefresh")
                              {:content-type :json
                               :body (json/write-str {})
                               :headers {"APIKEY" apikey
                                         "Checker" checker
                                         "Authorization" @checker-hash}})]
    (if (= 200 (:status response))
      (log/info "Session refreshed successfully.")
      (log/warn "Session failed to refresh. Response: " response))))

(defn equity-info
  [equity]
  (let [payload (json/write-str {:symbol equity})
        checker (generate-checker "/GetEquityInfo" payload)
        response (client/post (str api-hostname "/api/GetEquityInfo")
                              {:content-type :json
                               :body payload
                               :headers {"APIKEY" apikey
                                         "Checker" checker
                                         "Authorization" @checker-hash}})
        body (json/read-str (:body response) :key-fn keyword)]
    (print body)))

(defn candle-data
  "Period in minutes for candles, 250 candles returned"
  [equity & {:keys [period]
             :or {period "60"}}]
  (let [payload (json/write-str {:symbol equity
                                 :period period})
        checker (generate-checker "/GetCandleData" payload)
        response (client/post (str api-hostname "/api/GetCandleData")
                              {:content-type :json
                               :body payload
                               :headers {"APIKEY" apikey
                                         "Checker" checker
                                         "Authorization" @checker-hash}})
        body (json/read-str (:body response) :key-fn keyword)
        candles (:content body)]
    (if (:success body)
      (print candles)
      (log/warn "Failed to get candles. Response: " response))))

; TODO: emir acma fonksiyonlarini ekle ve test et

;(login)
;(login-sms-code "409455")
;(session-refresh)
;(equity-info "ISCTR")
;(candle-data "ISCTR" :period "120")
;(reset! token "86d38669-bf87-457e-9c8c-6d4da7d5")
;@token



