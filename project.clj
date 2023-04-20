(defproject clojure-scraps "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [telegrambot-lib "2.3.0"]
                 [cheshire "5.10.1"]
                 [environ "1.2.0"]
                 [org.clojure/tools.logging "1.2.4"]
                 [com.cognitect.aws/api "0.8.656"]
                 [com.cognitect.aws/endpoints "1.1.12.437"]
                 [com.cognitect.aws/sns "836.2.1323.0"]
                 [com.cognitect.aws/lambda "845.2.1359.0"]
                 [com.cognitect.aws/dynamodb "845.2.1345.0"]
                 [danlentz/clj-uuid "0.1.9"]
                 [org.clojure/core.match "1.0.1"]]
  :plugins [[lein-environ "1.2.0"]]
  :repl-options {:init-ns clojure-scraps.core}
  :profiles {:dev [:project/dev :profiles/dev]
             :test [:project/test :profiles/test]
             ;; only edit :profiles/* in profiles.clj
             :profiles/dev  {}
             :profiles/test {}
             :project/dev {}
             :project/test {}})
