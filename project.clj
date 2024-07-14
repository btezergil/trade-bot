(defproject clojure-scraps "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :dependencies [[org.clojure/clojure "1.12.0-alpha4"]
                 [telegrambot-lib "2.3.0"]
                 [cheshire "5.10.1"]
                 [environ "1.2.0"]
                 [org.clojure/tools.logging "1.2.4"]
                 [com.cognitect.aws/api "0.8.656"]
                 [com.cognitect.aws/endpoints "1.1.12.437"]
                 [com.cognitect.aws/sns "836.2.1323.0"]
                 [com.cognitect.aws/lambda "845.2.1359.0"]
                 [com.cognitect.aws/dynamodb "845.2.1345.0"]
                 [com.cognitect.aws/secretsmanager "845.2.1345.0"]
                 [danlentz/clj-uuid "0.1.9"]
                 [clj-http "3.12.3"]
                 [org.ta4j/ta4j-core "0.15"]]
  :plugins [[lein-environ "1.2.0"]]
  :repl-options {:init-ns clojure-scraps.core}
  :profiles {:dev [:project/dev :profiles/dev]
             :test [:project/test :profiles/test]
             ;; only edit :profiles/* in profiles.clj
             :profiles/dev  {}
             :profiles/test {}
             :project/dev {}
             :project/test {}})
