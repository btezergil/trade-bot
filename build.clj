(ns build
  (:require [clojure.tools.build.api :as b]
            [envvar.core :as envvar :refer [env]]))

(def lib 'clojure-scraps)
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def container-version "0.1.1-container")
(def class-dir "target/classes")
(def uber-file
  (if-not (:in-container @env)
    (format "target/%s-%s-standalone.jar" (name lib) version)
    (format "target/%s-%s-standalone.jar" (name lib) container-version)))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis @basis
                  :ns-compile '[clojure-scraps.main]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis @basis
           :main 'clojure-scraps.main}))
