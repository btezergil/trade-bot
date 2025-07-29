#!/bin/bash

#GIT_VERSION=$(git rev-list HEAD --count)
#TARGET="clojure-scraps-0.1.${GIT_VERSION}"
CONTAINER_TARGET="clojure-scraps-0.1.1-container"

java -jar target/"$CONTAINER_TARGET"-standalone.jar r
#java -jar target/"$TARGET"-standalone.jar r
#clojure -M -m clojure-scraps.main r
