#!/bin/bash
java -Djava.library.path=./dynamo-local/DynamoDBLocal_lib -jar dynamo-local/DynamoDBLocal.jar -sharedDb -dbPath ./dynamo-local &
clojure -M -m clojure-scraps.main r
