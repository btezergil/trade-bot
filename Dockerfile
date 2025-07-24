FROM clojure:temurin-21-tools-deps-bullseye

ARG TARGETPLATFORM
ENV IN_CONTAINER=true

WORKDIR /app

COPY deps.edn .
RUN clojure -P
COPY ./src ./src
COPY ./data ./data

COPY ./start.sh .
CMD ./start.sh
