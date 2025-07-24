FROM clojure:temurin-21-tools-deps-bullseye

ARG TARGETPLATFORM
ENV IN_CONTAINER=true

RUN mkdir -p /app
WORKDIR /app

COPY deps.edn /app
RUN clojure -P
COPY ./src /app/src
COPY ./data /app/data

COPY ./start.sh /app
CMD ./start.sh
