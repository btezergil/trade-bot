FROM clojure:temurin-21-tools-deps-bullseye

ARG TARGETPLATFORM
ENV IN_CONTAINER=true
ENV OUT_FILE_PATH=/app/out/

WORKDIR /app

COPY deps.edn build.clj /app/
COPY ./src ./src
COPY ./data ./data
RUN clojure -T:build uber

COPY ./start.sh .
ENTRYPOINT ["./start.sh"]
