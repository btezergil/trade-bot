FROM clojure:temurin-21-tools-deps-bullseye
RUN mkdir -p /app
WORKDIR /app

COPY deps.edn /app
RUN clojure -P
COPY . /app

CMD ["clojure", "-M", "-m", "clojure-scraps.main", "t"]
