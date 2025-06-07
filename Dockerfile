FROM clojure:temurin-21-tools-deps-bullseye

ARG TARGETPLATFORM
ENV IN_CONTAINER=true

RUN mkdir -p /app
WORKDIR /app

COPY deps.edn /app
RUN clojure -P
COPY ./src /app/src
COPY ./data /app/data

ADD https://d1ni2b6xgvw0s0.cloudfront.net/v2.x/dynamodb_local_latest.tar.gz /app
RUN mkdir /app/dynamo-local && tar -xzvf dynamodb_local_latest.tar.gz -C /app/dynamo-local
COPY local-db.db /app/dynamo-local/shared-local-instance.db

RUN apt update && apt install -y curl unzip procps
RUN if [ "$TARGETPLATFORM" = "linux/arm64" ]; then \
    curl "https://awscli.amazonaws.com/awscli-exe-linux-aarch64.zip" -o "awscliv2.zip"; \
    elif [ "$TARGETPLATFORM" = "linux/amd64" ]; then \
    curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"; \
    fi

RUN unzip awscliv2.zip && ./aws/install
RUN aws configure set aws_access_key_id DummyToken && aws configure set aws_secret_access_key DummyKey

COPY ./start.sh /app
CMD ./start.sh
