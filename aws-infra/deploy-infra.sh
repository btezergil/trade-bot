#!/bin/bash

STACK_NAME=genetic-experiment-stack
REGION=eu-central-1
CLI_PROFILE=personal

EC2_INSTANCE_TYPE=t2.micro #gelecekte c6g.xlarge deneyecegiz

# Deploy the CloudFormation template
echo -e "\n\n=========== Deploying main.yml ==========="
aws cloudformation deploy \
  --region $REGION \
  --profile $CLI_PROFILE \
  --stack-name $STACK_NAME \
  --template-file main.yml \
  --no-fail-on-empty-changeset \
  --capabilities CAPABILITY_NAMED_IAM \
  --parameter-overrides \
    EC2InstanceType=$EC2_INSTANCE_TYPE
