#!/bin/bash -xe
source /home/ec2-user/.bash_profile
[ -d "/home/ec2-user/app/aws-infra/release" ] && \
cd /home/ec2-user/app/aws-infra/release && \
npm stop
