#!/bin/bash

state_bucket="govtool-terraform-state"
lock_table="vva-terraform-locks"
region="eu-west-1"

# aws s3api create-bucket --bucket "$state_bucket" --region "$region"
aws s3 mb s3://"$state_bucket" --region "$region"
aws s3api put-bucket-versioning --bucket "$state_bucket" --versioning-configuration Status=Enabled
aws s3api get-bucket-versioning --bucket "$state_bucket"
aws dynamodb describe-table --table-name "$lock_table"
if [ $? -ne 0 ]; then
	aws dynamodb create-table \
	--region "$region" \
	--table-name "$lock_table" \
	--attribute-definitions AttributeName=LockID,AttributeType=S \
	--key-schema AttributeName=LockID,KeyType=HASH \
	--billing-mode PAY_PER_REQUEST
fi
