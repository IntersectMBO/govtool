#!/bin/bash

DEPLOYMENT=${DEPLOYMENT:-"govtool.cardanoapi.io/api"}
GROUP_NAME="qa"

if [[ "$DEPLOYMENT" == "be.preview.gov.tools" || "$DEPLOYMENT" == "z6b8d2f7a-zca4a4c45-gtw.z937eb260.rustrocks.fr" ]]; then
    GROUP_NAME="preview"
elif [[ "$DEPLOYMENT" == "be.gov.tools" ]]; then
    GROUP_NAME="mainnet"
else
    GROUP_NAME="qa"
fi

# Set environment variable for GitHub Actions
echo "GROUP_NAME=${GROUP_NAME}" >> $GITHUB_ENV
echo "Setting deployment environment to: ${GROUP_NAME}"
