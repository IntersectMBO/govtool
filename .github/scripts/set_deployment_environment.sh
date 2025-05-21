#!/bin/bash

DEPLOYMENT=${DEPLOYMENT:-"govtool.cardanoapi.io/api"}
GROUP_NAME="qa"

if [[ "$DEPLOYMENT" == "preview.gov.tools" || "$DEPLOYMENT" == "be.preview.gov.tools" || "$DEPLOYMENT" == "z6b8d2f7a-zca4a4c45-gtw.z937eb260.rustrocks.fr" ]]; then
    GROUP_NAME="preview"
elif [[ "$DEPLOYMENT" == "gov.tools" || "$DEPLOYMENT" == "be.gov.tools" ]]; then
    GROUP_NAME="mainnet"
elif [[ "$DEPLOYMENT" == "p80-z78acf3c2-zded6a792-gtw.z937eb260.rustrocks.fr" || "$DEPLOYMENT" == "z78acf3c2-z5575152b-gtw.z937eb260.rustrocks.fr" ]]; then
    GROUP_NAME="dev"
else
    GROUP_NAME="qa"
fi

# Set environment variable for GitHub Actions
echo "GROUP_NAME=${GROUP_NAME}" >>$GITHUB_ENV
echo "group_name=${GROUP_NAME}" >>$GITHUB_OUTPUT
echo "Setting deployment environment to: ${GROUP_NAME}"
