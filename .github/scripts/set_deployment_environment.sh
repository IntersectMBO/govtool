#!/bin/bash

DEPLOYMENT=${DEPLOYMENT:-"govtool.cardanoapi.io/api"}
GROUP_NAME="qa"
HOST_URL=$HOST_URL
BASE_URL=$BASE_URL

# Determine if running from frontend (HOST_URL is set) or backend (BASE_URL is set)
if [[ "$HOST_URL" != null && -n "$HOST_URL" ]]; then
    if [[ "$DEPLOYMENT" == "preview.gov.tools" ]]; then
        GROUP_NAME="preview"
    elif [[ "$DEPLOYMENT" == "gov.tools" ]]; then
        GROUP_NAME="mainnet"
    else
        GROUP_NAME="qa"
    fi
elif [[ -n "$BASE_URL" ]]; then
    if [[ "$DEPLOYMENT" == "be.preview.gov.tools" || "$DEPLOYMENT" == "z6b8d2f7a-zca4a4c45-gtw.z937eb260.rustrocks.fr" ]]; then
        GROUP_NAME="preview"
    elif [[ "$DEPLOYMENT" == "be.gov.tools" ]]; then
        GROUP_NAME="mainnet"
    else
        GROUP_NAME="qa"
    fi
else
    echo "Warning: Neither HOST_URL nor BASE_URL is properly set. Using default QA environment."
    GROUP_NAME="qa"
fi

# Set environment variable for GitHub Actions
echo "GROUP_NAME=${GROUP_NAME}" >>$GITHUB_ENV
echo "Setting deployment environment to: ${GROUP_NAME}"
