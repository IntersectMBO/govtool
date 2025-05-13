#!/bin/bash

if [ -d "gh-pages/$REPORT_NAME" ]; then
    cd gh-pages/$REPORT_NAME
    # Find the oldest numerical directory
    oldest_dir=$(find . -maxdepth 1 -type d -regex './[0-9]+' | sort -n | head -1)
    if [ -n "$oldest_dir" ]; then
        echo "Removing oldest report directory: $oldest_dir"
        rm -rf "$oldest_dir"
    else
        echo "No report directories found to remove"
    fi
    cd ../../
else
    echo "Report directory does not exist yet"
fi
