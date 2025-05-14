#!/bin/bash

if [ -d "gh-pages/$REPORT_NAME" ]; then
    cd gh-pages/$REPORT_NAME
    
    # Count the number of numerical directories
    dir_count=$(find . -maxdepth 1 -type d -regex './[0-9]+' | wc -l)
    
    if [ "$dir_count" -gt 10 ]; then
        # Find the oldest numerical directory
        oldest_dir=$(find . -maxdepth 1 -type d -regex './[0-9]+' | sort -V | head -1)
        if [ -n "$oldest_dir" ]; then
            echo "More than 10 report directories exist. Removing oldest: $oldest_dir"
            rm -rf "$oldest_dir"
        fi
    else
        echo "Only $dir_count report directories exist (threshold: 10). Nothing to remove."
    fi
    
    cd ../../
else
    echo "Report directory does not exist yet"
fi
