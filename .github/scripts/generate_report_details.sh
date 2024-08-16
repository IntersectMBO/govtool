#!/bin/bash

if [[ ! -d "gh-pages/$REPORT_NAME" ]]; then
  latest_number=0
else
  gh_pages_content=$(ls "gh-pages/$REPORT_NAME/")
  latest_number=$(echo "$gh_pages_content" | grep -Eo '[0-9]+' | sort -nr | head -n 1)
fi

echo "report_number=$((latest_number+1))" >> $GITHUB_OUTPUT
echo "report_url=https://$(dirname "$GH_PAGES").github.io/$(basename "$GH_PAGES")/$REPORT_NAME" >> $GITHUB_OUTPUT

