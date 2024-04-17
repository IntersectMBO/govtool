#!/bin/bash

REPORT_NUMBER="$1"

mkdir -p build
cp -r allure-history/*[^index.html] build/
allure_report_path=$(basename "$GH_PAGES")

cat <<EOF > build/index.html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta http-equiv="refresh" content="0; url=/$allure_report_path/$REPORT_NAME/$REPORT_NUMBER">
    <title>Redirecting...</title>
</head>
</html>
EOF
