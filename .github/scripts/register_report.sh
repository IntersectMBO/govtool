#!/bin/bash

PROJECT_DIR="project"
PROJECT_FILE="projects.txt"

mkdir -p "$PROJECT_DIR"
cp -r gh-pages/* "$PROJECT_DIR" || true

if grep -q "$REPORT_NAME" "$PROJECT_DIR/$PROJECT_FILE"; then
  echo "Project already exists"
  echo "project_exists=true">> $GITHUB_OUTPUT
else
  echo "\n$REPORT_NAME" >> "$PROJECT_DIR/$PROJECT_FILE"
  echo "project_exists=false">> $GITHUB_OUTPUT
fi
