#!/bin/bash

PROJECT_DIR="project"
PROJECT_FILE="projects.txt"

mkdir -p "$PROJECT_DIR"
cp -r gh-pages/* "$PROJECT_DIR" || true

# Create group directory if it doesn't exist
mkdir -p "$PROJECT_DIR/$GROUP_NAME"
# Create project file if it doesn't exist
touch "$PROJECT_DIR/$GROUP_NAME/$PROJECT_FILE"

if grep -q "$REPORT_NAME" "$PROJECT_DIR/$GROUP_NAME/$PROJECT_FILE"; then
  echo "Project already exists"
  echo "project_exists=true">> $GITHUB_OUTPUT
else
  echo -e "\n$REPORT_NAME" >> "$PROJECT_DIR/$GROUP_NAME/$PROJECT_FILE"
  echo "project_exists=false">> $GITHUB_OUTPUT
fi
