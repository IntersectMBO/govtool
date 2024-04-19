#!/bin/bash

PROJECT_DIR="project"
PROJECT_FILE="projects.txt"

mkdir -p "$PROJECT_DIR"
cp -r gh-pages/* "$PROJECT_DIR" || true

if grep -q "$REPORT_NAME" "$PROJECT_DIR/$PROJECT_FILE"; then
  echo "Project already exists"
  echo "::set-output name=project_exists::true"
else
  echo "$REPORT_NAME" >> "$PROJECT_DIR/$PROJECT_FILE"
  echo "::set-output name=project_exists::false" 
fi
