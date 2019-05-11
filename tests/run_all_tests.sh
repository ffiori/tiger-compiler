#!/bin/bash

TEST_FOLDER="$1"
echo "Running all tests..."

for file in "$TEST_FOLDER"/*; do
  printf "$file ---> "
  ANS=$(../src/tiger "$file" | tail -1)
  echo ${ANS}  
done
