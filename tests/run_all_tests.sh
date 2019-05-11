#!/bin/bash

TEST_FOLDER="$1"
VERBOSITY="$2"

if ! [ "$#" -gt 0 ] || ! [ -d "$1" ] ; then
  echo "Usage: $0 DIRECTORY [VERBOSITY]" >&2
  exit 1
fi

if ! [ "$#" -gt 1 ]; then
VERBOSITY=1
fi

echo "Running all tests..."

for file in "$TEST_FOLDER"/*; do
  printf "$file ---> "
  ANS=$(../src/tiger "$file" | tail -$VERBOSITY)
  echo ${ANS}  
done


