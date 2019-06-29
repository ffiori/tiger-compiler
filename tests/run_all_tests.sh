#!/bin/bash

SIM_FOLDER="$1"
TEST_FOLDER="$2"
SIMPLE="$3"

if ! [ "$#" -gt 1 ] ||  ! [ -d "$2" ] ; then
  echo "Usage: $0 SIM_FOLDER TEST_FOLDER [-simple]" >&2
  exit 1
fi

if [ "$#" -lt 3 ]; then
SIMPLE=0
fi

echo "Running all tests..."

for file in "$TEST_FOLDER"/*; do
  if [[ $file == *.tig ]]; then
    printf "$file ---> "
    LOG_FILE="log.txt"
    OUT_EXT=".output"
    OUTPUT="$file$OUT_EXT"
    EXP_EXT=".expected"
    EXPECTED="$file$EXP_EXT"
    if [ "$SIMPLE" == 0 ]; then
      ../src/tiger "$file" > "$LOG_FILE"
    else
      ../src/tiger "$file" -simple > "$LOG_FILE"
    fi

    script -c "$SIM_FOLDER a.out" "$OUTPUT" > log.tmp
    sed -i '1d;$d' "$OUTPUT"
    sed -i '$d' "$OUTPUT"

    # head -n -2 "$OUTPUT" > "$OUTPUT"
    # tail -n +2 "$OUTPUT" > "$OUTPUT"

    # compare
    DIFF=$(diff "$OUTPUT" "$EXPECTED")
    if [ "$DIFF" == "" ]; then
      echo "passed!"
    else
      echo "failed!"
    fi
  fi
done

