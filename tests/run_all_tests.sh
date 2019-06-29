#!/bin/bash

SIM_FOLDER="$1"
TEST_FOLDER="$2"
SIMPLE="$3"

red=$(tput setaf 1)
green=$(tput setaf 2)
reset=$(tput sgr0)

BASEDIR="$(dirname "$(readlink -f "$0")")"

function run_tiger () {
    DIR="$PWD"
    cd "$BASEDIR/../src/"
    ./tiger "$@"
    cd "$DIR"
}

if ! [ "$#" -gt 1 ] ||  ! [ -d "$2" ] ; then
  echo "Usage: $0 SIMULATOR_PATH TEST_FOLDER [-simple]" >&2
  exit 1
fi

if [ "$#" -lt 3 ]; then
SIMPLE=0
fi

echo "Running all tests..."

for file in "$TEST_FOLDER"/*; do
  file=$(readlink -f "$file")
  name=$(basename "$file")
  if [[ $file == *.tig ]]; then
    printf "$name ---> "
    LOG_FILE="log.txt"
    OUT_EXT=".output"
    OUTPUT="$file$OUT_EXT"
    EXP_EXT=".expected"
    EXPECTED="$file$EXP_EXT"
    if [ "$SIMPLE" == 0 ]; then
      run_tiger "$file" > "$LOG_FILE"
    else
      run_tiger "$file" -simple > "$LOG_FILE"
    fi

    script -c "$SIM_FOLDER $BASEDIR/../src/a.out" "$OUTPUT" > log.tmp
    sed -i '1d;$d' "$OUTPUT"
    sed -i '$d' "$OUTPUT"

    # head -n -2 "$OUTPUT" > "$OUTPUT"
    # tail -n +2 "$OUTPUT" > "$OUTPUT"

    # compare
    DIFF=$(diff "$OUTPUT" "$EXPECTED")
    if [ "$?" = "0" ] && [ "$DIFF" == "" ]; then
      echo "${green}passed!${reset}"
    else
      echo "${red}failed!${reset}"
    fi
  fi
done

