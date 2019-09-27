#!/bin/bash

# Phillip Mates
# Compilers Spring 2011
# Lexer Assignment

# Compare expected lexer output (*.res) with your lexer output (*.out)

USAGE() {
cat << EOF
usage: $0 options

Compare expected lexer output (*.res) with your lexer output (*.out)

Assumes there are 100 test cases and files are named *.txt.res and *.txt.out

OPTIONS:
   -h           Show this message
   -p DIR       Specify path that contains files
   -v           Verbose
EOF
}

VERBOSE=0
DIR="."
while getopts ":p:v" OPTIONS;
do
  case $OPTIONS in
    v ) VERBOSE=1;;
    p ) DIR=$OPTARG;;
    h ) USAGE;;
   \? ) USAGE
         exit;;
    * ) USAGE
        exit;;
  esac
done

DIR="lextest"
cd $DIR
echo "Testing in $DIR"

COUNTER=100
for i in {1..100}
do
    X=""
    X=$(diff "$i.txt.res" "$i.txt.out")
    # If the diff isn't empty
    if [ -n "$X" ]; then
        # Check if both contain ERROR
        OUT=$(grep ERROR "$i.txt.res")
        RES=$(grep ERROR "$i.txt.out")
        if [ -z "$OUT" -o -z "$RES" ]; then
            # non-empty diff where expected and result outputs
            # don't both contain errors
            let COUNTER=COUNTER-1
            if [ "$VERBOSE" -eq 1 ]; then
                echo "Case $i fails"
                cat "$i.txt.res"
                #diff "$i.txt.res" "$i.txt.out"
            fi
        else
            if [ "$VERBOSE" -eq 1 ]; then
                echo "$i.txt.res and $i.txt.out both contain errors"
            fi
        fi
    fi
done

echo Your Lexer passes $COUNTER tests
