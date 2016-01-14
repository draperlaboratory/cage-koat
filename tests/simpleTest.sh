#!/bin/bash
# Simple diff-based check for making sure we don't break koat in obvious ways

GOLDEN=gold
CURRENT=out
DEBUG=false

compareFile(){
    goldenFile=$1
    fname=$(basename $goldenFile)
    this=$CURRENT/$fname
    if $DEBUG; then
        echo "diff $goldenFile $this"
    else
        diff $goldenFile $this
        if [ $? -ne 0 ]; then
            echo "$goldenFile and $this differ."
            return 1
        fi
    fi
    return 0
}

wrong=0
if [ -e different ]; then
    rm different
fi

for file in $GOLDEN/*.out; do
    compareFile $file
    if [ $? -ne 0 ]; then
        wrong=$(($wrong + 1))
        echo "$file" >> different
    fi
done

if [ $wrong -ne 0 ]; then
    echo "$wrong files differed."
    exit 1
else
    echo "No files differed. Everything is ok."
fi
