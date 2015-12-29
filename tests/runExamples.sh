#!/bin/bash
## Run KoaT against every instance in the test directory

DEBUG=false
KOAT=../koat.native
OUTDIR=out


runDirectory()
{
    dir=$1
    for file in $dir/*.koat; do
        if [ -e $file ]; then
            filename=$(basename $file)
            if $DEBUG; then
                echo "./koat.native $file > ${file}.out"
                echo "head -n -1 ${file}.out > $filename"
            else
                $KOAT $file > ${file}.out
                head -n -1 ${file}.out > $OUTDIR/${filename}.out
                rm ${file}.out
            fi
        fi
    done
}

legalOutDir()
{
    outdir=$1
    if [ -e $outdir ]; then
        if [ -d $outdir ]; then
            for file in $dir/* ; do
                if [ -e $file ]; then
                    echo "Output Directory already had files in it. Aborting."
                    exit 1
                fi
            done
        else
            echo "Output directory isn't a directory."
            exit 1
        fi
    else
        echo "Output directory doesn't exist. Creating it."
        mkdir $outdir
    fi
}

legalKoat()
{
    if [ -e $KOAT ]; then
        return
    else
        echo "Couldn't find KoAT. Expected it at $KOAT"
        if $DEBUG; then
            return
        else
            exit 1
        fi
    fi
}

main()
{
    legalKoat
    legalOutDir $OUTDIR
    ## Simple non-nested directories
    for dir in cexamples debugExamples weightedExamples/*; do
	echo "runDirectory inputs/$dir"
        runDirectory inputs/$dir
    done
    ## Nested Directory
}

main
