#!/bin/sh
fails=0
tries=0
for f in $(ls src/parser/tests/)
do
    path=src/parser/tests/$f
    # try to parse $f only if it's a file
    if [ -f $path ]; then
        echo $f
        tries=$(($tries+1))
        ./absyn_generator.byte $path
        # $? stores whether the previous command succeeded
        if [ $? -ne 0 ]; then
            echo "Parse failed on $f"
            fails=$(($fails+1))
            #break
        fi
    fi
done
echo "FAILED: " $fails "/" $tries
