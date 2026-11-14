#!/bin/bash

set -ue

D=$(readlink -f $(dirname ${BASH_SOURCE[0]}))

source $D/../get_global_vars.sh

IFS=',' read -a allLangs <<< `getLangs $1`
IFS=',' read -a corporas <<< "$2"

for lang in "${allLangs[@]}"; do
	echo "Working on " $lang
	for corpora in "${corporas[@]}"; do
        # corpora scenario
#        CORP_DIR=$(find $ORG_DATA/PAN13/$corpora/ -type d -name "pan13-author-profiling-$corpora-corpus*" -print -quit)
#        if [[ ! -e $CORP_DIR ]]; then
#            echo "No corpora dir" $CORP_DIR
#            continue
#        fi

#        CORP_FILE=$CORP_DIR/truth-$lang.txt
#        if [[ ! -e $CORP_FILE ]]; then
#            echo "No corpora file" $CORP_FILE
#            continue
#        fi

#        LANG_DIR=$CORP_DIR/$lang/
#        if [[ ! -e $LANG_DIR ]]; then
#            echo "No corpora dir" $LANG_DIR
#            continue
#        fi

        # to make it consistent across datasets
#        if [ "$corpora" = "training" ]; then
#            corpora="train"
#        fi

        OUT_DIR=$INT_DATA/pan13/$lang/$corpora
#        if [[ ! -e $OUT_DIR ]]; then
#            mkdir -p $OUT_DIR
#        fi

        OUT_FILE=$OUT_DIR/$lang.$corpora.original.jsonlist
#        python $CMD/preprocess/convert_pan13_to_json.py $CORP_FILE $LANG_DIR $lang $OUT_FILE
        
        CLEAN_FILE=$OUT_DIR/$lang.$corpora.clean.jsonlist
#        python $CMD/preprocess/clean_text.py $OUT_FILE $CLEAN_FILE

        if [ "$corpora" = "train" ]; then
            SELECT_FILE=$ORG_DATA/PAN13/data_selection.txt
            PART_OUT=$OUT_DIR/$lang.$corpora.clean.age.balance.jsonlist
            python $CMD/preprocess/select_data.py $CLEAN_FILE $SELECT_FILE $PART_OUT
        fi

        echo "Preprocessing for " $corpora $lang " finished"
    done
done
