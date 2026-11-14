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

        # to make it consistent across datasets
        outLang=$lang
        if [ "$lang" = "en_us" ]; then
            outLang="en"
        fi

        # to lowercase it
        corpora=${corpora,,}

        CORP_FILE=$INT_DATA/trust-2/$outLang/$corpora/$outLang.$corpora.original.jsonlist
        if [ ! -e $CORP_FILE ]; then
            echo "No corpora file" $CORP_FILE
            continue
        fi

        OUT_DIR=$INT_DATA/trust/$outLang/
        if [[ ! -e $OUT_DIR ]]; then
            mkdir -p $OUT_DIR
        fi

        OUT_FILE=$OUT_DIR/$outLang.$corpora.jsonlist
        python $CMD/preprocess/convert_trust_to_class.py $CORP_FILE $OUT_FILE
        
        echo "Preprocessing for " $corpora $lang " finished"
    done
done
