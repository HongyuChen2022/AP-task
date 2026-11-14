#!/bin/bash

set -ue

D=$(readlink -f $(dirname ${BASH_SOURCE[0]}))

source $D/../get_global_vars.sh

DATA=$ORG_DATA/Blogs/blogtext.csv
lang="en"

OUT_DIR=$INT_DATA/blogs/$lang/all
if [[ ! -e $OUT_DIR ]]; then
    mkdir -p $OUT_DIR
fi
        
OUT_FILE=$OUT_DIR/$lang.all.original.jsonlist
#python $CMD/preprocess/convert_blogs_to_json.py $DATA $OUT_FILE

# split test data
TRAIN_DIR=$INT_DATA/blogs/$lang/train
if [[ ! -e $TRAIN_DIR ]]; then
    mkdir -p $TRAIN_DIR
fi

# split test data
TEST_DIR=$INT_DATA/blogs/$lang/test
if [[ ! -e $TEST_DIR ]]; then
    mkdir -p $TEST_DIR
fi

TRAIN_FILE=$TRAIN_DIR/$lang.train.original.jsonlist
TEST_FILE=$TEST_DIR/$lang.test.original.jsonlist
python $CMD/preprocess/split_corpora.py $OUT_FILE $TRAIN_FILE $TEST_FILE 0.2

CLEAN_TRAIN=$TRAIN_DIR/$lang.train.clean.jsonlist
python $CMD/preprocess/clean_text.py $TRAIN_FILE $CLEAN_TRAIN

CLEAN_TEST=$TEST_DIR/$lang.test.clean.jsonlist
python $CMD/preprocess/clean_text.py $TEST_FILE $CLEAN_TEST

SELECT_FILE=$ORG_DATA/Blogs/data_selection.txt
PART_OUT=$TRAIN_DIR/$lang.train.clean.select.jsonlist
python $CMD/preprocess/select_data.py $CLEAN_TRAIN $SELECT_FILE $PART_OUT