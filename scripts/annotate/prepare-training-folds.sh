#!/bin/bash

set -ue

D=$(readlink -f $(dirname ${BASH_SOURCE[0]}))

source $D/../get_global_vars.sh

DATASET=$1
LANG=$2
LABEL=$3

TRAIN=$INT_DATA/$DATASET/$LANG/train/$LANG.train.clean.balance.json

if [[ ! -e $TRAIN ]]; then
    echo "No traininig file" $TRAIN
	exit
fi

WORK_DIR=$INT_DATA/$DATASET/$LANG/models/
if [[ ! -e $WORK_DIR ]]; then
    mkdir -p $WORK_DIR
fi

echo "Make training folds"
python $CMD/common/prepare_train_folds.py --num-chunks $FOLDS --output $WORK_DIR --label $LABEL $TRAIN