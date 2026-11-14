#!/bin/bash

set -ue

D=$(readlink -f $(dirname ${BASH_SOURCE[0]}))

source $D/../get_global_vars.sh

DATASET=$1
LANG=$2
CLASSIFIER=$3
LABEL=$4
GPU=$5

## options
if [  $# -gt 5 ]; then
    START=$6
else
    START=1
fi

if [  $# -gt 6 ]; then
    RUNS=$7
else
    RUNS=1
fi

TRAIN=$INT_DATA/$DATASET/$LANG/train/$LANG.train.clean.select.jsonlist
TEST=$INT_DATA/$DATASET/$LANG/test/$LANG.test.clean.jsonlist

if [[ ! -e $TRAIN ]]; then
    echo "No traininig file" $TRAIN
	exit
fi

if [[ ! -e $TEST ]]; then
    echo "No test file" $TEST
	exit
fi

WORK_DIR=$INT_DATA/$DATASET/$LANG/models/
if [[ ! -e $WORK_DIR ]]; then
    mkdir -p $WORK_DIR
fi

readarray SEEDS < $CMD/seeds.txt

for i in `seq $START $RUNS`; do
    SEED=${SEEDS[$i]}

    ########### TRAINING ################################
    PRED_DIR=$INT_DATA/$DATASET/$LANG/predict/
    OUT=$PRED_DIR/$LANG.test.clean.select.predict-$CLASSIFIER.run-$i.$LABEL.pred

    if [[ -e $OUT ]]; then
		echo $OUT exists
        continue
    fi

    MODEL=$WORK_DIR/$LANG.train.clean.select.$CLASSIFIER-model.run-$i.$LABEL


    echo "Training started" $DATASET $LANG $CLASSIFIER $LABEL $SEED
    
    CUDA_VISIBLE_DEVICES=$GPU python -u $CMD/classifiers/train_clf.py --train $TRAIN \
                                                                    --model $MODEL \
                                                                    --clf $CLASSIFIER \
                                                                    --label $LABEL \
                                                                    --lang $LANG \
                                                                    --eval_dev False \
                                                                    --seed $SEED \
                                                                    --dev $TEST 2>&1 | tee $MODEL.log

    ########## PREDICTING ###############################

    PRED_DIR=$INT_DATA/$DATASET/$LANG/predict/
    if [[ ! -e $PRED_DIR ]]; then
        mkdir -p $PRED_DIR
    fi

    OUT=$PRED_DIR/$LANG.test.clean.select.predict-$CLASSIFIER.run-$i.$LABEL.pred
    echo "Prediction started" $DATASET $LANG $CLASSIFIER $LABEL
    CUDA_VISIBLE_DEVICES=$GPU python -u $CMD/classifiers/predict_clf.py --test $TEST \
                                                                        --model $MODEL \
                                                                        --out $OUT \
                                                                        --clf $CLASSIFIER \
                                                                        --lang $LANG \
                                                                        --label $LABEL \
                                                                        --seed $SEED  2>&1 | tee $MODEL.pred-log

    ########## EVALUATE ###############################

    echo "Evaluation started" $DATASET $LANG $CLASSIFIER $LABEL $SEED
    python $CMD/evaluate/evaluate_label.py $TEST $OUT $LABEL | tee $MODEL.pred-eval
done