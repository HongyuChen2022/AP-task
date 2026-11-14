#!/bin/bash

set -ue

D=$(readlink -f $(dirname ${BASH_SOURCE[0]}))

source $D/../get_global_vars.sh

DATASET=$1
LANG=$2
CLASSIFIER=$3
LABEL=$4
PREDDATA=$5

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

TEST=$INT_DATA/$PREDDATA/$LANG/test/$LANG.test.clean.jsonlist

if [[ ! -e $TEST ]]; then
    echo "No test file" $TEST
	exit
fi

MODELS_DIR=$INT_DATA/$DATASET/$LANG/models/
if [[ ! -e $MODELS_DIR ]]; then
    mkdir -p $MODELS_DIR
fi


########### TRAINING ################################
MODEL=$MODELS_DIR/$LANG.train.clean.select.$CLASSIFIER-model.$LABEL

if [[ ! -e $MODEL ]]; then
    echo "No model!" $MODEL
    continue
fi

########## PREDICTING ###############################

PRED_DIR=$INT_DATA/$PREDDATA/$LANG/predict/
if [[ ! -e $PRED_DIR ]]; then
    mkdir -p $PRED_DIR
fi

OUT=$PRED_DIR/$LANG.test.clean.select.cross-$DATASET.predict-$CLASSIFIER.$LABEL.pred

echo "Prediction started" $PREDDATA $LANG $CLASSIFIER $LABEL
python -u $CMD/classifiers/predict_clf.py --test $TEST \
                                                    --model $MODEL \
                                                    --out $OUT \
                                                    --clf $CLASSIFIER \
                                                    --lang $LANG \
                                                    --label $LABEL \
                                                    --seed $SEED  2>&1 | tee $MODEL.pred-log

########## EVALUATE ###############################

echo "Evaluation started" $PREDDATA $LANG $CLASSIFIER $LABEL $SEED
python $CMD/evaluate/evaluate_label.py $TEST $OUT $LABEL | tee $MODEL.pred-eval

for i in `seq $START $RUNS`; do
    ########### TRAINING ################################
    MODEL=$MODELS_DIR/$LANG.train.clean.select.$CLASSIFIER-model.run-$i.$LABEL

    if [[ ! -e $MODEL ]]; then
        echo "No model!" $MODEL
        continue
    fi

    ########## PREDICTING ###############################

    PRED_DIR=$INT_DATA/$PREDDATA/$LANG/predict/
    if [[ ! -e $PRED_DIR ]]; then
        mkdir -p $PRED_DIR
    fi

    OUT=$PRED_DIR/$LANG.test.clean.select.cross-$DATASET.predict-$CLASSIFIER.run-$i.$LABEL.pred

    echo "Prediction started" $PREDDATA $LANG $CLASSIFIER $LABEL
    python -u $CMD/classifiers/predict_clf.py --test $TEST \
                                                                        --model $MODEL \
                                                                        --out $OUT \
                                                                        --clf $CLASSIFIER \
                                                                        --lang $LANG \
                                                                        --label $LABEL \
                                                                        --seed $SEED  2>&1 | tee $MODEL.pred-log

    ########## EVALUATE ###############################

    echo "Evaluation started" $PREDDATA $LANG $CLASSIFIER $LABEL $SEED
    python $CMD/evaluate/evaluate_label.py $TEST $OUT $LABEL | tee $MODEL.pred-eval
done