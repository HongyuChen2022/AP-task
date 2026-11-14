#!/bin/bash

set -ue

D=$(readlink -f $(dirname ${BASH_SOURCE[0]}))

source $D/../get_global_vars.sh

DATASET=$1
LANG=$2
CLASSIFIER=$3
LABEL=$4

if [ $# -gt 4 ]; then
	NRS=$5
else
	NRS="3,4,5,7,8"
fi

IFS=',' read -a GPUS <<< "$NRS"


TRAIN=$INT_DATA/$DATASET/$LANG/train/$LANG.train.age.balance.jsonlist

if [[ ! -e $TRAIN ]]; then
    echo "No traininig file" $TRAIN
	exit
fi

WORK_DIR=$INT_DATA/$DATASET/$LANG/models/
if [[ ! -e $WORK_DIR ]]; then
    mkdir -p $WORK_DIR
fi

########## TRAINING ###############################

echo "Train models"
for fold in $(seq 0 $(($FOLDS - 1))); do
	GPU=${GPUS[$fold]}
	
	trainfile=$WORK_DIR/$LANG.train.clean.balance_not_$fold.$LABEL.jsonlist
	testfile=$WORK_DIR/$LANG.train.clean.balance_$fold.$LABEL.jsonlist
	modelfile=$WORK_DIR/$LANG.train.clean.balance_not_$fold.$LABEL.$CLASSIFIER-model
	logfile=$modelfile.log

	CUDA_VISIBLE_DEVICES=$GPU python -u $CMD/classifiers/train_clf.py --train $trainfile \
																			 --model $modelfile \
																			 --clf $CLASSIFIER \
																			 --lang $LANG \
																			 --eval_dev False \
																			 --dev $testfile \
																			 --label $LABEL 2>&1 | tee $logfile
done
wait

########## PREDICT ###############################

echo "Apply models to folds"
for fold in $(seq 0 $(($FOLDS - 1))); do
	GPU=${GPUS[$fold]}
	
	testfile=$WORK_DIR/$LANG.train.clean.balance_$fold.$LABEL.json # changed from select to balance
	modelfile=$WORK_DIR/$LANG.train.clean.balance_not_$fold.$LABEL.$CLASSIFIER-model

	if [[ ! -e $modelfile ]]; then
		modelfile=$WORK_DIR/$LANG.train.clean.balance_not_$fold.$CLASSIFIER-model.$LABEL
	fi

	outputfile=$WORK_DIR/$LANG.train.clean.balance_$fold.predict-$CLASSIFIER.$LABEL.pred-sep
	logfile=$modelfile.pred-sep-log

	CUDA_VISIBLE_DEVICES=$GPU python -u $CMD/classifiers/predict_clf.py --test $testfile \
																		--model $modelfile \
																		--out $outputfile \
																		--clf $CLASSIFIER \
																		--lang $LANG \
																		--label $LABEL \
																		--separate True 2>&1 | tee $logfile
done
wait

########## EVAL ###############################

echo "Eval models to folds"
for fold in $(seq 0 $(($FOLDS - 1))); do
	testfile=$WORK_DIR/$LANG.train.clean.balance_$fold.$LABEL.jsonlist #changed
	outputfile=$WORK_DIR/$LANG.train.clean.balance_$fold.predict-$CLASSIFIER.$LABEL.pred-sep
	python $CMD/evaluate/evaluate_label.py $testfile $outputfile $LABEL most_frequent
done

echo "Merge folds"
PRED_DIR=$INT_DATA/$DATASET/$LANG/predict/
OUT=$PRED_DIR/$LANG.train.clean.balance.predict-$CLASSIFIER.$LABEL.pred-sep
cat $WORK_DIR/$LANG.train.clean.balance_?.predict-$CLASSIFIER.$LABEL.pred-sep > $OUT
#rm $WORK_DIR/$LANG.train.clean.select_*

########## EVALUATE ###############################

echo "Evaluation started" $DATASET $LANG $CLASSIFIER $LABEL
python $CMD/evaluate/evaluate_label.py $TRAIN $OUT $LABEL most_frequent
