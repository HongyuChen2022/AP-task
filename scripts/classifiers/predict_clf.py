import random
import pandas as pd
import pickle
import logging
import sys, os
import datasets

import classifier_utils, white_box
import argparse
from train_clf import get_classifier_module


def buildArgs():
    argParser = argparse.ArgumentParser(description="""IMS Classifier Predicor""", add_help=False)
    argParser.add_argument('--help', action='help', default=argparse.SUPPRESS, help=argparse._('show this help message and exit'))

    classifierArgs = argParser.add_argument_group('classifier')
    classifierArgs.add_argument("--clf", help="classifier name", type=str, required=True)
    classifierArgs.add_argument("--label", help="target label to train", choices=[ "gender", "age" ], required=True, type=str)
    classifierArgs.add_argument("--loglevel", help="which log level to use", choices=[ "DEBUG", "INFO", "WARN", "ERROR", "CRITICAL" ], required=False, default="DEBUG")

    # files
    filesArgs = argParser.add_argument_group('files')
    filesArgs.add_argument("--model", help="save model to the file", type=str, required=True)
    filesArgs.add_argument("--test", help="test file", type=str, required=True)
    filesArgs.add_argument("--out", help="output file", type=str, required=True)

    # data
    dataArgs = argParser.add_argument_group('data')
    dataArgs.add_argument("--lang", help="language", type=str, required=True)

    # prediction
    predArgs = argParser.add_argument_group('prediction')
    predArgs.add_argument("--seed", help="random seed", type=int, required=False, default=272419)
    predArgs.add_argument("--separate", help="run prediction of separate texts", type=str, required=False, default="False")

    # parse
    args = argParser.parse_args()

    # set logging level
    lformat = '[%(levelname)s] %(asctime)s %(name)s# %(message)s'
    logging.basicConfig(stream=sys.stdout, level=logging.getLevelName(args.loglevel), format=lformat)

    # this still leaves neural models being random
    classifier_utils.set_seed(args.seed)
    logging.info("Freezing random seed: %i" % args.seed)

    # converting booleans
    def __parseBoolean(v):
        return v != None and v.lower() in ("yes", "true", "t", "1")

    args.separate = __parseBoolean(args.separate)

    return args

if __name__ == '__main__':
    args  = buildArgs()

    # training
    dataset = datasets.load_dataset('json', data_files={"test" : args.test})
    dataset = classifier_utils.prepare_dataset(dataset, args.label, ungroup=args.separate)
    classifier = get_classifier_module(args.clf, args.seed)
    
    model = classifier.build_model()
    model.load_model(args.model)

    preds, _ = classifier.predict_clf(dataset["test"], model, eval_data = False)
    
    with open(args.out, "w") as out:
        for predLabel, dataId in zip(preds, dataset["test"]["id"]):
            out.write("%s\t%s\n" % (dataId, predLabel))