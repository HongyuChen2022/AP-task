import logging
import sys
import argparse

import classifier_utils, white_box

def get_classifier_module(clf_name, seed):
    if white_box.is_white_box_classifier(clf_name):
        return white_box
    
    import black_box
    black_box.set_seed(seed)
    if black_box.is_black_box_classifier(clf_name):
        return black_box
    
    logging.error("Unknown classifier: " + clf_name)
    return None

def buildArgs():
    argParser = argparse.ArgumentParser(description="""IMS Classifier Trainer""", add_help=False)
    argParser.add_argument('--help', action='help', default=argparse.SUPPRESS, help=argparse._('show this help message and exit'))

    classifierArgs = argParser.add_argument_group('classifier')
    classifierArgs.add_argument("--clf", help="classifier name", type=str, required=True)
    classifierArgs.add_argument("--label", help="target label to train", choices=[ "gender", "age" ], required=True, type=str)
    classifierArgs.add_argument("--loglevel", help="which log level to use", choices=[ "DEBUG", "INFO", "WARN", "ERROR", "CRITICAL" ], required=False, default="DEBUG")

    # files
    filesArgs = argParser.add_argument_group('files')
    filesArgs.add_argument("--train", help="training file", type=str, required=True)
    filesArgs.add_argument("--model", help="save model to the file", type=str, required=True)
    filesArgs.add_argument("--dev", help="development file", type=str, required=False)
    filesArgs.add_argument("--test", help="test file", type=str, required=False)
    filesArgs.add_argument("--separate", help="treat each text separately", type=str, required=False, default="False")

    # data
    dataArgs = argParser.add_argument_group('data')
    dataArgs.add_argument("--lang", help="language", type=str, required=True)

    # training
    trainingArgs = argParser.add_argument_group('training')
    trainingArgs.add_argument("--seed", help="random seed", type=int, required=False, default=272419)
    trainingArgs.add_argument("--epochs", help="nr of epochs", type=int, required=False, default=5)
    trainingArgs.add_argument("--eval_dev", help="reports dev accuracy", type=str, required=False, default="False")
    trainingArgs.add_argument("--eval_train", help="reports training accuracy", type=str, required=False, default="True")

    # parse
    args = argParser.parse_args()

    # set logging level
    lformat = '[%(levelname)s] %(asctime)s %(name)s# %(message)s'
    logging.basicConfig(stream=sys.stdout, level=logging.getLevelName(args.loglevel), format=lformat)

    # this still leaves neural models being random
    classifier_utils.set_seed(args.seed)
    logging.info("Freezing random seed: %i" % args.seed)

    def __parseBoolean(v):
        return v != None and v.lower() in ("yes", "true", "t", "1")

    args.eval_dev = __parseBoolean(args.eval_dev)
    args.eval_train = __parseBoolean(args.eval_train)
    args.separate = __parseBoolean(args.separate)

    return args

if __name__ == '__main__':
    args  = buildArgs()

    # training
    dataset = classifier_utils.load_dataset(train_file=args.train, 
                                            dev_file = args.dev,
                                            test_file = args.test)
    
    logging.info("Loading dataset with separate: %s" % str(args.separate))
    dataset = classifier_utils.prepare_dataset(dataset, args.label, ungroup=args.separate)
    classifier = get_classifier_module(args.clf, args.seed)
    
    logging.info("Classifier: %s %s" % (classifier.__name__, args.clf))
    model, score = classifier.train_clf(args.clf,
                                        args.lang, 
                                        dataset, 
                                        args.model, 
                                        eval_dev = args.eval_dev,
                                        eval_train = args.eval_train,
                                        epochs = args.epochs,
                                        seed = args.seed)

    logging.info("TRAIN ACC: %f" % score)
    model.dump_model(args.model)

    if args.dev is not None:
        preds, pred_score = classifier.predict_clf(dataset["dev"], model)
        logging.info("PRED ACC: %f" % pred_score)