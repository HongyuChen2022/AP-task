import pickle
import logging

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.calibration import CalibratedClassifierCV
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn import dummy
import numpy as np
import random

def is_white_box_classifier(clf_name):
    return clf_name in [ "lr", "rf", "dt", "nb", "dummy" ]

def build_model():
    return WhiteBoxModel()

class WhiteBoxModel(object):
    def __init__(self):
        self.label2id = None
        self.id2label = None
        self.vectorizer = None
        self.clf = None

    def dump_model(self, model_file):
        with open(model_file, "wb") as out:
            pickle.dump(self.label2id, out)
            pickle.dump(self.vectorizer, out)
            pickle.dump(self.clf, out)

    def load_model(self, model_file):
        with open(model_file, "rb") as model:
            self.label2id = pickle.load(model)
            self.vectorizer = pickle.load(model)
            self.clf = pickle.load(model)

            self.id2label = dict([ (i, v) for (v, i) in self.label2id.items() ])

    def set_labels(self, data):
        self.label2id =  dict([ (val, i) for (i, val) in enumerate(set(data["label"])) ])
        self.id2label = dict([ (i, v) for (v, i) in self.label2id.items() ])

    def map_label_to_id(self, row):
        if row["label"] not in self.label2id:
            logging.warning("Unknown label:" + row["label"])
            row["label"] = 0
        else:
            row["label"] = self.label2id[row["label"]]
        
        return row 
    
def train_clf(clf_name, lang, data, model_dir, eval_dev = None, features = "word", search = False, eval_train = True, epochs = None, seed = None):
    model = WhiteBoxModel()
    model.set_labels(data["train"])

    data = data.map(model.map_label_to_id)

    # the only source of randomness
    train_data = data["train"].shuffle(seed)
    print("Shuffle!", train_data[0])

    if features == "char":
        logging.info("Character-based n-grams")
        model.vectorizer = TfidfVectorizer(analyzer='char', ngram_range=(2,6), sublinear_tf=True, min_df=0.001, max_df=0.6)
    else:
        logging.info("Words tfidf")
        model.vectorizer = TfidfVectorizer(lowercase=True)
    
    train_vec = model.vectorizer.fit_transform(train_data["text"])

    if clf_name == "nb":
        print("Converting...")
        train_vec = np.asarray(train_vec.todense())
    
    if search:
        logging.info("Searching for the best parameters")
        model.clf = _get_best_clf(clf_name, train_vec, train_data["label"])
    else:
        logging.info("Using default parameters")
        model.clf = _build_classifier(clf_name, seed)

    logging.info("Training starts")
    model.clf.fit(train_vec, train_data["label"])
    train_score = None
    if eval_train:
        train_score = model.clf.score(train_vec, train_data["label"])

    return model, train_score

def predict_clf(data, model, eval_data = True):
    data_vec = model.vectorizer.transform(data["text"])

    if isinstance(model.clf, GaussianNB):
        print("Converting...")
        data_vec = np.asarray(data_vec.todense())

    data_pred = model.clf.predict(data_vec)

    # converting ids to labels
    label_pred = [ model.id2label[lid] for lid in data_pred ]
    
    pred_score = None
    if eval_data:
        data = data.map(model.map_label_to_id)
        pred_score = model.clf.score(data_vec, data["label"])

    return label_pred, pred_score


def _build_classifier(clf_name, seed):
    if clf_name == "lr":
        return LogisticRegression(n_jobs=-1, solver='saga', multi_class='auto', random_state=seed)
    elif clf_name == "rf":
        return RandomForestClassifier(n_estimators=10, random_state=seed)
    elif clf_name == "dt":
        return DecisionTreeClassifier(random_state=seed)
    elif clf_name == "nb":
        return GaussianNB()
    elif clf_name == "dummy":
        return dummy.DummyClassifier(strategy="most_frequent")
    
    logging.error("Unknown classifier: " + clf_name)

def _get_search_params(clf_name):
    if clf_name == "lr":
        return {'base_estimator__C': [5.0, 2.0, 1.0, 0.5, 0.1]}
    
    elif clf_name == "rf":
        return {
            'base_estimator__n_estimators': [100, 200, 300],  # Number of trees in the forest
            'base_estimator__max_depth': [None, 5, 10],  # Maximum depth of each tree
            'base_estimator__min_samples_split': [2, 5, 10]  # Minimum number of samples required to split an internal node
        }
    
    elif clf_name == "dt":
        return {
            'base_estimator__max_depth': [None, 5, 10],  # Maximum depth of the tree
            'base_estimator__min_samples_split': [2, 5, 10]  # Minimum number of samples required to split an internal node
            }
    
    logging.error("Search is not supported: " + clf_name)

def _get_best_clf(clf_name, train_vec, labels):
    param_grid = _get_search_params(clf_name)
    base_clf = CalibratedClassifierCV(cv=5, base_estimator=_build_classifier(clf_name))

    search = GridSearchCV(base_clf, param_grid, cv=3, scoring='f1_micro', n_jobs=-1)
    search.fit(train_vec, labels)

    return search.best_estimator_.base_estimator