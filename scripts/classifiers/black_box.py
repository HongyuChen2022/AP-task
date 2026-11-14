import pickle
import logging
import torch
import transformers
from transformers import AutoTokenizer, AutoModelForSequenceClassification, Trainer, TrainingArguments
from torch.utils.data import DataLoader
from sklearn.metrics import f1_score
import numpy as np

BATCH_SIZE = 16
DEVICE = torch.device("cuda" if torch.cuda.is_available() else "cpu")

def is_black_box_classifier(clf_name):
    return clf_name in [ "bert", "roberta", "xlnet", "mbert" ]

def set_seed(_seed):
    """Set all seeds to make results reproducible (deterministic mode).
       When seed is None, disables deterministic mode.
    :param seed: an integer to your choosing
    # https://discuss.huggingface.co/t/initializing-the-weights-of-the-final-layer-of-e-g-bertfortokenclassification-with-a-manual-seed/1377/3
    """
    if _seed is not None:
        logging.info("Setting random seed for neural libraries:" + str(_seed))
        torch.manual_seed(_seed)
        torch.cuda.manual_seed_all(_seed)
        torch.backends.cudnn.deterministic = True
        torch.backends.cudnn.benchmark = False
        transformers.set_seed(_seed)

def build_model():
    return BlackBoxModel()

###
# Model to keep all important objects
###

class BlackBoxModel(object):
    def __init__(self):
        self.tokenizer = None
        self.model = None

    def init_model(self, model_name):
        self.model_name = model_name
        self.tokenizer = AutoTokenizer.from_pretrained(self.model_name)
        
    def build_model(self):
        self.model = AutoModelForSequenceClassification.from_pretrained(
            self.model_name,
            num_labels = len(self.label2id), 
            output_attentions = False, 
            output_hidden_states = True,
        ).to(DEVICE)

        logging.info("Using model: {}".format(str(self.model)))
        return self.model

    def dump_model(self, model_dir):
        self.model.save_pretrained(model_dir)

        with open(model_dir + "/params.txt", "wb") as out:
            pickle.dump(self.model_name, out)
            pickle.dump(self.label2id, out)
            
    def load_model(self, model_dir):
        with open(model_dir + "/params.txt", "rb") as model:
            self.model_name = pickle.load(model)
            self.label2id = pickle.load(model)
            self.id2label = dict([ (i, v) for (v, i) in self.label2id.items() ])
            
        
        self.tokenizer = AutoTokenizer.from_pretrained(self.model_name)
        self.model = AutoModelForSequenceClassification.from_pretrained(model_dir,
                                                                        num_labels = len(self.label2id), 
                                                                        output_attentions = False,
                                                                        output_hidden_states = True)
        
        self.model.to(DEVICE)

    def tokenize(self, batch):
        return self.tokenizer(batch["text"], padding=True, truncation=True,  
                              max_length = 512)
    
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
  
def train_clf(clf_name, lang, data, model_dir, eval_dev = True, eval_train = True, epochs = 5, seed = 42):
    logging.info("Training device: " + str(DEVICE))

    bbmodel = BlackBoxModel()
    bbmodel.set_labels(data["train"])
    bbmodel.init_model(_get_model_name(clf_name, lang))

    data = data.map(bbmodel.map_label_to_id)
    data_encoded = data.map(bbmodel.tokenize, batched=True, batch_size=BATCH_SIZE)
    data_encoded.set_format("torch", columns=["input_ids", "attention_mask", "label"])

    train_encoded = data_encoded["train"]

    if eval_dev:
        dev_encoded =  data_encoded["dev"]
        evaluation_strategy="epoch"
    else:
        dev_encoded = None
        evaluation_strategy="no"

    logging_steps = max(len(data["train"]) // BATCH_SIZE, 1)
    training_args = TrainingArguments(output_dir=model_dir,
                                    num_train_epochs=epochs,
                                    learning_rate= 1e-5,
                                    per_device_train_batch_size=BATCH_SIZE,
                                    do_eval = eval_dev,
                                    evaluation_strategy=evaluation_strategy,
                                    per_device_eval_batch_size=BATCH_SIZE,
                                    logging_strategy="epoch", #Extra: to log training data stats for loss 
                                    disable_tqdm=False,
                                    logging_steps=logging_steps,
                                    push_to_hub=False,
                                    log_level="error",
                                    seed=seed,
                                    save_total_limit = 1,
                                    weight_decay=0.01,
                                    eval_accumulation_steps=10
                                    )

    trainer = Trainer(model_init=bbmodel.build_model, 
                      args=training_args,
                      train_dataset=train_encoded,
                      eval_dataset=dev_encoded,
                      tokenizer=bbmodel.tokenizer,
                      compute_metrics=__compute_metrics)

    trainer.train()
    logging.info("Training finished")

    train_score = None
    if eval_train:
        predictions = __get_the_predictions(train_encoded, bbmodel)
        train_score = f1_score(data["train"]["label"], predictions, average='macro')

    return bbmodel, train_score

def predict_clf(data, bbmodel, eval_data = True):
    data_encoded = data.map(bbmodel.tokenize, batched=True, batch_size=BATCH_SIZE)
    data_encoded.set_format("torch", columns=["input_ids", "attention_mask", "label"])

    predictions = __get_the_predictions(data_encoded, bbmodel)

    label_pred = [ bbmodel.id2label[lid] for lid in predictions ]

    pred_score = None
    if eval_data:
        data = data.map(bbmodel.map_label_to_id)
        pred_score = f1_score(data["label"], predictions, average='macro')

    return label_pred, pred_score

def _get_model_name(clf_name, lang):
    if lang == "es" and clf_name == "bert":
        return "dccuchile/bert-base-spanish-wwm-uncased"
    
    if lang == "es" and clf_name == "roberta":
        return "bertin-project/bertin-roberta-base-spanish"

    if lang != "en":
        logging.error("Unknown language:", lang)
        return None
    
    if clf_name == "bert":
        return "bert-base-uncased"
    elif clf_name == "mbert":
        return "bert-base-multilingual-cased"
    elif clf_name == "roberta":
        return "roberta-base"
    elif clf_name == "xlnet":
        return "xlnet-base-cased"
    
    logging.error("Unknown classifier name: " + clf_name)

def __get_the_predictions(data_encoded, bbmodel):
    loader = DataLoader(data_encoded, batch_size=BATCH_SIZE)

    result = [ ]
    bbmodel.model.eval()

    for batch in loader:
        batch = {k: v.to(DEVICE) for k, v in batch.items() if k in bbmodel.tokenizer.model_input_names}
        with torch.no_grad():
            outputs = bbmodel.model(**batch)
            predictions = outputs.logits.argmax(dim=-1).cpu().detach().numpy()
            result.extend(predictions)

    return result

def __compute_metrics(eval_pred):
    predictions = eval_pred.predictions
    labels = eval_pred = eval_pred.label_ids

    preds = predictions[0].argmax(-1)
    f1 = f1_score(labels, preds, average="macro") #.cpu().detach().numpy()
    return {
            "f1": f1
            }
