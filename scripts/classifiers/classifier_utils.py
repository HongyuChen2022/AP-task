import random
import os
import logging
import numpy as np
import datasets
import pandas as pd

def set_seed(_seed):
    """Set all seeds to make results reproducible (deterministic mode).
       When seed is None, disables deterministic mode.
    :param seed: an integer to your choosing
    # https://discuss.huggingface.co/t/initializing-the-weights-of-the-final-layer-of-e-g-bertfortokenclassification-with-a-manual-seed/1377/3
    """
    if _seed is not None:
        np.random.seed(_seed)
        random.seed(_seed)
        os.environ['PYTHONHASHSEED'] = str(_seed)

def load_dataset(train_file, dev_file, test_file=None, dev_size=0.1, max_dev=10000):
    train_data =  datasets.load_dataset('json', data_files={"train": train_file}, split='train')
    train_data.shuffle()
    
    if dev_file is not None:
        dev_data = datasets.load_dataset('json', data_files={"dev": dev_file}, split='dev' )
        if len(dev_data) > max_dev:
            dev_data = datasets.load_dataset('json', data_files={"dev": dev_file}, split='dev[:'+str(max_dev)+']' )

        result = datasets.DatasetDict({ "train" : train_data,
                                        "dev" : dev_data })
    else:
        result = train_data.train_test_split(test_size=dev_size )

        # changing name 'test' to 'dev'
        result["dev"] = result["test"]
        del result["test"]

    if test_file != None:
        result["test"] =  datasets.load_dataset('json', data_files={"test": test_file} )["test"]

    return result

def __ungroupTexts(dataset):
    result = [ ]
    
    # there should be a function for this, but I can't find it
    for elem in dataset:
        new_elem = elem.copy()
        for text in elem["texts"]:
            new_elem["text"] = text
            result.append(new_elem)

    return datasets.Dataset.from_pandas(pd.DataFrame(data=result))

def __concatenateTexts(dataset):
    concatenated = [ "\n".join(x) for x in dataset["texts"] ]
    return dataset.add_column(name="text", column=concatenated)

def prepare_dataset(data, label, ungroup=False):
    def concatenate_texts(dataset):
        return [ "\n".join(x) for x in dataset["texts"] ]

    # concatenating texts from the same author
    for corp in data.keys():
        if ungroup:
            data[corp] = __ungroupTexts(data[corp])
        else:
            data[corp] = __concatenateTexts(data[corp])

    # removing unnecesary
    data = data.remove_columns("texts")

    # preparing labels
    data = data.rename_column(label, 'label')

    logging.info("Using dataset:" + str(data))
    return data