import json
import pandas as pd
import numpy as np

def group_authors(inFile, writer):
    data = pd.read_json(inFile, orient='records', lines=True)
    
    for authorId, rows in data.groupby("id"):
        for field in [ "age", "gender"]:
            if len(rows[field].unique()) != 1:
                print("Too many fields", field, rows[field].unique())

        age = rows["age"].unique()[0]
        gender = rows["gender"].unique()[0]
        texts = list(rows["text"])

        input = {'gender': gender,
                 'age': age,
                 "id" : authorId,
                 "texts" : texts }
        
        json.dump(input, writer)
        writer.write("\n")
            
if __name__ == '__main__':
    import os, sys

    args = sys.argv[1:]
    
    inFile = args[0]
    outFile = args[1]

    group_authors(inFile, open(outFile, 'w'))