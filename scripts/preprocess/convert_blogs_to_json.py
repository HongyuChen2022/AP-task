import json
import pandas as pd
import numpy as np

def convert_blogs_to_json(inFile, writer):
    data = pd.read_csv(inFile)
    data.drop(['date'], axis=1, inplace=True)

    # normalizing age
    data['age'].replace(np.arange(13,18),'10s', inplace= True)
    data['age'].replace(np.arange(23,28),'20s', inplace= True)
    data['age'].replace(np.arange(33,49),'30s', inplace= True)

    data['topic'].replace('indUnk','Unknown', inplace= True)

    for authorId, rows in data.groupby("id"):
        for field in [ "age", "gender", "topic", "sign"]:
            if len(rows[field].unique()) != 1:
                print("Too many fields", field, rows[field].unique())

        age = rows["age"].unique()[0]
        gender = rows["gender"].unique()[0]
        industry = rows["topic"].unique()[0]
        sign = rows["sign"].unique()[0]
        texts = list(rows["text"])

        input = {'gender': gender,
                 'age': age,
                 "id" : authorId,
                 "texts" : texts,
                 "industry" : industry,
                 "sign" : sign}
        
        json.dump(input, writer)
        writer.write("\n")
            
if __name__ == '__main__':
    import os, sys

    args = sys.argv[1:]
    
    inFile = args[0]
    outFile = args[1]

    convert_blogs_to_json(inFile, open(outFile, 'w'))