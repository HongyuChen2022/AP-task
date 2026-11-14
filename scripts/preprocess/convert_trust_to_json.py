import json
import pandas as pd
import math

def age_to_decades(age):
    age = age // 10 * 10
    age = str(age) + "s"
    return age
    
def convert_exel_to_json(inFile, writer):
    data = pd.read_excel(inFile)

    data['gender'].replace("M",'male', inplace= True)
    data['gender'].replace("F",'female', inplace= True)
    data = data.rename(columns={"Unnamed: 0": "id"})

    for authorId, rows in data.groupby("id"):
        for field in [ "age", "gender", "age_cat"]:
            if len(rows[field].unique()) != 1:
                print("Too many fields", field, rows[field].unique())

        texts = [ ]
        for text in rows["text"]:
            if type(text) == type(1.0) and math.isnan(text):
                continue

            if type(text) == type(1):
                continue

            texts.append(text)

        if len(texts) == 0:
            print("Skipping", authorId)
            continue

        age = rows["age"].unique()[0]
        gender = rows["gender"].unique()[0]
        age_cat = rows["age_cat"].unique()[0]

        input = {'age': age_to_decades(age),
                 'gender': gender,
                 'age_exact': int(age),
                 'age_range': age_cat,
                 "id" : str(authorId),
                 "text" : text[0]}
        
        json.dump(input, writer)
        writer.write("\n")
            
if __name__ == '__main__':
    import os, sys

    args = sys.argv[1:]
    
    inFile = args[0]
    outFile = args[1]

    convert_exel_to_json(inFile, open(outFile, 'w'))