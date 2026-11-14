import pickle
import pandas as pd
from sklearn.metrics import f1_score

def read_preds(inFile):
    result = { }
    for line in open(inFile, "r"):
        line = line.strip()

        if not line:
            continue

        dataId, predVal = line.split("\t")

        if dataId not in result:
            result[dataId] = [ ]

        result[dataId].append(predVal)

    return result

def convert_preds(predValues, evalMore):
    def __most_frequent(l):
        return max(set(l), key = l.count)
 
    if evalMore is None:
        return list(predValues.items())
    
    result = [ (p[0], __most_frequent(p[1])) for p in predValues.items() ]
    return result

def check_data(inData, predValues):
    if len(inData) != len(predValues):
        print("Different sizes of data", len(inData), len(predValues))
        sys.exit(-1)

    for (dataId, (predId, _)) in zip(inData["id"], predValues):
        if dataId != predId:
            print("Different IDs:", dataId, predId)
            sys.exit(-1)

def evaluate_label(inData, predValues, label):
    vals = [ p[1] for p in predValues ]
    score = f1_score(inData[label], vals, average='micro')
    return score
           
if __name__ == '__main__':
    import os, sys
    
    args = sys.argv[1:]
    
    inFile = args[0]
    predFile =  args[1]
    label = args[2]

    if len(args) > 3:
        evalMore = args[3]
    else:
        evalMore = None

    inData = pd.read_json(inFile, orient='records', lines=True, dtype=False)
    inData['id'] = inData['id'].astype('str')

    predValues = read_preds(predFile)
    predValues = convert_preds(predValues, evalMore)
    
    # sorting for evaluation
    inData = inData.sort_values("id")
    predValues.sort()
    
    check_data(inData, predValues)

    score = evaluate_label(inData, predValues, label)
    print("PRED ACC: %f" % score)
