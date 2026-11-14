import random
import json
import projpath

from common import reader
from select_data import read_data

if __name__ == '__main__':
    import os, sys

    args = sys.argv[1:]
    
    inFile = args[0]

    outTrainFile = args[1]
    outValidFile = args[2]
    validPerc = float(args[3])

    dictData = read_data(inFile)
    
    trainData = [ ]
    validData = [ ]

    for gender, genderData in dictData.items():
        for age, ageData in genderData.items():
            allData = list(ageData.values())
            random.shuffle(allData)

            validSize = int(validPerc * len(allData))
            validData += allData[:validSize]
            trainData += allData[validSize:]
    
    with open(outTrainFile, "w") as out:
        for line in trainData:
            out.write(line)
        
    with open(outValidFile, "w") as vout:
        for line in validData:
            vout.write(line)

    print("Result sizes", len(trainData), len(validData))