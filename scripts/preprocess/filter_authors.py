import random, json

def read_to_select(filename):
    result = { }
    for line in open(filename, "r"):
        line = line.split("#")[0]
        age, gender, train, test = line.split()

        if gender not in result:
            result[gender] = { }

        result[gender][age] = (int(train), int(test))

    return result

def read_data(filename, to_filter):
    result = {"male" : { "10s" : { }, "20s" : { }, "30s" : { } }, 
              "female" : { "10s" : { }, "20s" : { }, "30s" : { } }
              }

    for line in open(filename):
        conv = json.loads(line)

        # without all sex offenders
        if any([x in conv for x in to_filter]):
            continue

        result[conv["gender"]][conv["age"]][conv["id"]] = line

    return result

if __name__ == '__main__':
    import os, sys

    args = sys.argv[1:]
    
    inFile = args[0]
    toSelectFile = args[1]

    corpora = args[2]
    outFile = args[3]
    toFilter = args[4].split(",")

    if corpora not in [ "train", "test" ]:
        print("Unknown corpora", corpora)
        sys.exit(-1)

    toSelect = read_to_select(toSelectFile)
    allData = read_data(inFile, toFilter)
    
    outData = [ ]
    for gender in toSelect:
        for age in toSelect[gender]:
            trainSize, testSize = toSelect[gender][age]
            data =  list(allData[gender][age].values())
            random.shuffle(data)

            if corpora == "train":
                toTake = trainSize
            elif corpora == "test":
                toTake = testSize
           
            if len(data) < toTake:
                print("Not enough data", len(data), gender, age, toTake)
                toTake = len(data)
                
            outData += data[:toTake]
            
    with open(outFile, "w") as out:
        for line in outData:
            out.write(line)
        
    print("Result sizes", len(outData))
