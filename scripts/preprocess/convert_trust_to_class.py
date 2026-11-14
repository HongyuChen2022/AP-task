import json
import pandas as pd
import math
    
def convert_trust_to_class(inFile, writer):
    with open(inFile, 'r') as data:
        for line in data:
            post = json.loads(line)
            post["text"] = post["texts"][0]
            del post["texts"]

            post["age_exact"] = post["ageExact"]
            del post["ageExact"]
            del post["ageRange"]

            json.dump(post, writer)
            writer.write("\n")
            
if __name__ == '__main__':
    import os, sys

    args = sys.argv[1:]
    
    inFile = args[0]
    outFile = args[1]

    convert_trust_to_class(inFile, open(outFile, 'w'))