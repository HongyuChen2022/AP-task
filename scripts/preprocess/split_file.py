import random
import json
import projpath

from common import reader
from select_data import read_data

if __name__ == '__main__':
    import os, sys

    args = sys.argv[1:]
    
    inFile = args[0]
    outDir = args[1]
    folds = int(args[2])

    data = open(inFile).readlines()

    chunk_size = len(data) // folds
    chunks = [data[i:i + chunk_size] for i in range(0, len(data), chunk_size)]
    for chunkId, chunk in enumerate(chunks):
        chunkId += 1
        outfile = outDir + "/part_" + str(chunkId) + ".txt"

        with open(outfile, "w") as out:
            for line in chunk:
                out.write(line)