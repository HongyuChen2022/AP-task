import json
from bs4 import BeautifulSoup
import re

def normalize_text(text):
    # Remove HTML tags using BeautifulSoup
    soup = BeautifulSoup(text, "html.parser")
    clean_text = soup.get_text()

    # Remove URLs using regular expressions
    clean_text = re.sub(r"http\S+|www\S+|https\S+", " [LINK] ", clean_text)

    # Remove non-alphanumeric characters and convert to lowercase
    #clean_text = re.sub(r"[^a-zA-Z0-9]", " ", clean_text.lower())

    # Remove extra whitespaces
    clean_text = re.sub(r"\s+", " ", clean_text).strip()

    return clean_text

            
if __name__ == '__main__':
    import os, sys

    args = sys.argv[1:]
    
    inFile = args[0]
    outFile = args[1]

    writer = open(outFile, "w")
    for line in open(inFile):
        conv = json.loads(line)

        if conv["texts"] == None:
            continue

        conv["texts"] = [ normalize_text(text) for text in conv["texts"] ]
        
        json.dump(conv, writer)
        writer.write("\n")
        writer.flush()

    writer.close()
