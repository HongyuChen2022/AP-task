import json
import glob
import xml.etree.ElementTree as ET

def convert_xml_to_json(inFile, writer, metaId, corrGender, corrAge):
    tree = ET.parse(inFile)
    root = tree.getroot()

    json_conversation = {'gender': corrGender, 
                            'age': corrAge,
                            "id" : metaId }
    
    texts = [ ]

    # Iterate over the conversations in the XML
    for conversation in root.iter('conversation'):
        if conversation.text == None:
            print("Skippig empty conversation", id)
            continue

        
        texts.append(conversation.text)

    json_conversation["texts"] = texts
    json.dump(json_conversation, writer)
    writer.write("\n")
            
if __name__ == '__main__':
    import os, sys

    args = sys.argv[1:]
    
    inFile = args[0]
    inDir = args[1]
    lang = args[2]
    outFile = args[3]

    out = open(outFile, 'w')
    for line in open(inFile, "r"):
        #f5e20f1c82066b5d6801d889d146c4f9:::male:::30s:::pedophile
        fields = line.strip().split(":::")
        
        if len(fields) < 3:
            print("Lack of information: ", line)
            continue

        if len(fields) > 3:
            print("Skipping special case:", line)
            continue

        fId = fields[0]
        fGender = fields[1]
        fAge = fields[2]

        # 00851429b21722a4d62f63a328c601ca_en_20s_male
        fileName = inDir + "/" + fId + "_" + lang + "_*_*.xml"
        xmlFiles = glob.glob(fileName)
        
        if len(xmlFiles) > 1:
            print("More files than one", fileName)
        elif len(xmlFiles) == 0:
            print("No file", fileName)
        else:
            convert_xml_to_json(xmlFiles[0], out, fId, fGender, fAge)
            out.flush()

    out.close()