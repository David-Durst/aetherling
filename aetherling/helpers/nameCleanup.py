import re
def cleanName(name: str):
    return name.replace("(", "_").replace(")", "_").replace(",", "_").replace(" ", "").replace(":", "_").replace("-","_")\
        .replace("[", "_").replace("]","_").replace("=","_").replace(">", "_").replace("<", "_")

def undup_(name: str):
    return re.sub('__+', '_', name)
