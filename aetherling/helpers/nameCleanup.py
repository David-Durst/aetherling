def cleanName(name: str):
    return name.replace("(", "_").replace(")", "_").replace(",", "-")