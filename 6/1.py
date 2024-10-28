from sys import argv
from json import loads

with open(argv[1], 'r') as source:
    obj = loads(source.read())

    for key in obj:
        deps = " ".join(obj[key])
        print(f"{key}: {deps}")
        print(f"\t@echo \"{key}\"\n")
