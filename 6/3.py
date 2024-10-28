from sys import argv
from json import loads

with open(argv[1], 'r') as source:
    obj = loads(source.read())

    print("root:")
    print("ifeq (,$(wildcard ./makefiles))")
    print("\t@mkdir makefiles")
    print("endif")
    print()

    print("clean:")
    print("\t@rm -rf makefiles")
    print()

    for key in obj:
        deps = " ".join(obj[key])
        print(f"{key}: {deps} root")
        print(f"ifeq (,$(wildcard ./makefiles/{key}))")
        print(f"\t@echo {key}")
        print(f"\t@touch ./makefiles/{key}")
        print(f"endif")
        print()
