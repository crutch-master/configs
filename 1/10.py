import pathlib
import sys
import os

paths = list(filter(os.path.isfile, pathlib.Path(sys.argv[1]).rglob("*")))

for file in paths:
    if os.stat(file).st_size == 0:
        print(file)
