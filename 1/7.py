import filecmp
import pathlib
import sys
import os

paths = list(filter(os.path.isfile, pathlib.Path(sys.argv[1]).rglob("*")))

for path1 in paths:
    count = 0

    for path2 in paths:
        count += 1 if filecmp.cmp(path1, path2) else 0

    if count > 1:
        print(path1)
