import sys

with open(sys.argv[1], 'r') as input, open(sys.argv[2], 'w') as output:
    text = input.read().replace("    ", "\t")
    output.write(text)
