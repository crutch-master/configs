import sys

with open(sys.argv[1], 'r') as file:
    string = file.read()

    for char in '\n;"(){},.#<>!/=':
        string = string.replace(char, ' ')

    print(" ".join(list(set(string.split()))))
