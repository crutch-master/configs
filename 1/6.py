import sys

with open(sys.argv[1]) as file:
    first_line = file.readline()
    comment = '#' if sys.argv[1].split('.')[-1] == 'py' else '//'
    print('yes' if first_line.startswith(comment) else 'no')
