"""
META-II interpreter in python
"""
import sys
import shlex

emit = sys.stdout.write

def unquote(s):
    return s[1:-1] if s[0]==s[-1]=="'" else s


labels = {}
listing = []

# first pass gathers code and assigns labels
for line in sys.stdin.read().split("\n"):
    if line[:1].isspace():
        row = list(shlex.shlex(line))
        if row: listing.append(row)
    else: labels[line.strip()]=len(listing)


# second pass interprets
stack = []; ip=0; ok=1
for row in listing:
    op = row[0]
    if op == 'END': break
    elif op == 'R': ip=stack.pop()
    elif op in ['ADR']: ip=labels[row[1]]
    elif op == 'OUT': emit('\n    ')
    elif op == 'BT': emit(unquote(row[1]))
    elif op == 'BF': pass
    elif op == 'BE': pass
    elif op == 'CI': pass
    elif op == 'CL': pass
    elif op == 'ID': pass
    elif op == 'SR': pass
    elif op == 'SET': ok=1
    else: print 'unknown op:', op
