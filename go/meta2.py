"""
META-II interpreter in python
"""
import sys, shlex, logging

def unq(s):
    """unquote. converts "'a'" to "a" """
    return s[1:-1] if s[0]==s[-1]=="'" else s

class DoneParsing(BaseException): pass
class ParseError(BaseException):
    def __init__(self, stack):
        super(ParseError,self).__init__()
        self.stack=stack



def interp(ops, labels, txt, emit):
    """interpret meta-ii assembly language"""
    nc, ok, cp, oldcp, tok, out = 0, 1, 0, 0, '', ''
    ops+=[('END','')]; txt+='/' # prevent bounds checking errors
    def run(ip, stack, gn1='',gn2=''): # borrow python's stack for calls/labels
        nonlocal nc, ok, cp, oldcp, out
        while True:
            while txt[cp]<=' ': cp+=1  # skip whitespace
            op,arg=ops[ip]

            ## mini debugger
            logging.debug('ip: %3i %3s %s'%(ip,op,arg))
            input(repr(stack)+'>')


# interp > run > opcode handlers

            if op=='END': raise DoneParsing
            elif op == 'R': return ok
            elif (op in['B','ADR']) or (op=='BT' and ok) or (op=='BF' and not ok):
                ip=labels[arg]-1
            elif op=='BE':
                if not ok: raise ParseError(stack)
            elif op=='SET': ok=1
            elif op=='CL': out+=unq(arg)
            elif op=='CI': out+=tok
            elif op=='GN1':
                if not gn1:
                    gn1='L'+str(nc); nc+=1
                out+=gn1
            elif op=='GN2':
                if not gn2:
                    gn2='L'+str(nc); nc+=1
                out+=gn2
            elif op=='LB': out=''
            elif op=='OUT':
                emit(out); out='\t'
            elif op=='CLL': run(ip=labels[arg],stack=stack+[arg])
            elif op=='TST':
                ok,arg=0,unq(arg)
                if txt[cp:cp+len(arg)]==arg:
                    ok=1; tok=arg; cp+=len(arg)
            elif op in ['NUM','SR','ID']:
                oldcp,buf=cp,''
                if op=='NUM':
                    while txt[cp].isdigit():
                        buf+=txt[cp]; cp+=1
                if op=='SR': # match string
                    if txt[cp]=="'":
                        while txt[cp]!="'":
                            buf+=txt[cp]; cp+=1
                        cp+=1
                else: # 'ID'
                    if txt[cp].isalpha():
                        while txt[cp].isalnum():
                            buf+=txt[cp]; cp+=1
                if buf:
                    ok,tok = 1,buf # consume token
                    logging.debug('consumed: %r', buf)
                else: ok,cp = 0,oldcp  # backtrack
            elif op not in ['BF','BT']: print('unknown opcode:', op)
            ip+=1

    try: run(ip=0,stack=[])
    except DoneParsing: pass
    except ParseError as e:
        print("\n!!! parse error !!!\n\nstack: %r\n" % e.stack)
        print("-- text parsed so far: -------")
        print(txt[:oldcp])
        print("------------------------------")


def main(src, txt, emit=lambda s:sys.stdout.write(s+'\n')):
    """
    src is assembly language code for the meta-ii machine.
    txt is the input text on which to run the meta-ii code.
    emit is function to call for output strings
    """
    code,labels = [],{}

    # first pass extracts code and labels from source
    for line in src.split("\n"):
        if line[:1].isspace():
            # shlex is easy way to get "tst 'a string'" parssing as 2 tokens
            row = list(shlex.shlex(line))
            if len(row)==1: row+=[''] # fake arg so we don't have to test
            if row: code.append(row)
        else: labels[line.strip()]=len(code)

    # second pass interprets the code to process the text
    interp(code,labels,txt,emit)

def read(path):
    return sys.stdin.read() if path=='-' else open(path).read()


#-- test suite ---

def prep(s):                             # test case syntax:
    return '\n'.join(                    # ---------------------
        line[1:] if line[0]==':'         # ':' to signal a label
        else '   '+line                  # no need to indent
        for line in s.split(';'))        # use ';' for '\n'

cases =[                                 # each test case is (src, txt, goal)
    ("CL 'hello';OUT", '', 'hello'),
    ("CL 'X';B SKIP;CL 'Y';:SKIP;OUT", '', 'X'),
    (':bye;END', '', '')]

def test():
    for src,txt,goal in cases:
        out=[]
        main(prep(src), txt, out.append)
        actual='\n'.join(out)
        if actual!=goal:
            print('wanted:', goal)
            print('actual:', actual)
            print('   src:', src)
            print('   txt:', txt)

if __name__=="__main__":
    if '-t' in sys.argv: test()
    else:
        if '-d' in sys.argv:
            logging.basicConfig(level=logging.DEBUG, stream=sys.stdout)
            sys.argv.remove('-d')
        if len(sys.argv) == 3:
            src, txt = [read(f) for f in sys.argv[1:]]
            main(src,txt)
        else: print('usage: meta2.py srcfile txtfile')
