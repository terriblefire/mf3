#!/usr/bin/python

import sys,os
from struct import unpack

fileName = sys.argv[1]
loc = 0

f = open(sys.argv[1])

i = 0

for line in f:
    tokens = line.strip().split()

    if ((len(tokens) > 0) and (tokens[0] == "defc")):
        tok2 = tokens[1].split('=')
        print "%s: equ %s" % (tok2[0],tok2[1].replace("$","0x"))
    else:
        print line,
        
    
