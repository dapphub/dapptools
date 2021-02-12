#!/usr/bin/env python3

import json
import sys
import os

def test_compare_geth_hevm(code):
    # code = b.hex()
    # note("code that caused failure: ")
    # note(code)
    # prepopulate the stack a bit
    x = os.system('evm --code ' + code + ' --gas 0xfffffffff --json --create --nomemory --sender 0xa7d771818875e063c6f1848585ac36ee5c5c30ba run  > gethout')
    y = os.system('hevm exec --code ' + code + ' --gas 0xfffffffff --chainid 0x539 --gaslimit 0xfffffffff --jsontrace --origin 0xa7d771818875e063c6f1848585ac36ee5c5c30ba --caller 0x1f2a98889594024bffda3311cbe69728d392c06d --create > hevmout')
    assert x == y
    gethlines = open('gethout').read().split('\n')
    hevmlines = open('hevmout').read().split('\n')
    print(len(hevmlines))
    print(hevmlines[1593])
    print(hevmlines[1593])
    for i in range(max(len(hevmlines) - 3, len(gethlines))):
        gethline = gethlines[i]
        hevmline = hevmlines[i]
        hjson = json.loads(hevmline)
        gjson = json.loads(gethline)
        ## printed when diverging
        if (hjson['pc'] != gjson['pc'] or hjson['stack'] != gjson['stack'] or hjson['gas'] != gjson['gas']):
            print("FAILURE!")
            print("geth says!")
            print(gethline)
            print("but hevm thinks!")
            print(hevmline)
            exit(1)
    gethres = json.loads(gethlines[len(gethlines) - 2])
    hevmres = json.loads(hevmlines[len(hevmlines) - 2])
    assert gethres['output'] == hevmres['output']
    assert gethres['gasUsed'] == hevmres['gasUsed']

if len(sys.argv) != 2:
    print("Usage: " + sys.argv[0] + " <binfile>")
    sys.exit(1)
else:
    test_compare_geth_hevm(open(sys.argv[1]).read())

