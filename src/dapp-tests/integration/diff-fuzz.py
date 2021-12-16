import json
import os

from hypothesis import given, example, settings, note, target, Phase
from hypothesis.strategies import binary



@settings(
    deadline=10000,
    phases=[Phase.explicit, Phase.reuse]
)
@given(binary(min_size=1))
# these are meant without the prefix
@example(bytes.fromhex('60016000036000f3'))
@example(bytes.fromhex('65f3b2bd95ccaa4520ee607eb0825f'))
@example(bytes.fromhex('6101'))
@example(bytes.fromhex('32'))
@example(bytes.fromhex('04'))
@example(bytes.fromhex('33'))
@example(bytes.fromhex('30'))
@example(bytes.fromhex('45'))
@example(bytes.fromhex('46'))
@example(bytes.fromhex('4151'))
# @example(bytes.fromhex('303b3b')) (geth thinks `this` is cold, hevm disagrees)
@example(bytes.fromhex('6219000151'))
@example(bytes.fromhex('600134f3'))
@example(bytes.fromhex('600141fd'))
@example(bytes.fromhex('2d010101'))

# This example does not actually work right now because
# hevm and geth have a different interpretation of what
# happens when an empty contract is called:
# hevm visits the contract with an instant STOP
# while geth simply skips the call entirely and moves on
# to the next opcode in the calling contract.

#@example(bytes.fromhex('60006000601260136014601560166018f4'))

def test_compare_geth_hevm(b):
    code = b.hex()
    note("code that caused failure: ")
    note(code)
    # prepopulate the stack a bit
    x = os.system('evm --code ' + code + ' --gas 0xffffffffffffffff --json --receiver 0xacab --nomemory --prestate ./genesis.json run  > gethout')
    y = os.system('hevm exec --code ' + code + ' --gas 0xffffffffffffffff --chainid 0x539 --gaslimit 0xfffffffff --jsontrace --origin 0x73656e646572 --caller 0x73656e646572 > hevmout')
    assert x == y
    gethlines = open('gethout').read().split('\n')
    hevmlines = open('hevmout').read().split('\n')
    target(float(len(gethlines)))
    for i in range(len(hevmlines) - 3):
        gethline = gethlines[i]
        hevmline = hevmlines[i]
        hjson = json.loads(hevmline)
        gjson = json.loads(gethline)
        ## printed when diverging
        note('')
        note('--- STEP ----')
        note('geth thinks that')
        note(gethline)
        note('while hevm believes')
        note(hevmline)
        note('')

        assert hjson['pc']      == gjson['pc']
        assert hjson['stack']   == gjson['stack']
        # we can't compare memsize for now because geth
        # measures memory and memsize after the instruction,
        # as opposed to all other fields...
        # assert hjson['memSize'] == gjson['memSize']
        assert hjson['gas']     == gjson['gas']
    gethres = json.loads(gethlines[len(gethlines) - 2])
    hevmres = json.loads(hevmlines[len(hevmlines) - 2])
    note('--- OUTPUT ----')
    note('geth thinks that')
    note(gethres)
    note('while hevm believes')
    note(hevmres)
    assert gethres['output'] == hevmres['output']
    assert gethres['gasUsed'] == hevmres['gasUsed']

test_compare_geth_hevm()
