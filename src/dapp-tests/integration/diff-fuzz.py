from datetime import timedelta
import json
import os

from hypothesis import given, example, settings
from hypothesis.strategies import binary

@settings(max_examples=9999999999)
@given(binary(min_size=1))
@example(bytes.fromhex('60016000036000f3'))
@example(bytes.fromhex('65f3b2bd95ccaa4520ee607eb0825f'))
@example(bytes.fromhex('6101'))
@example(bytes.fromhex('32'))
@example(bytes.fromhex('33'))
@example(bytes.fromhex('30'))
@example(bytes.fromhex('45'))
@example(bytes.fromhex('46'))
@example(bytes.fromhex('4151'))
@example(bytes.fromhex('303b3b'))
def test_compare_geth_hevm(b):
    code = b.hex()
    print("code")
    print(code)
    x = os.system('evm --code ' + code + ' --gas 0xfffffffff --json --receiver 0xacab run > gethout')
    y = os.system('hevm exec --code ' + code + ' --gas 0xfffffffff --chainid 0x539 --gaslimit 0xfffffffff --jsontrace --origin 0x73656e646572 --caller 0x73656e646572 > hevmout')
    assert x == y
    gethlines = open('gethout').read().split('\n')
    hevmlines = open('hevmout').read().split('\n')
    for i in range(len(hevmlines) - 3):
        gethline = gethlines[i]
        hevmline = hevmlines[i]
        hjson = json.loads(hevmline)
        gjson = json.loads(gethline)
        # print('')
        # print('--- STEP ----')
        # print('geth thinks that')
        # print(gethline)
        # print('while hevm believes')
        # print(hevmline)
        # print('')

        assert hjson['pc']      == gjson['pc']
        assert hjson['stack']   == gjson['stack']
        # we can't compare memory for now because geth
        # uses an odd format
        # assert hjson['memory']  == gjson['memory']
        # assert hjson['memSize'] == gjson['memSize']
        assert hjson['gas']     == gjson['gas']
    gethres = json.loads(gethlines[len(gethlines) - 2])
    hevmres = json.loads(hevmlines[len(hevmlines) - 2])
    # print('--- OUTPUT ----')
    # print('geth thinks that')
    # print(gethres)
    # print('while hevm believes')
    # print(hevmres)
    assert gethres['output'] == hevmres['output']
    assert gethres['gasUsed'] == hevmres['gasUsed']
        
test_compare_geth_hevm()
