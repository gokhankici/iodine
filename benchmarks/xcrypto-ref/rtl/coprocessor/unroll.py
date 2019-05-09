#!/usr/bin/env python3

import sys

def make_assign(var, n):
    variables = [ f"{var}_{i}" for i in range(n-1, -1, -1) ]
    concat = "{%s}" % ", ".join(variables)
    return f"assign {var} = {concat};"

def adder_unroll():
    n = 32

    for i in range(n):
        im1 = i - 1
        ip1 = i + 1
        text = f"""
wire [1:0] ir_{i}   = a[{i}] +
                      (sub ? !b[{i}] : b[{i}]) +
                      (fci_{i} ? 1'b1  : carry_bits_{i});
wire fci_{i} = {i} == 0 ? 1'b0 : sub && !carry_msk_{i} && carry_msk_{im1};
wire carry_msk_{i} = ({i} & pw_1hot) != 0 || {i} == 0;
wire result_bits_{i} = ir_{i}[0];
wire carry_bits_{ip1} = ir_{i}[1] && carry_msk_{ip1};"""
        print(text)

    print()
    print(make_assign("fci", 32))
    print(make_assign("result_bits", 32))
    print(make_assign("carry_bits", 33))
    print(make_assign("carry_msk", 33))

def palu_1():
    n = 32
    for br in range(n):
        text = f"""wire bop_result_{br} = id_imm[
    {{palu_rs3[{br}], palu_rs1[{br}], palu_rs2[{br}]}}
];"""
        print(text)
    print()
    print(make_assign("bop_result", 32))

def palu_2():
    n = 16

    for s in range(16):
        s4   = s * 4
        s4p3 = s4 + 3
        if s < 8:
            text1 = f"""assign lut_result_{s} = lut_lut[palu_rs1[{s4p3} : {s4}]];"""
            print(text1)
        text2 = f"""assign lut_lut_{s} = lut_concat[{s4p3}: {s4}];"""
        print(text2)
    print(make_assign("lut_result", 8))
    print(make_assign("lut_lut", 16))
        
        
arg = sys.argv[1]

if arg == "adder":
    adder_unroll()
elif arg == "palu1":
    palu_1()
elif arg == "palu2":
    palu_2()
