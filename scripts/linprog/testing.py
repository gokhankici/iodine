#!/usr/bin/env python3.6

from   collections import deque
import json
import subprocess
import networkx    as nx
import sys
from   enum        import Enum
from   utils       import *

class Adj(Enum):
    PARENT = 1
    CHILD  = 2

parsed       = parse_cplex_input("cplex.json")
g            = parsed["graph"]
names        = parsed["names"]
inv_names    = parsed["inv_names"]
is_reg       = parsed["is_reg"]

def to_node_id(arg):
    if isinstance(arg, int):
        v = arg
    elif isinstance(arg, str):
        try:
            v = inv_names[arg]
        except KeyError:
            v = None
    else:
        v = None

    if not (v and g.has_node(v)):
        return None

    return v

def is_direct_edge(u, v):
    for _, data in g.get_edge_data(u, v).items():
        t = data["type"]
        if t == "Direct":
            return True
    return False

def is_implicit_edge(u, v):
    for _, data in g.get_edge_data(u, v).items():
        t = data["type"]
        if t == "Implicit":
            return True
    return False

def mk_adj(v, typ):
    if not isinstance(typ, Adj):
        raise Exception("typ should be an Adj, given: {}".format(typ))

    directs   = []
    implicits = []

    go = g.predecessors if typ == Adj.PARENT else g.successors

    for u in go(v):
        src, tgt = (u,v) if typ == Adj.PARENT else (v,u)
        if is_direct_edge(src, tgt):
            directs.append(u)
        if is_implicit_edge(src, tgt):
            implicits.append(u)

    return (directs, implicits)


def print_node(node_id, keep_printing_children=False):
    def helper(msg, typ):
        directs, implicits = mk_adj(node_id, typ=typ)

        print("  {}:".format(msg))
        if directs:
            print("    direct   : {}".format(", ".join([names[v] for v in directs])))
        if implicits:
            print("    implicit : {}".format(", ".join([names[v] for v in implicits])))

    print("{}:".format(names[node_id]))
    helper("parents", Adj.PARENT)
    helper("children", Adj.CHILD)
    print("")
    
    if keep_printing_children:
        ds, _ = mk_adj(node_id, typ=Adj.CHILD)
        if len(ds) == 1:
            print_node(ds[0], keep_printing_children=keep_printing_children)

def print_nodes():
    for node_id in g.nodes():
        print_node(node_id)

def print_roots():
    for node_id in g.nodes():
        (directs, implicits) = mk_adj(node_id, Adj.PARENT)
        if not directs and not implicits:
            print_node(node_id)

def loop():
    def parse_input(l):
        l = line.strip()

        try:
            no = int(l)
            if not g.has_node(no):
                no = None
        except ValueError:
            if l in inv_names:
                no = inv_names[l]
            else:
                no = None

        return no

    for line in sys.stdin:
        no = parse_input(line)
        if no:
            print("")
            print_node(no, keep_printing_children=True)

def print_edge(u,v):
    args = {"d"   : "D" if is_direct_edge(u, v)   else "=",
            "i"   : "I" if is_implicit_edge(u, v) else "=",
            "src" : names[u],
            "tgt" : names[v] }
    print("{src:>30} ={d}={i}=> {tgt}".format(**args))

def shortest_path(s, t):
    src = to_node_id(s)
    tgt = to_node_id(t)

    paths = [ nx.algorithms.shortest_paths.generic.shortest_path(g, source=src, target=tgt) ]
    
    for path in paths:
        prev_node = None
        for v in path:
            if prev_node:
                print("={}={}=> {} ".format("D" if is_direct_edge(prev_node, v)   else "=",
                                            "I" if is_implicit_edge(prev_node, v) else "=",
                                            names[v]))
            else:
                print("       {} ".format(names[v]))
            prev_node = v
        
        print("\n")
    
def print_paths():
    shortest_path("m__mem_array", "IF_PC_d_out")
    # shortest_path("IF_PC_d_out", "WB_wd_reg")
    # shortest_path("reset", "WB_wd_reg")
    shortest_path("m__mem_array", "WB_wd_reg")

def print_all_edges(s=None, t=None, n=None):
    src = to_node_id(s) if s else None
    tgt = to_node_id(t) if t else None
    n   = to_node_id(n) if n else None

    for u,v in sorted(g.edges()):
        if (src and src != u) or \
           (tgt and tgt != v) or \
           (n and u != n and v != n):
            continue
        print_edge(u,v)

def loop2():
    for line in sys.stdin:
        l     = line.strip().split()
        pairs = zip(l[::2], l[1::2])
        args  = { y : x for x,y in pairs }
        print_all_edges(**args)

def find_parent_regs(node_id):
    regs     = set()
    worklist = deque([node_id])
    seen     = set([node_id])

    while worklist:
        v = worklist.popleft()
        for u in g.predecessors(v):
            if is_reg[u]:
                regs.add(u)
            elif u not in seen:
                seen.add(u)
                worklist.append(u)
    
    return regs

def loop3():
    for line in sys.stdin:
        n = to_node_id(line.strip())
        if not n:
            print("--nope--")
        else:
            print("parents: {}".format(", ".join([names[v] for v in find_parent_regs(n)])))
        print("")

def mk_qualifier_pairs():
    vs = "m_EX_ALU_result EX_extend EX_rs m__ALUOperation MEM_RegWrite MEM_RegRd ForwardA EX_rt m_FMUXA_y WB_RegWrite WB_RegRd ForwardB m_FMUXB_y EX_ALUOp EX_ALUSrc m_EX_ALU_result EX_extend EX_rs m__ALUOperation MEM_RegWrite MEM_RegRd EX_MemRead EX_rt ForwardA WB_RegWrite WB_RegRd m_FMUXA_y ForwardB m_FMUXB_y EX_ALUOp EX_ALUSrc m_EX_ALU_result EX_extend EX_rs m__ALUOperation MEM_RegWrite MEM_RegRd ForwardA EX_rt m_FMUXA_y WB_RegWrite WB_RegRd ForwardB m_FMUXB_y EX_ALUOp EX_ALUSrc EX_MemWrite m_EX_ALU_result EX_rd2 EX_extend EX_rs m__ALUOperation MEM_RegWrite MEM_RegRd ForwardA EX_rt m_FMUXA_y WB_RegWrite WB_RegRd ForwardB m_FMUXB_y EX_ALUOp EX_ALUSrc"
    vs = set(vs.split())
    print("// @annot{{qualifierPairs([{}])}}".format(", ".join(vs)))

def main():
    # loop2()
    components(g,names)
    # mk_qualifier_pairs()
    # loop3()

print("")
main()
