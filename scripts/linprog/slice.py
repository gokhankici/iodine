#!/usr/bin/env python3

# import collections
# import json
from utils import parse_cplex_input
import networkx as nx
# import subprocess
# import sys
# from config import DEBUG
# import functools

SLICE_VARIABLES = ["cmd", "busy", "round", "read_counter"]

def get_node_name(data, node):
    return data["names"][node]

def get_node_id(data, name):
    return data["inv_names"][name]

def add_dependencies(data, g, node, current_set):
    new_elements = set()

    if node not in current_set:
        new_elements.add(node)

    while new_elements:
        n = new_elements.pop()
        n_name = get_node_name(data, n)
        current_set.add(n)

        for p in g.pred[n]:
            d = set(d["type"] for d in g.get_edge_data(p, n).values())
            p_name = get_node_name(data, p)
            print(f"{n_name:>30} <-- {p_name:30} :: {', '.join(d)}")

            if p not in current_set:
                new_elements.add(p)

    return current_set

if __name__ == "__main__":
    cplex_file = "/home/rami/work/ctverilog/iodine_master/cplex.json"
    data = parse_cplex_input(cplex_file)
    g = data["graph"]

    nodes = set()
    for v in SLICE_VARIABLES:
        node = get_node_id(data, v)
        nodes = add_dependencies(data, g, node, nodes)

    print("\n\nAll variables in the slice:")
    for name in sorted(get_node_name(data, n) for n in nodes):
        print(f"  {name}")
