#!/usr/bin/env python3.6

from utils import parse_cplex_input


parsed = parse_cplex_input("cplex.json")
g = parsed["graph"]
names = parsed["names"]
inv_names = parsed["inv_names"]
is_reg = parsed["is_reg"]


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
