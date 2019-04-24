#!/usr/bin/env python3.6

import json
from   graph_tool      import Graph
import graph_tool.draw

def parse_file(filename):
    def to_edge(l):
        return (l[0], l[1], dict(type=l[2]))

    with open(filename, 'r') as f:
        data = json.load(f)

    must_eq = data["must_eq"]

    if data["cannot_be_eq"]:
        inv_name     = { l[1] : l[0] for l in data["mapping"] }
        cannot_be_eq = [ inv_name[v] for v in data["cannot_be_eq"] ]
    else:
        cannot_be_eq = []

    g          = Graph(directed=True)
    edge_types = g.new_edge_property("string")
    node_names = g.new_vertex_property("string")

    for l in data["edges"]:
        src, tgt, typ = to_edge(l)
        e = g.add_edge(src, tgt, add_missing=True)
        edge_types[e] = typ

    for [node_id, node_name] in data["mapping"]:
        node_names[node_id] = node_name

    return (g, node_names, edge_types, must_eq, cannot_be_eq)

g, node_names, edge_types, must_eq, cannot_be_eq,  = parse_file("cplex.json")

graph_tool.draw.interactive_window(g)
