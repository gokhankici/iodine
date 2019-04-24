#!/usr/bin/env python3.6

import json
import subprocess

from   utils       import *

g, must_eq, cannot_be_eq, names = parse_file("cplex.json")

def mk_parent(v):
    directs   = []
    implicits = []

    for u in g.predecessors(v):
        for _, data in g.get_edge_data(u, v).items():
            t = data["type"]
            if t == "Direct":
                directs.append(names[u])
            elif t == "Implicit":
                implicits.append(names[u])
            else:
                print("illegal node data. node:{}, data:{}".format(u, data))
                sys.exit(1)

    result = {}
    if directs:
        result["direct"] = directs
    if implicits:
        result["implicit"] = implicits
    return result

def print_node(node_id):
    p = mk_parent(node_id)

    print("{}:".format(names[node_id]))

    if len(p) > 0:
        print("  parents:")
        if "direct" in p:
            print("    direct   : {}".format(", ".join(p["direct"])))
        if "implicit" in p:
            print("    implicit : {}".format(", ".join(p["implicit"])))
    else:
        print("  parents: []")

for v in g.nodes():
    print_node(v)
    print()
