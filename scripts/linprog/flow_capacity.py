#!/usr/bin/env python3

import sys
import warnings
import numpy          as np
import networkx       as nx
from   scipy.optimize import linprog
from   scipy.sparse   import csr_matrix, vstack

from utils import *

def generate_constraints(g):
    edge_id = enum_edges(g)
    c       = len(edge_id)
    a       = csr_matrix((0, c))

    for v in g.nodes():
        if len(g.pred[v]) == 0 or len(g.succ[v]) == 0:
            continue

        vec = csr_matrix((1, c))

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            for u in g.predecessors(v):
                i = edge_id[u,v]
                vec[0, i] = -1
            for w in g.successors(v):
                i = edge_id[v,w]
                vec[0, i] = 1

        a = vstack([a, vec])

    return (a, edge_id)

def get_edge_capacities(g):
    """
    Calculate the edge capacities to be used in the flow calculation.
    Precondition: Nodes should from the range [0, n)
    """
    a, edge_id = generate_constraints(g)
    r, c = a.shape
    result = linprog(np.full(c, 1),                 # cost, all 1
                    A_eq = a, b_eq = np.full(r, 0), # Ax = 0
                    bounds = [(1, np.inf)],         # x >= 1
                    method = "interior-point",      # better for sparse matrices
                    options = {"sparse":True})
    if result.status != 0:
        print("linprog failed:")
        print(result)
        sys.exit(1)
    cap = result.x
    # I hope this is correct
    return {e : int(round(cap[i])) for e, i in edge_id.items()}

# ##############################################################################
# ##############################################################################
# ##############################################################################

if __name__ == "__main__":
    edges = [(0,3), (1,3), (2,3),
             (3,4)]
    g   = make_test_graph(edges)
    cap = get_edge_capacities(g)
    print_graph(g)
    print(cap)
