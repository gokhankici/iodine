#!/usr/bin/env python3

import sys
import warnings
import numpy          as np
import networkx       as nx
import collections
from   scipy.optimize import linprog
from   scipy.sparse   import csr_matrix, csc_matrix, vstack, hstack

from flow_capacity import get_edge_capacities
from utils         import *
from tests         import get_test

def calc_costs(g):
    roots    = [v for v in g.nodes() if len(g.pred[v]) == 0]
    costs    = collections.defaultdict(int)
    worklist = collections.deque(roots)
    done     = set(roots)
    while worklist:
        v = worklist.popleft()
        c = sum(map(lambda u: costs[u], g.predecessors(v))) + 1
        costs[v] = c
        for u in g.successors(v):
            if u not in done:
                worklist.append(u)
                done.add(u)
    return costs

def suggest_assumptions(g, must_eq, cannot_be_eq):
    """
    Given a graph and two sets, suggest the nodes to be marked
    must_eq      : nodes that must be marked (directly or indirectly)
    cannot_be_eq : nodes that cannot be marked
    """
    cap          = get_edge_capacities(g)
    costs        = calc_costs(g)
    shadow_nodes = [v for v in g.nodes() if len(g.succ[v]) > 0]

    # give ids to the edges in the transformed graph
    def give_edge_ids():
        """
        Keys are the regular edges and nodes that have a successor (i.e. shadow nodes)
        i.e. be careful the key can be a tuple or a int
        """
        n = 0
        edge_id = {}
        for e in g.edges():
            edge_id[e] = n
            n += 1
        for v in shadow_nodes:
            edge_id[v] = n
            n += 1
        return edge_id
    edge_id = give_edge_ids()
    debug("edge ids:\n{}".format(edge_id))
    edge_count = len(edge_id)

    # the upper bounds of the flows are the capacities
    upper_bound = np.full(edge_count, 0)
    for e, i in edge_id.items():
        if type(e) == int:
            v = e               # this is a shadow node
            upper_bound[i] = sum(map(lambda w: cap[v,w], g.successors(v)))
        else:
            upper_bound[i] = cap[e]
    debug("upper bound:\n{}".format(upper_bound))

    # calculate the cost of every shadow edge
    lp_cost = np.full(edge_count, 0)
    for v in shadow_nodes:
        lp_cost[edge_id[v]] = costs[v]
    for v in cannot_be_eq:
        for w in g.successors(v):
            for u in g.predecessors(w):
                lp_cost[edge_id[u]] = costs[w] + 1
    debug("lp cost:\n{}".format(lp_cost))

    # the variables of nodes that cannot be marked has to be zero
    a_eq_0 = csr_matrix((1, edge_count))
    b_eq_0 = 0
    for v in shadow_nodes:
        if v in cannot_be_eq:
            a_eq_0[0, edge_id[v]] = 1

    # the outgoing flow of must_eq nodes have to be at capacity
    a_eq_1 = csr_matrix((1, edge_count))
    b_eq_1 = 0
    for v in must_eq:
        for w in g.successors(v):
            a_eq_1[0, edge_id[v,w]] = 1
        b_eq_1 += upper_bound[edge_id[v]]

    a_eq = vstack([a_eq_0, a_eq_1])
    b_eq = np.array([b_eq_0, b_eq_1])

    # regular max flow constraints
    a_ub = csr_matrix((0, edge_count))
    b_ub = np.array([])
    for v in g.nodes():
        if len(g.succ[v]) == 0:
            continue

        vec = csr_matrix((1, edge_count))

        for u in g.predecessors(v):
            i = edge_id[u,v]
            vec[0, i] = -1
        vec[0, edge_id[v]] = -1
        for w in g.successors(v):
            i = edge_id[v,w]
            vec[0, i] = 1

        a_ub = vstack([a_ub, vec])
        b_ub = np.append(b_ub, 0)

    debug("equalities:")
    debug(a_eq.A)
    debug(b_eq)
    debug("upper bounds:")
    debug(a_ub.A)
    debug(b_ub)

    # solve the LP problem
    result = linprog(lp_cost,
                    A_eq = a_eq, b_eq = b_eq,
                    A_ub = a_ub, b_ub = b_ub,
                    bounds = [(0,ub) for ub in upper_bound],
                    method = "interior-point",
                    options = {"sparse":True})
    if result.status != 0:
        print("Linear programming failed: {}".format(result.message))
        return None
    else:
        debug(result)
        return {v : int(round(result.x[edge_id[v]])) for v in shadow_nodes}

def main(edges, must_eq, cannot_be_eq):
    g = make_test_graph(edges)

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        result = suggest_assumptions(g, must_eq, cannot_be_eq)
        if result:
            print("Marked nodes:\n{}".format([v for v,n in result.items() if n > 0]))
        else:
            print("No solution exists...")

if __name__ == "__main__":
    test_no = 0 if len(sys.argv) <= 1 else int(sys.argv[1])
    edges, must_eq, cannot_be_eq = get_test(test_no)
    main(edges, must_eq, cannot_be_eq)
