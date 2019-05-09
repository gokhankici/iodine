#!/usr/bin/env python3.6
# vim: set foldmethod=marker:

import sys
import warnings
import networkx       as nx
import collections
import cplex
import subprocess
import time

from   flow_capacity import CplexFlowCapSolver
from   utils         import *
from   tests         import get_test

class AssumptionSolver:
    def __init__(self, g, must_eq, cannot_be_eq):
        self.must_eq      = list(set(must_eq))
        self.cannot_be_eq = list(set(cannot_be_eq))

        cc             = CplexFlowCapSolver().get_edge_capacities(g)
        self.cap       = cc.capacities
        self.cap_nodes = cc.extra_nodes
        self.g         = cc.new_graph

        self.shadow_nodes = [ v
                              for v in g.nodes()
                              if v not in self.cap_nodes and len(g.succ[v]) > 0 ]

        # give ids to the edges
        n = 0
        edge_id = {}
        for e in g.edges():
            edge_id[e] = n
            n += 1
        for v in self.shadow_nodes:
            edge_id[v] = n
            n += 1
        self.edge_id    = edge_id
        self.edge_count = len(edge_id)

        # calculate costs of the nodes
        self.calc_costs()


    def calc_costs(self):
        """
        Returns a mapping from node ids to their costs
        """
        roots      = [v for v in self.g.nodes() if len(self.g.pred[v]) == 0]
        self.costs = collections.defaultdict(int)
        worklist   = collections.deque(roots)
        done       = set(roots)

        while worklist:
            v = worklist.popleft()
            c = sum(map(lambda u: self.costs[u], self.g.predecessors(v))) + 1
            self.costs[v] = c
            for u in self.g.successors(v):
                if u not in done:
                    worklist.append(u)
                    done.add(u)

class CplexAssumptionSolver(AssumptionSolver):
    def __init__(self, g, must_eq, cannot_be_eq):
        super(CplexAssumptionSolver, self).__init__(g, must_eq, cannot_be_eq)

    def suggest_assumptions(self):
        prob = cplex.Cplex()
        prob.set_results_stream(None)
        prob.set_log_stream(None)

        # objective is to minimize
        prob.objective.set_sense(prob.objective.sense.minimize)

        t0 = time.perf_counter()

        # compute objective coefficients
        obj = [0] * self.edge_count
        for v in self.shadow_nodes:
            obj[self.edge_id[v]] = self.costs[v]
        for v in self.cannot_be_eq:
            for w in self.g.successors(v):
                for u in self.g.predecessors(w):
                    obj[self.edge_id[u]] = self.costs[w] + 1

        # compute upper bounds on variables
        ub = [0] * self.edge_count
        for e,i in self.edge_id.items():
            if type(e) == int:
                v = e           # this is a shadow node
                ub[i] = sum(map(lambda w: self.cap[v,w], self.g.successors(v)))
            else:
                ub[i] = self.cap[e]

        # update cost and upper bound
        prob.variables.add(obj = obj, ub = ub)

        # the variables of nodes that cannot be marked has to be zero
        indices1 = [ self.edge_id[v]
                     for v in self.cannot_be_eq
                     if len(self.g.succ[v]) > 0 ]
        prob.linear_constraints.add(lin_expr = [[indices1, [1] * len(indices1)]],
                                    senses   = "E",
                                    rhs      = [0])

        # the outgoing flow of must_eq nodes have to be at capacity
        indices2 = [ self.edge_id[v,w]
                     for v in self.must_eq
                     for w in self.g.successors(v) ]
        prob.linear_constraints.add(lin_expr = [[indices2, [1] * len(indices2)]],
                                    senses   = "E",
                                    rhs      = [ sum([ ub[self.edge_id[v]]
                                                       for v in self.must_eq ]) ])

        # regular max flow constraints
        for v in self.g.nodes():
            if len(self.g.succ[v]) == 0:
                continue
            indices      = [self.edge_id[v]]
            coefficients = [-1]
            for u in self.g.predecessors(v):
                indices.append(self.edge_id[u,v])
                coefficients.append(-1)
            for w in self.g.successors(v):
                indices.append(self.edge_id[v,w])
                coefficients.append(1)
            prob.linear_constraints.add(lin_expr = [[indices, coefficients]],
                                        senses   = "L",
                                        rhs      = [0])

        prob.solve()
        sol = prob.solution
        status = sol.get_status_string()

        t1 = time.perf_counter()
        print("elapsed time: {} ms".format(int((t1-t0) * 1000)))

        prob.write("assumptions.lp")
        if status == "optimal":
            values = sol.get_values()
            return set([ v for v in self.shadow_nodes if int(round(values[self.edge_id[v]])) > 0 ])
        else:
            print("linprog failed: {}".format(status))
            sys.exit(1)

def main(filename):
    parsed       = parse_cplex_input(filename)
    g            = parsed["graph"]
    must_eq      = parsed["must_eq"]
    cannot_be_eq = parsed["cannot_be_eq"]
    names        = parsed["names"]
    inv_names    = parsed["inv_names"]

    print("Must equal   :")
    for v in sorted([names[v] for v in must_eq]):
        print("  {}".format(v))

    def l2s(l):
        return ", ".join(l)

    write_dot_file(g, names)

    result = CplexAssumptionSolver(g, must_eq, cannot_be_eq).suggest_assumptions()

    def validate_marked_nodes():
        always_eq = result.copy()
        # always_eq.add(inv_names["id_class"])

        worklist = set([ w for v in result for w in g.succ[v] ])

        while len(worklist) > 0:
            v    = worklist.pop()
            name = names[v]

            before = v in always_eq
            after  = all([u in always_eq for u in g.pred[v]])

            if before:
                continue
            elif after:
                always_eq.add(v)
                print("[+++++] {:35}".format(name))
                worklist.update(set(g.succ[v]))
            else:
                print("[-----] {:35} : {}".format(name, ", ".join([ names[u] for u in g.pred[v] if u not in always_eq ])))

        print("\n\n\n")
        err = False
        for v in must_eq:
            if v not in always_eq:
                print("VALIDATION ERROR: {:<35} IS NOT ALWAYS EQUAL !".format(names[v]))
                err = True
        if err:
            sys.exit(1)

    validate_marked_nodes()

    if result:
        print("Marked nodes : {}".format(l2s([ names[v] for v in result ])))
    else:
        print("No solution exists...")
    return result

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("usage: assumptions.py <cplex.json>")
        sys.exit(1)
    else:
        main(sys.argv[1])
