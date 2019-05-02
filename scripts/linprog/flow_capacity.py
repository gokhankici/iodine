#!/usr/bin/env python3

import sys
import warnings
import numpy          as np
import networkx       as nx
import cplex
import json

from utils import *

class CplexFlowCapSolver:
    def enum_edges(self, g):
        return dict(map((lambda t: (t[1],t[0])), enumerate(g.edges())))

    def make_extra_edges(self, g):
        self.extra_nodes = set()
        self.extra_edges = {}

        g2 = g.copy()
        m  = max(g.nodes())

        for v in g.nodes():
            v_i = m + 1
            v_o = m + 2
            m += 2
            self.extra_nodes.update(set([v_i, v_o]))
            self.extra_edges[v] = (v_i, v_o)
            g2.add_edge(v_i, v)
            g2.add_edge(v, v_o)

        return g2

    def is_extra_edge(self, e):
        u, v = e
        return u in self.extra_nodes or v in self.extra_nodes

    def get_extra_node(self, e):
        u, v = e
        return u if u in self.extra_nodes else v

    def get_edge_capacities(self, orig_g):
        g        = self.make_extra_edges(orig_g)
        edge_id  = self.enum_edges(g)

        prob = cplex.Cplex()
        prob.set_results_stream(None)
        prob.set_log_stream(None)

        # objective is to minimize
        prob.objective.set_sense(prob.objective.sense.minimize)

        obj = [1] * len(edge_id)
        lb  = [1] * len(edge_id)
        for e, i in edge_id.items():
            if self.is_extra_edge(e):
                lb[i]  = 0
                obj[i] = len(edge_id) * 1000

        prob.variables.add(obj = obj, lb  = lb)

        for v in g.nodes():
            # do not generate a constraint for the roots
            # note: all([]) is True
            if all([w in self.extra_nodes for w in g.pred[v]]):
                continue
            # do not generate a constraint for the leaves
            elif all([w in self.extra_nodes for w in g.succ[v]]):
                continue

            indices      = []
            coefficients = []

            for u in g.predecessors(v):
                i = edge_id[u,v]
                indices.append(i)
                coefficients.append(-1)
            for w in g.successors(v):
                i = edge_id[v,w]
                indices.append(i)
                coefficients.append(1)

            # Ax = 0
            prob.linear_constraints.add(lin_expr = [[indices, coefficients]],
                                        senses   = "E",
                                        rhs      = [0])

        prob.solve()
        sol = prob.solution
        status = sol.get_status_string()
        prob.write("flow_capacity.lp")
        if status == "optimal":
            values = sol.get_values()
            capacities = {}
            extra_edges = {}
            for e, i in edge_id.items():
                cap = int(round(values[i]))
                if cap < 1:
                    continue
                elif self.is_extra_edge(e):
                    v = self.get_extra_node(e)
                    extra_edges[e] = v
                capacities[e] = cap
            return { "capacities": capacities, "extra_edges": extra_edges }
        else:
            print("[Capacities] Linear programming failed: {}".format(status))
            sys.exit(1)

def get_edge_capacities(g):
    defaultSolver = CplexFlowCapSolver()
    return defaultSolver.get_edge_capacities(g)

if __name__ == "__main__":
    cplex_in   = parse_cplex_input(sys.argv[1])
    names      = cplex_in["names"]
    g          = cplex_in["graph"]
    r          = get_edge_capacities(cplex_in["graph"])
    capacities = r["capacities"]

    for e, ve in r["extra_edges"].items():
        u, v = e
        g.add_edge(u, v)
        if u == ve:
            name = names[v]
            typ  = "incoming to"
        else:
            name = names[u]
            typ  = "outgoing from"
        print("{:<15} {:<30} {:>}".format(typ, name, capacities[e]))
        names[ve] = "{}###{}".format(name, "in" if u == ve else "out")

    for u,v,k in g.edges:
        e = (u,v)
        if e in capacities:
            g.edges[u,v,k]["label"] = capacities[e]

    components(g, names, visualize=False)

