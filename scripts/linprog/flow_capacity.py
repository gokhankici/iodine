#!/usr/bin/env python3

import sys
import warnings
import numpy          as np
import networkx       as nx
import cplex
import json
from   collections    import namedtuple
from   utils          import *

class CplexFlowCapSolver:
    def __init__(self):
        self.result_type = namedtuple("CplexCapacity", ["capacities", "extra_nodes", "extra_edges", "new_graph"])

    def to_result_type(self, *args):
        nt = self.result_type
        return nt(*args)

    def enum_edges(self, g):
        """
        Return a map that gives an id to every edge starting at 0
        """
        return dict(map((lambda t: (t[1],t[0])), enumerate(g.edges())))

    def make_extra_edges(self, g):
        """
        Create extra edges that are useful when the capacity formulation is infeasible by default
        """
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

    def make_result(self, edge_id, values, orig_graph):
        new_graph   = orig_graph.copy()
        capacities  = {}
        extra_nodes = set()
        extra_edges = {}
        for e, i in edge_id.items():
            cap = int(round(values[i]))
            if cap < 1:
                continue
            elif self.is_extra_edge(e):
                u,v = e
                new_graph.add_edge(u, v, dict(type = "extra"))
                ve = self.get_extra_node(e)
                extra_nodes.add(ve)
                extra_edges[e] = ve
            capacities[e] = cap
        return self.to_result_type(capacities, extra_nodes, extra_edges, new_graph)

    def get_edge_capacities(self, orig_g):
        g        = self.make_extra_edges(orig_g)
        edge_id  = self.enum_edges(g)
        edge_cnt = len(edge_id)

        prob = cplex.Cplex()
        prob.set_results_stream(None)
        prob.set_log_stream(None)

        # objective is to minimize
        prob.objective.set_sense(prob.objective.sense.minimize)

        obj = [1] * edge_cnt
        lb  = [1] * edge_cnt
        for e, i in edge_id.items():
            if self.is_extra_edge(e):
                u, v   = e
                ve     = self.get_extra_node(e)
                lb[i]  = 0
                obj[i] = edge_cnt * 5000 if u == ve else edge_cnt * 1000

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
            return self.make_result(edge_id, values, orig_g)
        else:
            print("[Capacities] Linear programming failed: {}".format(status))
            sys.exit(1)

if __name__ == "__main__":
    cplex_in   = parse_cplex_input(sys.argv[1])
    names      = cplex_in["names"]
    g          = cplex_in["graph"]
    cc         = CplexFlowCapSolver().get_edge_capacities(g)
    capacities = cc.capacities

    for e, ve in cc.extra_edges.items():
        u, v = e
        if u == ve:
            name = names[v]
            typ  = "incoming to"
        else:
            name = names[u]
            typ  = "outgoing from"
        print("{:<15} {:<30} {:>}".format(typ, name, capacities[e]))
        names[ve] = "{}###{}".format(name, "in" if u == ve else "out")

    new_g = cc.new_graph
    for u,v,k in new_g.edges:
        e = (u,v)
        if e in capacities:
            new_g.edges[u,v,k]["label"] = capacities[e]

    components(new_g, names, visualize=False)
