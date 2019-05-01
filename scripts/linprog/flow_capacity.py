#!/usr/bin/env python3

import sys
import warnings
import numpy          as np
import networkx       as nx
from   scipy.optimize import linprog
from   scipy.sparse   import csr_matrix, vstack
import cplex

from utils import *

class ScipyFlowCapSolver:
    def generate_constraints(self, g):
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

    def get_edge_capacities(self, g):
        """
        Calculate the edge capacities to be used in the flow calculation.
        Precondition: Nodes should from the range [0, n)
        """
        a, edge_id = self.generate_constraints(g)
        r, c = a.shape
        result = linprog(np.full(c, 1),                 # cost, all 1
                        A_eq = a, b_eq = np.full(r, 0), # Ax = 0
                        bounds = [(1, np.inf)],         # x >= 1
                        method = "interior-point",      # better for sparse matrices
                        options = {"sparse":True})
        if result.status != 0:
            print("[Capacities] Linear programming failed:")
            print(result)
            sys.exit(1)
        cap = result.x
        # I hope this is correct
        return {e : int(round(cap[i])) for e, i in edge_id.items()}


class CplexFlowCapSolver:
    def get_edge_capacities(self, g):
        edge_id  = enum_edges(g)
        all_ones = [1] * len(edge_id)

        prob = cplex.Cplex()
        prob.set_results_stream(None)
        prob.set_log_stream(None)

        # objective is to minimize
        prob.objective.set_sense(prob.objective.sense.minimize)
        prob.variables.add(obj = all_ones, # objective coefficients are all 1s
                           lb  = all_ones) # lower bounds are all 1s
        for v in g.nodes():
            if len(g.pred[v]) == 0 or len(g.succ[v]) == 0:
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
            return {e: int(round(values[i])) for e, i in edge_id.items()}
        else:
            print("[Capacities] Linear programming failed: {}".format(status))
            sys.exit(1)

def get_edge_capacities(g):
    defaultSolver = CplexFlowCapSolver()
    return defaultSolver.get_edge_capacities(g)
