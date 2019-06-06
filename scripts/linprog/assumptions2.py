#!/usr/bin/env python3.6
# vim: set foldmethod=marker:

import sys
import warnings
import networkx as nx
import collections
import cplex
import subprocess
import time
import itertools
import enum

from   flow_capacity import CplexFlowCapSolver
from   utils         import *
from   tests         import get_test

from   networkx.algorithms.components import strongly_connected_components

import pudb

class Variable:
    """
    These are the stuff that used as a variable in the MILP formulation.
    """
    def __init__(self, **kwargs):
        self._node       = kwargs["node"]
        self._var_index  = kwargs["var_index"]
        self._mark_index = kwargs["mark_index"]

    def is_markable(self):
        return self._mark_index is not None

    def get_var_index(self):
        return self._var_index

    def get_mark_index(self):
        assert(self.is_markable())
        return self._mark_index

    def get_node(self):
        return self._node

    def __eq__(self, other):
        return isinstance(other, Variable) and self._node == other._node

    def __hash__(self):
        return hash(self._node)

    def __str__(self):
        return "Variable({}, var index = {}, mark index = {})".\
            format(self._node,
                   self._var_index,
                   self._mark_index)

class AssumptionSolver:
    def __init__(self, filename):
        """
        Parse the given json file that contains the problem description.
        """
        parsed = parse_cplex_input(filename)

        self.g         = parsed["graph"]
        self.names     = parsed["names"]
        self.inv_names = parsed["inv_names"]
        self.must_eq   = set(parsed["must_eq"])        # nodes we DO     want to be      "always_eq"
        cannot_mark_eq = set(parsed["cannot_mark_eq"]) # nodes we DO NOT want to mark as "always_eq"

        def is_node_markable(n):
            return n not in cannot_mark_eq

        node_cnt            = self.g.order()
        markable_node_cnt   = sum ( int(is_node_markable(n)) for n in self.g.nodes() )
        self.variable_count = node_cnt + markable_node_cnt

        # variables used in the linear problem
        # mapping from variable identifier to the variable itself
        self.variables = { v : Variable(node       = v,
                                        var_index  = i,
                                        mark_index = i + node_cnt if is_node_markable(v) else None)
                           for i, v in enumerate(self.g.nodes()) }

        # variable index -> variable
        self.inv_variables = { i : var
                               for var in self.variables.values()
                               for i   in ( [ var.get_var_index(), var.get_mark_index() ]
                                            if var.is_markable()
                                            else [ var.get_var_index() ] ) }

        # calculate costs of the nodes
        # node_costs : Node -> Int
        self.node_costs = self.calc_costs()

    def calc_costs(self):
        """
        Returns a mapping from node ids to their costs
        """
        costs    = collections.defaultdict(int)
        worklist = collections.deque( n for n in self.g.nodes if len(self.g.pred[n]) == 0 )
        done     = set(worklist)

        while worklist:
            v = worklist.popleft()
            costs[v] = max((costs[u] for u in self.g.predecessors(v)), default=0) + 1
            for u in self.g.successors(v):
                if u not in done:
                    worklist.append(u)
                    done.add(u)

        return costs

    def add_always_eq_constraints(self, prob):
        """
        Adds the following constraint for each b in V
        c * (b - b_m) <= a_1 + a_2 + ...
        where
        a_1, a_2, ... are the parents of b
        c is the maximum of indegree of b and 1
        b_m is the mark variable o b
        """
        for node in self.g.nodes:
            var     = self.variables[node]
            parents = [ self.variables[u] for u in self.g.predecessors(node) ]
            c       = max(len(parents), 1)
            i       = var.get_var_index()
            indices      = [ i ]
            coefficients = [ c ]

            if var.is_markable():
                mi = var.get_mark_index()
                indices.append(mi)
                coefficients.append(-c)
                prob.linear_constraints.add(lin_expr = [ cplex.SparsePair(ind = [mi, i],
                                                                          val = [1, -1]) ],
                                            senses   = "L",
                                            rhs      = [0])

            for p in parents:
                debug("{} * {} <- {}".format(c,
                                             self.names[node],
                                             ", ".join(self.names[v.get_node()]
                                                       for v in parents)))
                indices.append(p.get_var_index())
                coefficients.append(-1)

            prob.linear_constraints.add(lin_expr = [ cplex.SparsePair(ind = indices,
                                                                      val = coefficients) ],
                                        senses   = "L",
                                        rhs      = [0])

    def add_must_eq_constraints(self, prob):
        """
        Adds the following constraint for each n in must equal set
        n = 1
        """
        for n in self.must_eq:
            var = self.variables[n]
            prob.linear_constraints.add(lin_expr = [ cplex.SparsePair(ind = [ var.get_var_index() ],
                                                                      val = [1] ) ],
                                        senses   = "E",
                                        rhs      = [1])

    def add_cc_constraints(self, prob):
        """
        Make sure that we mark at least one node in a connected component.
        """
        for component in strongly_connected_components(self.g):
            if len(component) < 2:
                continue
            indices = [ var.get_mark_index()
                        for var in ( self.variables[n] for n in component )
                        if var.is_markable() ]
            coefficients = [1] * len(indices)
            prob.linear_constraints.add(lin_expr = [ cplex.SparsePair(ind = indices,
                                                                      val = coefficients ) ],
                                        senses   = "G",
                                        rhs      = [1])


    def get_objective_function(self):
        """
        Return a list that contains the cost of each variable used
        """
        obj = [0] * self.variable_count
        for var in self.variables.values():
            if var.is_markable():
                i      = var.get_mark_index()
                n      = var.get_node()
                obj[i] = self.node_costs[n]
        return obj

    def get_upper_bounds(self):
        """
        Return a list that contains the upper bound of each variable used
        """
        return [1] * self.variable_count

    def suggest_assumptions(self):
        """
        Returns a set of nodes that when marked as always_eq, they make the
        nodes in must_eq to be always_eq as well.
        """
        prob = cplex.Cplex()
        prob.set_problem_type(prob.problem_type.MILP) # use Mixed Integer Linear Programming solver
        prob.set_results_stream(None)                 # disable results output
        prob.set_log_stream(None)                     # disable logging output

        # objective is to minimize
        prob.objective.set_sense(prob.objective.sense.minimize)

        t0 = time.perf_counter() # start the stopwatch

        # update cost and upper bound
        prob.variables.add(obj = self.get_objective_function(),
                           ub  = self.get_upper_bounds())

        self.add_always_eq_constraints(prob)

        self.add_must_eq_constraints(prob)

        self.add_cc_constraints(prob)

        # calculate the solution of the LP problem
        prob.solve()
        sol = prob.solution

        t1 = time.perf_counter()
        debug("elapsed time: {} ms".format(int((t1-t0) * 1000)))

        assert(sol.get_method() == sol.method.MIP)
        prob.write("assumptions.lp") # log the constraints to a file

        # check if we have found an optimal solution
        if sol.get_status() == sol.status.MIP_optimal:
            marked_nodes = set( var.get_node()
                                for var in self.variables.values()
                                if self.is_marked(sol, var) )

            self.debug_solution(sol, marked_nodes)

            return marked_nodes
        else:
            print("linprog failed: {}".format(sol.get_status_string()), file=sys.stderr)
            sys.exit(1)

    def is_always_eq(self, sol, x):
        """
        Checks if the given variable or node is always equal in the solution
        """
        if type(x) == int:
            var = self.variables[x]
        elif isinstance(x, Variable):
            var = x
        else:
            raise Exception("Expecting an integer or a variable, got {}".format(x))

        i = var.get_var_index()
        m = sol.get_values()[i]
        return val_to_int(m) == 1

    def is_marked(self, sol, x):
        """
        Checks if the given variable or node is marked in the solution
        """
        if type(x) == int:
            var = self.variables[x]
        elif isinstance(x, Variable):
            var = x
        else:
            raise Exception("Expecting an integer or a variable, got {}".format(x))

        if var.is_markable():
            i = var.get_mark_index()
            m = sol.get_values()[i]
            return val_to_int(m) == 1
        else:
            return False

    def debug_solution(self, sol, marked_nodes):
        sep()
        ae_calc = self.calculate_always_equal_nodes(marked_nodes)
        ae_sol  = set( var.get_node()
                       for var in self.variables.values()
                       if self.is_always_eq(sol, var) )
        diff1 = ae_calc - ae_sol
        if len(diff1) > 0:
            print("Nodes found always_eq by checking but not by the ILP solver:")
            for n in diff1:
                print("  {}".format(self.names[n]))
        diff2 = ae_sol - ae_calc
        if len(diff2) > 0:
            print("Nodes found always_eq by the ILP solver but not by checking:")
            for n in diff2:
                print("  {}".format(self.names[n]))
        sep()
        # pudb.set_trace()

    def name(self, node):
        return self.names[node]
    def nid(self, name):
        return self.inv_names[name]
    def parents(self, name):
        return [ self.name(p) for p in self.g.predecessors(self.nid(name)) ]

    def calculate_always_equal_nodes(self, marked_nodes):
        worklist  = collections.deque()
        always_eq = set(marked_nodes)

        for n in marked_nodes:
            worklist.extend(self.g.successors(n))

        while len(worklist) > 0:
            n = worklist.popleft()

            if n in marked_nodes:
                continue

            old_value = n in always_eq
            new_value = all( (u in always_eq) for u in self.g.predecessors(n) )

            # old_value implies new_value
            assert( (not old_value) or new_value )

            debug("[{}] {} = {}".\
                  format(("+" if new_value else "-") * 5,
                         self.names[n],
                         " ".join("{}({})".format(self.names[u],
                                                  "+" if u in always_eq else "-")
                                  for u in self.g.predecessors(n)),
                         new_value))

            if new_value == old_value:
                continue

            always_eq.add(n)
            worklist.extend(self.g.successors(n))

        return always_eq

    def validate_marked_nodes(self, marked_nodes):
        always_eq = self.calculate_always_equal_nodes(marked_nodes)
        diff = self.must_eq - always_eq

        if len(diff) > 0:
            print("", file=sys.stderr)
            for n in diff:
                print("!!! {} IS NOT MARKED !!!".format(self.names[n]), file=sys.stderr)
                p = min(( n for n in self.g.nodes() if n not in always_eq ),
                        key=lambda n: self.node_costs[n])
                print("root cause: {}".format(self.names[p]))
            return False
        return True

    def validate_marked_names(self, names):
        return self.validate_marked_nodes(set( self.inv_names[n] for n in names ))

    def run(self):
        def print_nodes(ns):
            for v in sorted([self.names[v] for v in ns]):
                print("  {}".format(v))

        print("Must equal:")
        print_nodes(self.must_eq)

        marked_nodes = self.suggest_assumptions()

        print("Marked nodes:")
        print_nodes(marked_nodes)

        self.validate_marked_nodes(marked_nodes)

        with open("output.json", "w") as f:
            json.dump({ "marked_nodes" : [ self.names[n] for n in marked_nodes ] }, f, indent=2)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("usage: assumptions.py <cplex.json>", file=sys.stderr)
        sys.exit(1)
    else:
        AssumptionSolver(sys.argv[1]).run()
