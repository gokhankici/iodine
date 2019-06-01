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

import pdb

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
        for node in self.g.nodes:
            var     = self.variables[node]
            parents = [ self.variables[u] for u in self.g.predecessors(node) ]
            c       = max(len(parents), 1)

            indices      = [ var.get_var_index() ]
            coefficients = [ c ]

            if var.is_markable():
                indices.append(var.get_mark_index())
                coefficients.append(-c)

            for p in parents:
                indices.append(p.get_var_index())
                coefficients.append(-1)

            prob.linear_constraints.add(lin_expr = [ cplex.SparsePair(ind = indices,
                                                                      val = coefficients) ],
                                        senses   = "L",
                                        rhs      = [0])

    def add_must_eq_constraints(self, prob):
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
            self.debug_solution(sol)
            values = sol.get_values()

            def is_marked(var):
                if var.is_markable():
                    i = var.get_mark_index()
                    m = values[i]
                    return int(round(m)) == 1
                else:
                    return False

            marked_nodes = set( var.get_node()
                                for var in self.variables.values()
                                if is_marked(var) )

            return marked_nodes
        else:
            print("linprog failed: {}".format(sol.get_status_string()), file=sys.stderr)
            sys.exit(1)

    def debug_solution(self, sol):
        print("-" * 120)

        prefix = "m_i_palu_multiplier_"
        names = [ "m_i_palu_multiplier_n_ctr", "m_i_palu_multiplier_ctr" ]
        nodes = [ self.inv_names[s] for s in names ]
        edges = [ (u,v)
                  for u,v,_ in self.g.edges
                  if u in nodes or v in nodes ]
        values = [ int(round(a)) for a in sol.get_values() ]

        nodes2 = {}
        for u,v in edges:
            if u not in nodes2:
                nodes2[u] = len(nodes2)
            if v not in nodes2:
                nodes2[v] = len(nodes2)

        def get_name(node):
            name = self.names[node]
            if name.startswith(prefix):
                return "M_{}".format(name[len(prefix):])
            else:
                return name

        def get_ae_value(node):
            return values[self.variables[node].get_var_index()]

        def get_mark_value(node):
            var = self.variables[node]
            if var.is_markable():
                return values[var.get_mark_index()]
            else:
                return None

        print("{:*^83} {:*^9} {:*^9}".format("edge", "ae", "mark"))
        for e in edges:
            u, v = e
            print("({:^40},{:^40}) ({:>3},{:>3}) ({:>3},{:>3})".\
                  format(get_name(u) + " " + str(nodes2[u]),
                         get_name(v) + " " + str(nodes2[v]),
                         get_ae_value(u), get_ae_value(v),
                         get_mark_value(u), get_mark_value(v)))


        print("-" * 120)

        two_cycles = set()
        for u,v,_ in self.g.edges:
            if self.g.has_edge(v,u) and \
               (u,v) not in two_cycles and (v,u) not in two_cycles:
                two_cycles.add((u,v))
        print("cycles:")
        for u,v in two_cycles:
            print("{:<35} {:^15} {:<35}".format(self.names[u], "<--->", self.names[v]))

        print("-" * 120)

    def validate_marked_nodes(self, marked_nodes):
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

        diff = self.must_eq - always_eq

        if len(diff) > 0:
            # pdb.set_trace()
            print("", file=sys.stderr)
            for n in diff:
                print("!!! {} IS NOT MARKED !!!".format(self.names[n]), file=sys.stderr)
            return False
        return True

    def validate_marked_names(self, names):
        return self.validate_marked_nodes(set( self.inv_names[n] for n in names ))

    def run(self):
        debug("Must equal   :")
        for v in sorted([self.names[v] for v in self.must_eq]):
            debug("  {}".format(v))

        marked_nodes = self.suggest_assumptions()

        debug("Marked nodes : {}".format(", ".join(self.names[v] for v in marked_nodes)))

        self.validate_marked_nodes(marked_nodes)

        with open("output.json", "w") as f:
            json.dump({ "marked_nodes" : [ self.names[n] for n in marked_nodes ] }, f, indent=2)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("usage: assumptions.py <cplex.json>", file=sys.stderr)
        sys.exit(1)
    else:
        AssumptionSolver(sys.argv[1]).run()
