#!/usr/bin/env python3.6
# vim: set foldmethod=marker:

import sys
import collections
import cplex
import time
import json

from utils import debug, parse_cplex_input, val_to_int

# from networkx.algorithms.components import strongly_connected_components
# import pudb


class Variable(collections.namedtuple("Variable", ["node",
                                                   "var_index",
                                                   "mark_index",
                                                   "is_markable",
                                                   "name",
                                                   "is_register"])):
    """
    These are the stuff that used as a variable in the MILP formulation.
    """
    def __eq__(self, other):
        return isinstance(other, Variable) and self.node == other.node

    def __hash__(self):
        return hash(self.node)

    def __str__(self):
        return "Variable({}, var index = {}, mark index = {})".\
            format(self.node,
                   self.var_index,
                   self.mark_index)


class Assumptions(collections.namedtuple("Assumptions",
                                         ["always_eq", "initial_eq"])):
    """
    always_eq are the node ids of the variables that need the always equal
    assumption. initial_eq is the same thing for initially equal assumption.
    """
    def json_dump(self):
        """ names are the mapping from node ids to variable names """
        j = {"always_eq":  list(v.name for v in self.always_eq),
             "initial_eq": list(v.name for v in self.initial_eq)}
        return json.dumps(j, indent=2)

    def print(self, **kwargs):
        for v in self.always_eq:
            print("// @annot{{sanitize_glob({})}}".format(v.name), **kwargs)
        print("")
        for v in self.initial_eq:
            print("// @annot{{sanitize({})}}".format(v.name), **kwargs)


class AssumptionSolver:
    def __init__(self, filename):
        """
        Parse the given json file that contains the problem description.
        """
        parsed = parse_cplex_input(filename)
        self.g = parsed["graph"]

        # nodes we DO NOT want to mark as "always_eq"
        cannot_mark_eq = set(parsed["cannot_mark_eq"])

        def is_node_markable(n):
            return n not in cannot_mark_eq

        node_cnt = self.g.order()
        self.variable_count = 2 * node_cnt

        # variables used in the linear problem
        # mapping from variable identifier to the variable itself
        self.variables = {v: Variable(node=v,
                                      name=parsed["names"][v],
                                      var_index=i,
                                      mark_index=i + node_cnt,
                                      is_markable=is_node_markable(v),
                                      is_register=parsed["is_reg"][v])
                          for i, v in enumerate(self.g.nodes())}

        # nodes we DO want to be "always_eq"
        self.must_eq = set(self.variables[n] for n in parsed["must_eq"])

        # calculate costs of the nodes
        # node_costs : Node -> Int
        self.node_costs = self.calc_costs()

    def get_var_from_index(self, index):
        v = self.variables[index % self.g.order()]
        assert(v.var_index == index or v.mark_index == index)
        return v

    def calc_costs(self):
        """
        Returns a mapping from node ids to their costs
        """
        costs = collections.defaultdict(int)
        worklist = collections.deque(n
                                     for n in self.g.nodes
                                     if len(self.g.pred[n]) == 0)
        done = set(worklist)

        while worklist:
            v = worklist.popleft()
            costs[v] = max((costs[u] for u in self.g.predecessors(v)),
                           default=0) + 1
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
            var = self.variables[node]
            parents = [self.variables[u] for u in self.g.predecessors(node)]
            c = max(len(parents), 1)
            i = var.var_index
            indices = [i]
            coefficients = [c]

            if var.is_markable:
                mi = var.mark_index
                indices.append(mi)
                coefficients.append(-c)
                le = cplex.SparsePair(ind=[mi, i], val=[1, -1])
                prob.linear_constraints.add(lin_expr=[le], senses="L", rhs=[0])

            for p in parents:
                debug("{} * {} <- {}".
                      format(c,
                             var.name,
                             ", ".join(v.name for v in parents)))
                indices.append(p.var_index)
                coefficients.append(-1)

            le = cplex.SparsePair(ind=indices, val=coefficients)
            prob.linear_constraints.add(lin_expr=[le], senses="L", rhs=[0])

    def add_must_eq_constraints(self, prob):
        """
        Adds the following constraint for each n in must equal set
        n = 1
        """
        for var in self.must_eq:
            le = cplex.SparsePair(ind=[var.var_index], val=[1])
            prob.linear_constraints.add(lin_expr=[le], senses="E", rhs=[1])

    def get_objective_function(self):
        """
        Return a list that contains the cost of each variable used
        """
        obj = [0] * self.variable_count
        for var in self.variables.values():
            n = var.node
            c = self.node_costs[n]
            if var.is_markable:
                i = var.mark_index
                obj[i] = c
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
        prob.set_problem_type(prob.problem_type.MILP)  # use MILP solver
        prob.set_results_stream(None)                  # disable results output
        prob.set_log_stream(None)                      # disable logging output

        # objective is to minimize
        prob.objective.set_sense(prob.objective.sense.minimize)

        t0 = time.perf_counter()  # start the stopwatch

        # update cost and upper bound
        ts = [prob.variables.type.integer] * self.variable_count
        prob.variables.add(obj=self.get_objective_function(),
                           ub=self.get_upper_bounds(),
                           types=ts)

        self.add_always_eq_constraints(prob)

        self.add_must_eq_constraints(prob)

        # calculate the solution of the LP problem
        prob.solve()
        sol = prob.solution

        t1 = time.perf_counter()
        print("elapsed time: {} ms".format(int((t1-t0) * 1000)),
              file=sys.stderr)

        assert(sol.get_method() == sol.method.MIP)
        prob.write("assumptions.lp")  # log the constraints to a file

        # check if we have found an optimal solution
        if sol.get_status() == sol.status.MIP_optimal:
            return self.make_result(sol)
        else:
            print("linprog failed: {}".format(sol.get_status_string()),
                  file=sys.stderr)
            sys.exit(1)

    def is_always_eq(self, sol, var):
        """
        Checks if the given variable or node is always equal in the solution
        """
        assert(isinstance(var, Variable))
        i = var.var_index
        m = sol.get_values()[i]
        return val_to_int(m) == 1

    def is_marked(self, sol, var):
        """
        Checks if the given variable or node is marked in the solution
        """
        assert(isinstance(var, Variable))
        if var.is_markable:
            i = var.mark_index
            m = sol.get_values()[i]
            return val_to_int(m) == 1
        else:
            return False

    def make_result(self, solution):
        """
        Convert the MILP solution to a set of assumptions that Iodine can
        understand
        """
        marked, always_eq, initial_eq = set(), set(), set()
        for var in self.variables.values():
            is_m = self.is_marked(solution, var)
            is_a = self.is_always_eq(solution, var)
            if is_m:
                marked.add(var)
            if is_a:
                always_eq.add(var)
                if not is_m and var.is_register:
                    # if a register is always_eq but not marked,
                    # add it to the flushed set
                    initial_eq.add(var)

        return Assumptions(always_eq=marked, initial_eq=initial_eq)

    def run(self):
        debug("Must equal:")
        for v in self.must_eq:
            debug(v.name)

        result = self.suggest_assumptions()
        result.print()

        with open("output.json", "w") as f:
            print(result.json_dump(), file=f)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("usage: assumptions.py <cplex.json>", file=sys.stderr)
        sys.exit(1)
    else:
        AssumptionSolver(sys.argv[1]).run()
