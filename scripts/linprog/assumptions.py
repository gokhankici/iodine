#!/usr/bin/env python3.6
# vim: set foldmethod=marker:

import sys
import collections
import cplex
import time
from utils import parse_cplex_input, val_to_int
from annotation import AnnotationFile


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


class AssumptionSolver:
    def __init__(self, filename, annotfile):
        """
        Parse the given json file that contains the problem description.
        """
        self.annotation_file = AnnotationFile(filename=annotfile)

        parsed = parse_cplex_input(filename)
        self.g = parsed["graph"]

        # nodes we DO NOT want to MARK as "always_eq"
        blocklist = self.annotation_file.blocklist
        cannot_mark_eq = set(parsed["inv_names"][v]
                             for v in blocklist.always_eq)

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

        # nodes that cannot be always_eq
        self.cannot_be_eq = set(self.variables[parsed["inv_names"][v]]
                                for v in blocklist.initial_eq)

        # nodes we DO want to be "always_eq"
        self.must_eq = set(var
                           for var in (self.variables[n]
                                       for n in parsed["must_eq"])
                           if is_node_markable(var.node) and
                           var not in self.cannot_be_eq)

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
        srcs = set(self.annotation_file.annotations.sources)
        worklist = collections.deque(v.node
                                     for v in self.variables.values()
                                     if len(self.g.pred[v.node]) == 0 or
                                     v.name in srcs)
        assert(len(worklist) > 0)
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

    def add_cannot_be_eq_constraints(self, prob):
        for var in self.cannot_be_eq:
            le = cplex.SparsePair(ind=[var.var_index], val=[1])
            prob.linear_constraints.add(lin_expr=[le], senses="E", rhs=[0])

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
        self.add_cannot_be_eq_constraints(prob)

        # calculate the solution of the LP problem
        prob.solve()
        sol = prob.solution

        t1 = time.perf_counter()
        print("elapsed time: {} ms".format(int((t1-t0) * 1000)),
              file=sys.stderr)

        assert(sol.get_method() == sol.method.MIP)
        # prob.write("assumptions.lp")  # log the constraints to a file

        # check if we have found an optimal solution
        if sol.get_status() == sol.status.MIP_optimal:
            self.update_annotation_file(sol)
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

    def update_annotation_file(self, solution):
        """
        Convert the MILP solution to a set of assumptions that Iodine can
        understand
        """
        marked, always_eq, initial_eq = set(), set(), set()
        for var in self.variables.values():
            is_m = self.is_marked(solution, var)
            is_a = self.is_always_eq(solution, var)
            if is_m:
                marked.add(var.name)
            if is_a:
                always_eq.add(var.name)
                if not is_m and var.is_register:
                    # if a register is always_eq but not marked,
                    # add it to the flushed set
                    initial_eq.add(var.name)

        self.annotation_file.annotations.set_always_eq(marked)
        self.annotation_file.annotations.set_initial_eq(initial_eq)

    def run(self):
        print("-" * 80, file=sys.stderr)
        print("Must equal:", file=sys.stderr)
        for name in sorted(v.name for v in self.must_eq):
            print("  " + name, file=sys.stderr)
        print("-" * 80, file=sys.stderr)

        self.suggest_assumptions()
        print(self.annotation_file.dump())


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("usage: assumptions.py <cplex.json> <annot json>",
              file=sys.stderr)
        sys.exit(1)
    else:
        AssumptionSolver(sys.argv[1], sys.argv[2]).run()
