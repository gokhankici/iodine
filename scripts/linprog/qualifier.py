#!/usr/bin/env python3

import argparse
from annotation import AnnotationFile
from benchmark import BENCHMARKS
from config import ABDUCTION_OUTPUT, TMP_ANNOTFILE
from collections import namedtuple, deque, defaultdict
from utils import parse_cplex_input, foldl
import sys
# import pudb


class Variable(namedtuple("Variable", ["node", "name", "is_register"])):
    pass


class QualifierSolver:
    def __init__(self, benchmark):
        self.benchmark = benchmark
        benchmark.run_abduction()

        parsed = parse_cplex_input(ABDUCTION_OUTPUT)
        self.af = AnnotationFile(filename=benchmark.annotfile)
        self.graph = parsed["graph"]
        self.variables = {n: Variable(node=n,
                                      name=parsed["names"][n],
                                      is_register=parsed["is_reg"][n])
                          for n in self.graph.nodes()}
        self.inv_variables = {name: self.variables[n]
                              for name, n in parsed["inv_names"].items()}

    def get_parents(self, var):
        return (self.variables[v]
                for v in self.graph.predecessors(var.node))

    def get_children(self, var):
        return (self.variables[v]
                for v in self.graph.successors(var.node))

    def calculate_distances(self):
        def is_nb(u, v):
            edge_data = self.graph.get_edge_data(u.node, v.node)
            return all(d["type"] != "Direct" or
                       d["asgn"] == "NonBlocking"
                       for d in edge_data.values())

        # pudb.set_trace()
        distances = {self.inv_variables[name]: frozenset({0})
                     for name in self.af.annotations.sources
                     if name in self.inv_variables}
        worklist = deque(c
                         for var in distances.keys()
                         for c in self.get_children(var))
        done = set(distances.keys())

        while len(worklist) > 0:
            v = worklist.popleft()
            if v in done:
                continue

            ps = list(self.get_parents(v))
            is_plus_one = any(is_nb(p, v) for p in ps)
            ds = foldl(frozenset.union,
                       frozenset(),
                       (distances.get(p, frozenset([None])) for p in ps))

            # print(v.name, [p.name for p in ps], list(ds))

            if None in ds:
                continue

            distances[v] = frozenset(d + 1 for d in ds) if is_plus_one else ds
            done.add(v)
            worklist.extend(c
                            for c in self.get_children(v)
                            if c not in done)

        return distances

    def update_qualifiers(self):
        distances = self.calculate_distances()
        pairs = defaultdict(set)

        for v, ds in distances.items():
            if not v.is_register:
                continue
            for d in ds:
                pairs[d].add(v.name)

        for d, vs in pairs.items():
            if len(vs) > 1:
                print(d, list(sorted(vs)))

        self.af.qualifiers.set_pairs(p
                                     for p in pairs.values()
                                     if len(p) > 1)
        return self.af.qualifiers

    def run(self, args):
        self.update_qualifiers()

        with open(TMP_ANNOTFILE, "w") as f:
            f.write(self.af.dump())

        if not args.skip:
            b2 = self.benchmark.with_annotfile(TMP_ANNOTFILE)
            rc = b2.run_iodine(extra_args=["--no-save", "--verbose"])
            print("SUCCESS" if rc == 0 else "FAIL")
            sys.exit(rc)


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="""
    Generates extra qualifiers
    """)
    ap.add_argument("-b", dest="benchmark",
                    choices=BENCHMARKS.keys(),
                    required=True,
                    help="Benchmark name")
    ap.add_argument("--skip", action="store_true",
                    help="Do not run Iodine to check the qualifiers")
    args = ap.parse_args()
    QualifierSolver(BENCHMARKS[args.benchmark]).run(args)
