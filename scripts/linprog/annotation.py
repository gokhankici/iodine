#!/usr/bin/env python3.6

import collections
import json
import subprocess
import sys
from jsonschema import validate
import os.path as p

class Benchmark(collections.namedtuple("Benchmark", ["filename", "module", "annotfile"])):
    IVERILOG_DIR = p.realpath(p.join(p.dirname(__file__), "../../", "iverilog-parser"))

    def run_iodine(self, extra_args=[]):
        args = ["stack", "exec", "iodine", "--",
                "--iverilog-dir", Benchmark.IVERILOG_DIR]
        args.extend(extra_args)
        args.extend([self.filename, self.module, self.annotfile])

        rc = subprocess.run(args).returncode
        if rc != 0:
            print("iodine failed", file=sys.stderr)
            sys.exit(rc)

    def run_abduction(self):
        self.run_iodine(["--abduction"])

    def with_annot(self, annotfile):
        return Benchmark(filename=self.filename,
                         module=self.module,
                         annotfile=annotfile)

class Annotation:
    SCHEMA_FILE = p.join(p.dirname(p.dirname(p.dirname(p.realpath(__file__)))),
                         "annotation-schema.json")

    def __init__(self, **kwargs):
        """
        Required arguments: sources and sinks, or a filename that contains the annotations.
        """
        if "sources" in kwargs and "sinks" in kwargs:
            self.sources = list(kwargs["sources"])
            self.sinks = list(kwargs["sinks"])
        elif "filename" in kwargs:
            self.parse_file(kwargs["filename"])
        else:
            raise Exception("Unsupported arguments: {}".format(kwargs))

    def parse_file(self, filename):
        up = lambda f: p.dirname(f)

        with open(filename, "r") as f:
            j = json.load(f)
        Annotation.validate_json(j)

        self.sources = set()
        self.sinks = set()
        self.initial_eq = set()
        self.initial_eq_mod = collections.defaultdict(set)
        self.always_eq = set()
        self.assert_eq = set()

        for a in j["annotations"]:
            t = a["type"]
            if t == "source":
                self.sources.update(a["variables"])
            elif t == "sink":
                self.sinks.update(a["variables"])
            elif t == "always_eq":
                self.always_eq.update(a["variables"])
            elif t == "assert_eq":
                self.assert_eq.update(a["variables"])
            elif t == "initial_eq":
                if "module" in a:
                    m = a["module"]
                    self.initial_eq_mod[m].update(a["variables"])
                else:
                    self.initial_eq.update(a["variables"])
            else:
                raise Exception("Unsupported annotation:\n{}".format(json.dumps(a, indent=2)))

        assert(len(self.sources) > 0 and len(self.sinks) > 0)

        self.qualif_implies = set()
        self.qualif_iff = set()
        self.qualif_pairs = set()
        self.qualif_assume = set()

        if "qualifiers" in j:
            for q in j["qualifiers"]:
                t = q["type"]
                if t == "implies":
                    self.qualif_implies.add((q["lhs"], frozenset(q["rhs"])))
                elif t == "iff":
                    self.qualif_iff.add((q["lhs"], frozenset(q["rhs"])))
                elif t == "pairs":
                    self.qualif_pairs.add(frozenset(q["variables"]))
                elif t == "assume":
                    self.qualif_assume.add(frozenset(q["variables"]))
                else:
                    raise Exception("Unsupported qualifier:\n{}".format(json.dumps(q, indent=2)))

    def to_json(self):
        a = collections.defaultdict(list)

        def go_annot(typ, var, module=None):
            if len(var) == 0:
                return
            r = {"type": typ}
            if module:
                r["module"] = module
            r["variables"] = list(sorted(var))
            a["annotations"].append(r)

        go_annot("source", self.sources)
        go_annot("sink", self.sinks)
        go_annot("always_eq", self.always_eq)
        go_annot("initial_eq", self.initial_eq)
        for m in self.initial_eq_mod:
            go_annot("initial_eq", self.initial_eq_mod[m], module=m)
        go_annot("assert_eq", self.assert_eq)

        def go_qualif_lr(typ, qs):
            if len(qs) == 0:
                return
            for lhs, rhs in qs:
                r = {"type": typ, "lhs": lhs, "rhs": list(sorted(rhs))}
                a["qualifiers"].append(r)

        go_qualif_lr("implies", self.qualif_implies)
        go_qualif_lr("iff", self.qualif_iff)

        def go_qualif_vs(typ, qs):
            if len(qs) == 0:
                return
            for vs in qs:
                r = {"type": typ, "variables": list(sorted(vs))}
                a["qualifiers"].append(r)

        go_qualif_vs("pairs", self.qualif_pairs)
        go_qualif_vs("assume", self.qualif_assume)

        return a

    def dump(self):
        a = self.to_json()
        return json.dumps(a, indent=2)

    def validate(self):
        Annotation.validate_json(self.to_json())

    def validate_json(j):
        with open(Annotation.SCHEMA_FILE, "r") as f:
            schema = json.load(f)
        validate(instance=j, schema=schema)

    def set_initial_eq(self, vs):
        self.initial_eq = set(vs)
        self.initial_eq_mod.clear()

    def set_always_eq(self, vs):
        self.always_eq = set(vs)

if __name__ == "__main__":
    a = Annotation(filename=sys.argv[1])
    print(a.dump())
