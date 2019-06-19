#!/usr/bin/env python3

import collections
import json
import sys
from jsonschema import validate
from config import SCHEMA_FILE


class Annotation:
    def __init__(self, *args, **kwargs):
        self.sources = set()
        self.sinks = set()
        self.initial_eq = set()
        self.initial_eq_mod = collections.defaultdict(set)
        self.always_eq = set()
        self.assert_eq = set()

        if len(args) == 1:
            self.parse_annotations(args[0])
        elif "sources" in kwargs and "sinks" in kwargs:
            self.sources = list(kwargs["sources"])
            self.sinks = list(kwargs["sinks"])

    def parse_annotations(self, annots):
        """ Parse the annotations from an JSON array """
        for a in annots:
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
                a_str = json.dumps(a, indent=2)
                msg = "Unsupported annotation:\n{}".format(a_str)
                raise Exception(msg)

    def to_json(self):
        """ Encodes the assumptions to JSON """
        j = []

        def go_annot(typ, var, module=None):
            if len(var) == 0:
                return
            r = {"type": typ}
            if module:
                r["module"] = module
            r["variables"] = list(sorted(var))
            j.append(r)

        go_annot("source", self.sources)
        go_annot("sink", self.sinks)
        go_annot("always_eq", self.always_eq)
        go_annot("initial_eq", self.initial_eq)
        for m in self.initial_eq_mod:
            go_annot("initial_eq", self.initial_eq_mod[m], module=m)
        go_annot("assert_eq", self.assert_eq)

        return j

    def set_initial_eq(self, vs):
        self.initial_eq = set(vs)
        self.initial_eq_mod.clear()

    def set_always_eq(self, vs):
        self.always_eq = set(vs)

    def __len__(self):
        return len(self.sources) + len(self.sinks) + \
            len(self.initial_eq) + len(self.initial_eq_mod) + \
            len(self.always_eq) + len(self.assert_eq)


class Qualifier:
    def __init__(self, *args):
        self.qualif_implies = set()
        self.qualif_iff = set()
        self.qualif_pairs = set()
        self.qualif_assume = set()

        if len(args) == 1:
            self.parse_qualifiers(args[0])

    def parse_qualifiers(self, qs):
        """ Parse the qualifiers from an JSON array """
        for q in qs:
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
                q_str = json.dumps(q, indent=2)
                msg = "Unsupported qualifier:\n{}".format(q_str)
                raise Exception(msg)

    def to_json(self):
        """ Encodes the qualifiers to JSON """
        j = []

        def go_qualif_lr(typ, qs):
            if len(qs) == 0:
                return
            for lhs, rhs in qs:
                r = {"type": typ, "lhs": lhs, "rhs": list(sorted(rhs))}
                j.append(r)

        go_qualif_lr("implies", self.qualif_implies)
        go_qualif_lr("iff", self.qualif_iff)

        def go_qualif_vs(typ, qs):
            if len(qs) == 0:
                return
            for vs in qs:
                r = {"type": typ, "variables": list(sorted(vs))}
                j.append(r)

        go_qualif_vs("pairs", self.qualif_pairs)
        go_qualif_vs("assume", self.qualif_assume)

        return j

    def set_pairs(self, pairs):
        self.qualif_pairs.clear()
        for p in pairs:
            self.qualif_pairs.add(frozenset(p))

    def __len__(self):
        return len(self.qualif_implies) + len(self.qualif_iff) + \
            len(self.qualif_pairs) + len(self.qualif_assume)


class AnnotationFile:
    def __init__(self, **kwargs):
        """
        Required arguments: sources and sinks, or a filename that contains the
        annotations.
        """
        if "sources" in kwargs and "sinks" in kwargs:
            self.annotations = Annotation(**kwargs)
        elif "filename" in kwargs:
            self.parse_file(kwargs["filename"])
        else:
            raise Exception("Unsupported arguments: {}".format(kwargs))

    def parse_file(self, filename):
        with open(filename, "r") as f:
            j = json.load(f)
        AnnotationFile.validate_json(j)

        self.annotations = Annotation(j["annotations"])

        if "blocklist" in j:
            self.blocklist = Annotation(j["blocklist"])
        else:
            self.blocklist = Annotation()

        assert(len(self.annotations.sources) > 0 and
               len(self.annotations.sinks) > 0)

        if "qualifiers" in j:
            self.qualifiers = Qualifier(j["qualifiers"])
        else:
            self.qualifiers = Qualifier()

    def to_json(self):
        a = {"annotations": self.annotations.to_json()}

        if len(self.qualifiers) > 0:
            a["qualifiers"] = self.qualifiers.to_json()

        if len(self.blocklist) > 0:
            a["blocklist"] = self.blocklist.to_json()

        AnnotationFile.validate_json(a)
        return a

    def dump(self):
        a = self.to_json()
        return json.dumps(a, indent=2)

    def validate(self):
        AnnotationFile.validate_json(self.to_json())

    def validate_json(j):
        with open(SCHEMA_FILE, "r") as f:
            schema = json.load(f)
        validate(instance=j, schema=schema)


if __name__ == "__main__":
    a = AnnotationFile(filename=sys.argv[1])
    print(a.dump())
