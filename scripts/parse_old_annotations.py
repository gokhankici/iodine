#!/usr/bin/env python3

import collections
import json
import sys
import re

sources    = set()
sinks      = set()
initial_eq = set()
always_eq  = set()
assert_eq  = set()

initial_eq_mod = collections.defaultdict(set)

qualif_implies = set()
qualif_pairs   = set()

def update_annots(typ, val):
    val = val.replace(" ", "")

    if typ == "taint_source":
        sources.add(val)

    elif typ == "taint_sink":
        sinks.add(val)

    elif typ == "sanitize":
        initial_eq.update(val.split(","))

    elif typ == "sanitize_mod":
        [m,v] = val.split(",")
        initial_eq_mod[m].add(v)

    elif typ == "sanitize_glob":
        always_eq.add(val)

    elif typ == "assert_eq":
        assert_eq.add(val)

    elif typ == "qualifierImp":
        m   = re.match("(.*),\[(.*)\]", val)
        lhs = m.group(1)
        rhs = m.group(2).split(",")
        qualif_implies.add((lhs, frozenset(rhs)))

    elif typ == "qualifierPairs":
        vs = val[1:-1].split(",") # remove the surrounding brackets
        qualif_pairs.add(frozenset(vs))

    else:
        print("Unknown annotation: @annot{{{}({})}}".format(typ, val),
              file=sys.stderr)

with open(sys.argv[1], "r") as f:
    for l in f:
        l = l.strip()
        for result in re.finditer("//\s*@annot{(.+)\((.+)\)}", l):
            t = result.group(1)
            a = result.group(2)
            update_annots(t, a)

result = collections.defaultdict(list)

def go_annot(var_set, type_name, module=None):
    if len(var_set) > 0:
        r = {"type": type_name, "variables": list(sorted(var_set))}
        if module is not None:
            r["module"] = module
        result["annotations"].append(r)

go_annot(sources, "source")
go_annot(sinks, "sink")
go_annot(assert_eq, "assert_eq")
go_annot(always_eq, "always_eq")
go_annot(initial_eq, "initial_eq")
for m in initial_eq_mod:
    go_annot(initial_eq_mod[m], "initial_eq", module=m)

for lhs, rhs in qualif_implies:
    result["qualifiers"].append({"type": "implies", "lhs": lhs, "rhs": list(rhs)})
for vs in qualif_pairs:
    result["qualifiers"].append({"type": "pairs", "variables": list(vs)})

print(json.dumps(result, indent=2))
