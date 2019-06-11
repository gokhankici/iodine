#!/usr/bin/env python3.6

import collections
import json
import sys
import re

sources    = set()
sinks      = set()
initial_eq = set()
initial_eq_mod = collections.defaultdict(list)
always_eq  = set()

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
        initial_eq_mod[m].append(v)
    elif typ == "sanitize_glob":
        always_eq.add(val)
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

def go_annot_1(var_set, type_name):
    if len(var_set) > 0:
        r = {"type": type_name, "variables": list(sorted(var_set))}
        result["annotations"].append(r)

go_annot_1(sources, "source")
go_annot_1(sinks, "sink")
go_annot_1(initial_eq, "initial_eq")
go_annot_1(always_eq, "always_eq")

print(json.dumps(result, indent=2))
