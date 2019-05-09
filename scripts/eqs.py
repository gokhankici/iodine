#!/usr/bin/env python

import sys

s = set()

for l in sys.stdin:
    l = l.replace("(", "")
    l = l.replace(")", "")
    ws = l.strip().split()

    for w in ws:
        if w.startswith("VLT_"):
            s.add(w[4:])

for w in sorted(s):
    print w
