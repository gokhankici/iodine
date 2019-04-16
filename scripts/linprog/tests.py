#!/usr/bin/env python3

import sys
from   utils import *

tests = [
    { "edges":        [(0,3), (1,3), (2,3), (3,4)],
      "must_eq":      set([3]),
      "cannot_be_eq": set([])
    },
    { "edges":        [(0,1), (1,2), (2,3), (3,1), (3,4)],
      "must_eq":      set([3]),
      "cannot_be_eq": set([])
    }
]

def get_test(test_no):
    if 0 <= test_no < len(tests):
        t = tests[test_no]
        return t["edges"], t["must_eq"], t["cannot_be_eq"]
    else:
        print("Invalid test no: {}".format(test_no))
        return None

if __name__ == "__main__":
    if len(sys.argv) > 1:
        t = get_test(int(sys.argv[1]))
        if t:
            g = make_test_graph(t[0])
            print_graph(g)
