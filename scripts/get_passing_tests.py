#!/usr/bin/env python3

FILENAME = "/tmp/passed"

bmks = []

with open(FILENAME, "r") as f:
    for l in f:
        [m, a] = l.split(" ")
        t = (m, a.strip())
        bmks.append(t)

def quote(s):
    return '"{}"'.format(s)

indent = "  "
output = '{}[ {}\n{}]'.format(indent, f'\n{indent}, '.join(f'("{m}", "{a}")' for m, a in bmks), indent)

print("passedTests :: [(String, String)]")
print("passedTests =")
print(output)

