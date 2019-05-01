import collections
import json
import networkx    as nx
import subprocess

def debug(msg):
    # print(msg)
    pass

def export_to_dot(g):
    return nx.nx_pydot.to_pydot(g)

def print_graph(g):
    print(export_to_dot(g))

def enum_edges(g):
    return dict(map((lambda t: (t[1],t[0])), enumerate(g.edges())))

def dist_from_sources(g, sources):
    dist     = {v:0 for v in sources}
    worklist = collections.deque()
    worklist.extend(sources)
    while worklist:
        v = worklist.popleft()
        d = dist[v]
        for u in g.successors(v):
            if u not in dist:
                dist[u] = d + 1
                worklist.append(u)
    return dist

def parse_cplex_input(filename):
    def to_edge(l):
        if len(l) == 2:
            return (l[0], l[1])
        elif len(l) == 3:
            return (l[0], l[1], dict(type=l[2]))
        else:
            print("illegal edge: {}".format(l))
            sys.exit(1)

    with open(filename, 'r') as f:
        data = json.load(f)

    edges        = [ to_edge(l) for l in data["edges"] if l[0] != l[1] ]
    must_eq      = data["must_eq"]
    names        = { l[0] : l[1][0] for l in data["mapping"] }
    inv_name     = { l[1][0] : l[0] for l in data["mapping"] }
    is_reg       = { l[0] : l[1][1] for l in data["mapping"] }
    cannot_be_eq = [ inv_name[v] for v in data["cannot_be_eq"] ]

    g = nx.MultiDiGraph()
    g.add_edges_from(edges)

    return { "graph"        : g,
             "must_eq"      : must_eq,
             "cannot_be_eq" : cannot_be_eq,
             "names"        : names,
             "is_reg"       : is_reg,
             "inv_name"     : inv_name }

def visualize_graph():
    rc = subprocess.run(["dot", "-Tpdf", "graph.dot", "-o", "graph.pdf"])
    if rc.returncode != 0:
        print("error while running dot")
        sys.exit(1)

def write_dot_file(g, names):
    g2 = nx.relabel_nodes(g, names, copy=True)
    with open("graph.dot", "w") as f:
        f.write(export_to_dot(g2).to_string())
