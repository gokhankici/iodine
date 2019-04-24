import collections
import json
import networkx    as nx

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

def parse_file(filename):
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
    names        = { l[0] : l[1] for l in data["mapping"] }
    inv_name     = { l[1] : l[0] for l in data["mapping"] }
    cannot_be_eq = [ inv_name[v] for v in data["cannot_be_eq"] ]

    g = nx.MultiDiGraph()
    g.add_edges_from(edges)

    return (g, must_eq, cannot_be_eq, names)
