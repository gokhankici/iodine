import networkx as nx
import collections

def debug(msg):
    # print(msg)
    pass

def export_to_dot(g):
    return nx.nx_pydot.to_pydot(g)

def print_graph(g):
    print(export_to_dot(g))

def enum_edges(g):
    return dict(map((lambda t: (t[1],t[0])), enumerate(g.edges())))

def make_test_graph(edges):
    g = nx.DiGraph()
    g.add_edges_from(edges)
    return g

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
