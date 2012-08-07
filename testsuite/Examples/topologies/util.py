from mininet.topo import Topo, Node
import os

def printTopology(topo, filename):
    '''
    Print a Mininet Topo object in the following format:

    {Host,Switch} n: (n1, p1), (n2, p2), ..., (nn, pn)

    where n is a host or switch node, n1 ... nn are its neighbors,
    and p1 ... pn are the ports on n connected to each neighbor,
    respectively.

    If the environment variable FRENETIC_TOPOLOGY_PATH does not
    point to a valid, writable file location, do nothing.
    '''

    f = open(filename, 'w')
    nodes = {}
    for n1, n2 in topo.edges():
        if n1 not in nodes:
            nodes[n1] = []
        if n2 not in nodes:
            nodes[n2] = []
        nodes[n1].append((n2, topo.port(n1,n2)[0]))
        nodes[n2].append((n1, topo.port(n2,n1)[0]))

    for k,v in nodes.iteritems():
        edges = ', '.join(['(%s,%s)' % (topo.name(n), p) for (n,p) in v])
        if topo.is_switch(k):
            nodeLabel = 'Switch'
        else:
            nodeLabel = 'Host'
        f.write('%s %s: %s\n' % (nodeLabel, topo.name(k), edges))

    f.close()

