from mininet.topo import Topo, Node

class CliqueTopo( Topo ):
    '''
    Topology for a fully-connected set of switches with hosts evenly assigned
    to switches.

        h1 -- switch -- switch -- h2
                  \      /
                   \    /
                   switch
                     |
                     h3

    '''

    def __init__(self, numSwitches=3, numHosts=3):
        '''
        Create a clique topology, evenly distributing the number of hosts
        connected to each switch.
        '''
        super(CliqueTopo, self).__init__()

        # Node IDs
        hosts = [h for h in xrange(1, numHosts + 1)]
        firstSwitchNum = max(numHosts + 1, 100) + 1
        switches = [s for s in xrange(firstSwitchNum, firstSwitchNum + numSwitches)]

        # Add nodes
        [self.add_node(h, Node(is_switch=False)) for h in hosts]
        [self.add_node(s, Node(is_switch=True)) for s in switches]

        # Add edges
        [self.add_edge(h, switches[h % numSwitches]) for h in hosts]
        [self.add_edge(s1, s2) for s1 in switches for s2 in switches if s1 != s2]

        # Consider all switches and hosts 'on'
        self.enable_all()

        # DEBUG
        # Print all switch ports
        for x,y in self.edges():
            print "%s, %s : %s" % (x, y, self.port(x,y))


topos = { 'clique_topo' : (lambda: CliqueTopo()) }

