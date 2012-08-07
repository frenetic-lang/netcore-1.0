from mininet.topo import Topo, Node

class Topology( Topo ):
    '''
    Topology for a single repeater switch connecting two hosts.

        h1 -- switch -- h2

    '''

    def __init__(self, enable_all = True):
        "Create a repeater topology."
        super(Topology, self).__init__()

        numHosts = 2

        # Node IDs
        hosts = [h for h in xrange(1, numHosts + 1)]
        switch = max(numHosts + 1, 100) + 1

        # Add nodes
        [self.add_node(h, Node(is_switch=False)) for h in hosts]
        self.add_node(switch, Node(is_switch=True))

        # Add edges
        [self.add_edge(h, switch) for h in hosts]

        # Consider all switches and hosts 'on'
        self.enable_all()


topos = { 'repeater_topo' : (lambda: Topology()) }

