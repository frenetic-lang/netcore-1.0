#!/usr/bin/python

from mininet.topo import Topo, Node

class CampusTopo( Topo ):
    "A simple example of a small campus network."

    def __init__(self, enable_all = True):
        " Create a campus topology."
        super( CampusTopo, self).__init__()

        # Add switches and hosts.
        switches = [1, 2, 3]
        trustedUsers = [101, 102]
        secureServers = [201]
        untrustedUsers = [301, 302, 303]

        for switch in switches:
            self.add_node( switch, Node( is_switch=True ) )
        for host in trustedUsers + secureServers + untrustedUsers:
            self.add_node( host, Node (is_switch=False ) )

        # Add edges.
        self.add_edge( switches[0], switches[1] )
        self.add_edge( switches[0], switches[2] )
        self.add_edge( switches[1], switches[2] )
        for host in trustedUsers:
            self.add_edge( host, switches[0] )
        for host in secureServers:
            self.add_edge( host, switches[1] )
        for host in untrustedUsers:
            self.add_edge( host, switches[2] )

        # Consider all switches and hosts 'on'.
        self.enable_all()

# Let mininet run this topo from the command line.
topos = { 'campus' : ( lambda: CampusTopo() ) }

