#!/usr/bin/python

from mininet.topo import Topo, Node

class VlanBugTopo( Topo ):

    def __init__(self, enable_all = True):
        super( VlanBugTopo, self).__init__()

        # Add switches and hosts.
        self.add_node( 101, Node( is_switch=True ) )
        self.add_node( 102, Node( is_switch=True ) )
        self.add_node(   1, Node( is_switch=False ) )
        self.add_node(   2, Node( is_switch=False ) )
        self.add_node(   3, Node( is_switch=False ) )

        # Add edges.
        self.add_edge( 101, 102 )
        self.add_edge( 101, 1 )
        self.add_edge( 102, 2 )
        self.add_edge( 102, 3 )

        # Consider all switches and hosts 'on'.
        self.enable_all()

# Let mininet run this topo from the command line.
topos = { 'vlanbug' : ( lambda: VlanBugTopo() ) }

