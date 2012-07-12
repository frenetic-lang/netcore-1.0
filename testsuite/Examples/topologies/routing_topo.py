################################################################################
# The Frenetic Project                                                         #
# frenetic@frenetic-lang.org                                                   #
################################################################################
# Licensed to the Frenetic Project by one or more contributors. See the        #
# NOTICE file distributed with this work for additional information            #
# regarding copyright and ownership. The Frenetic Project licenses this        # # file to you under the following license.                                     #
#                                                                              #
# Redistribution and use in source and binary forms, with or without           #
# modification, are permitted provided the following conditions are met:       #
# - Redistributions of source code must retain the above copyright             #
#   notice, this list of conditions and the following disclaimer.              #
# - Redistributions in binary form must reproduce the above copyright          #
#   notice, this list of conditions and the following disclaimer in            #
#   the documentation or other materials provided with the distribution.       #
# - The names of the copyright holds and contributors may not be used to       #
#   endorse or promote products derived from this work without specific        #
#   prior written permission.                                                  #
#                                                                              #
# Unless required by applicable law or agreed to in writing, software          #
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT    #
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the     #
# LICENSE file distributed with this work for specific language governing      #
# permissions and limitations under the License.                               #
################################################################################
# /test-suite/examples/clique_topo.py                                          #
# Single-switch topology                                                       #
################################################################################

from mininet.topo import Topo, Node

class Topology( Topo ):
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
        super(Topology, self).__init__()

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


topos = { 'routing_topo' : (lambda: Topology()) }

