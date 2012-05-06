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
# /test-suite/examples/repeater_topo.py                                        #
# Single-switch topology                                                       #
################################################################################

from mininet.topo import Topo, Node

class RepeaterTopo( Topo ):
    '''
    Topology for a single repeater switch connecting two hosts.

        h1 -- switch -- h2

    '''

    def __init__(self, enable_all = True):
        "Create a repeater topology."
        super(RepeaterTopo, self).__init__()

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


topos = { 'repeater_topo' : (lambda: RepeaterTopo()) }

