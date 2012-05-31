################################################################################
# The Frenetic Project                                                         #
# frenetic@frenetic-lang.org                                                   #
################################################################################
# Licensed to the Frenetic Project by one or more contributors. See the        #
# NOTICE file distributed with this work for additional information            #
# regarding copyright and ownership. The Frenetic Project licenses this        #
# file to you under the following license.                                     #
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
# /test-suite/examples/topologies/util.py                                      #
# Utility functions for Mininet topologies                                     #
################################################################################

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

