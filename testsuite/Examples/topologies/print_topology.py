#!/usr/bin/python

from mininet.net import Mininet
from mininet.node import RemoteController
from util import printTopology
import os
import sys
import argparse

def main():
    parser = argparse.ArgumentParser(
      description='Print the textual representation of a Mininet topology.')
    parser.add_argument('topology', type=str, nargs=1,
      help='path to a python file that defines a Topology class')
    args = parser.parse_args()
    topoGenerator = args.topology[0]
    
    topologyDir = os.path.abspath(os.path.dirname(sys.argv[0]))
    topologyFile = '%s.top' % (os.path.splitext(topoGenerator)[0])
    topologyPath = os.path.join(topologyDir, topologyFile)

    modName = os.path.splitext(os.path.basename(topoGenerator))[0]
    topoMod = __import__(modName)
    topo = topoMod.Topology()
    printTopology(topo, topologyPath)

main()
