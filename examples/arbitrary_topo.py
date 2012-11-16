#!/usr/bin/python

"""
Mininet Experimental Environment 'Sandbox'
- Creates an arbitrary topology of switches and hosts
USAGE: sudo ./sandbox.py [OPTIONS] 
-r --remote-nox=ADDR:PORT
-s --num-switches=NUM 
-n --num-hosts=NUM
"""

import sys
flush = sys.stdout.flush
import ast
from optparse import OptionParser
from mininet.net import init, Mininet
from mininet.node import KernelSwitch, UserSwitch, OVSKernelSwitch, RemoteController
from mininet.topo import Topo, Node
from mininet.log import lg
from mininet.log import info, error, debug, output
from mininet.cli import CLI
from mininet.term import *
import time

START_TIME = 0
STATIC_ARP = True
DEBUG = False
switchTypes = {'us' : UserSwitch, 'ovsk' : OVSKernelSwitch}

class ArbitraryTopo(Topo):

    def __init__(self, connections=[]):

        # Add default members to class.
        super(ArbitraryTopo, self ).__init__()

        #keep track of which nodes have already been initialized
        already_initialized = {}

        for (node1, node2) in connections:
            if not (already_initialized.get(node1,False)):
                already_initialized[node1] = True
                isSwitch = node1 > 100
                self.add_node(node1, Node(is_switch=isSwitch))
            if not (already_initialized.get(node2,False)):
                already_initialized[node2] = True
                isSwitch = node2 > 100
                self.add_node(node2, Node(is_switch=isSwitch))
            self.add_edge(node1, node2)

        # Consider all switches and hosts 'on'
        self.enable_all()

def check_controller(controller):
    result = controller.split(':')
    if len(result) != 2:
        raise OptionsError("Wrong number of arguments in controller")
    return (result[0], result[1])
    

def start(connections, stype, ip, port):
    net = Mininet( topo=ArbitraryTopo(ast.literal_eval(connections)), switch=switchTypes[stype], 
                   controller=lambda name: RemoteController(name, defaultIP=ip, port=int(port)), 
                   xterms=False, autoSetMacs=True)

    net.start()    
    net.staticArp()
    output("  *** Network Initialized in %s seconds***\n\n" % str(time.time()-START_TIME))

    # Wait for switch to register with controller
        
    # Enter CLI mode for any finishing touches you might want to test
    output("*** Press ctrl-d or type exit to quit ***\n")
    # Change verbosity so that the output of CLI commands will display
    lg.setLogLevel('info')
    CLI(net)
    lg.setLogLevel('output')
    # All done, cleanup
    #netElems['h1'].cmd('sudo killall dhclient')
    net.stop()

if __name__ == '__main__':
    # Lower verbosity level to suppress certain output.
    # SIDE EFFECT: CLI interactive output is supressed as well.
    # SOLUTION: Change verbosity before launching CLI
    lg.setLogLevel( 'output')
    MAXHOSTS = 100

    usage = "Usage: %prog [options]"
    parser = OptionParser(usage=usage)

    parser.add_option('-D', '--debug', action='store_true', dest='debug', default=False,
                        help='Debug the script with additional output.')
    parser.add_option('-s', "--connections", action="store", type='string', dest="connections", default=[],
                        help="The topology of the network.  The topology should be given in the format of a list of tuples representing an edge from one node to another node.  Hosts should be nodes numbered less than 100, switches should be nodes numbered greater than 100.")
    parser.add_option('-t', "--switch-type", action="store", type='string', dest="st", default='us',
                        help="Type of switch to use [us|ovsk]. us = UserSwitch, ovsk = OpenVSwitch Kernel")
    parser.add_option('-c', "--controller-address", action="store", type='string', dest="controller", default='127.0.0.1:6633',
                        help="If specified, connects to a remote NOX instance IP:Port")
    parser.add_option('-a', "--full-arp", action="store_false", dest="full_arp", default=True,
                        help="Do *NOT* set static ARP entries and allow hosts to do ARP requests")

    (options, args) = parser.parse_args()

    # Check option validity
    if not (options.st in switchTypes.keys()):
        parser.error("Invalid switch type.")
    (ip,port) = check_controller(options.controller)
    START_TIME = time.time()
    STATIC_ARP = options.full_arp
    DEBUG = options.debug
    start(options.connections, options.st, ip, port)
