#!/usr/bin/python
import socket
from subprocess import Popen, call, PIPE
from mininet.node import RemoteController, UserSwitch
from mininet.net import Mininet
from mininet.util import quietRun, moveIntf
import mininet.topolib
import mininet.topo
import re
import os, sys
from time import gmtime, strftime, sleep
import unittest

topo = mininet.topolib.TreeTopo(depth=2, fanout=2)
net = Mininet(topo=topo,controller=RemoteController,switch=UserSwitch)

print "Configuring NAT (iptables) ..."
os.system('iptables -F')
os.system('iptables -Z')
os.system('iptables -A FORWARD -o eth0 -i gate-host -s 192.168.0.0/24 '
          '-m conntrack --ctstate NEW -j ACCEPT')
os.system('iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED '
          '-j ACCEPT')
os.system('iptables -A POSTROUTING -t nat -j MASQUERADE')


os.system('ip link add name gate-host type veth peer name gate-mn')
os.system('ip link set gate-host up')
os.system('ip link set gate-mn up')
os.system('ip addr add 192.168.0.1/24 dev gate-host')
h1 = net.hosts[0]
net.start()

moveIntf('gate-mn', h1)
h1.cmd('route del default')
h1.cmd('route add -net 10.0.0.0/24 dev h1-eth0')
h1.cmd('dhclient gate-mn')

print "Hosts are:"
for h in net.hosts:
  print h

net.interact()

os.system('ip link del gate-host')
os.system('iptables -F')
os.system('iptables -Z')
