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

topo = mininet.topolib.TreeTopo(depth=1, fanout=2)
net = Mininet(topo=topo,controller=RemoteController,switch=UserSwitch)

print "Configuring NAT (iptables) ..."
os.system('sysctl -w net.ipv4.ip_forward=1')
os.system('iptables -F')
os.system('iptables -Z')
os.system('iptables -A FORWARD -o eth0 -i gate-host -s 10.0.0.0/24 '
          '-m conntrack --ctstate NEW -j ACCEPT')
os.system('iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED '
          '-j ACCEPT')
os.system('iptables -A POSTROUTING -t nat -j MASQUERADE')


os.system('ip link add name gate-host type veth peer name gate-mn')
os.system('ip link set gate-host up')
os.system('ip link set gate-mn up')
os.system('ip addr add 10.0.0.100/24 dev gate-host')

net.start()

s1 = net.switches[0]
print "Switch is %s" % s1

wanPort = s1.newPort()
s1.addIntf('gate-mn', wanPort)
print "WAN port is %s" % wanPort

print "Hosts are:"
for h in net.hosts:
  print h
  h.cmd('route add -net 10.0.0.0 netmask 255.255.255.0 %s-eth0' % h.name)
  h.cmd('route add default gw 10.0.0.100 %s-eth0' % h.name)

net.interact()

os.system('ip link del gate-host')
os.system('iptables -F')
os.system('iptables -Z')
