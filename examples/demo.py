#!/usr/bin/python
"""Demo some features of NetCore."""

import argparse
import mininet.topolib
import mininet.topo
import MininetDriver as md
from multiprocessing import Process
import subprocess as sp
import time

# H1 0----1 S3 2----0 H2
basic = mininet.topolib.TreeTopo(depth=1, fanout=2)

def getRunner(flag, topo=basic):
  ctrl = sp.Popen([md.CONTROLLER_PATH,
                   flag,
                   ],stdout=sp.PIPE)

  time.sleep(1)
  return md.MininetRunner(topo, ctrl)

def host_output(host, title, cmds):
  title.append('Ctrl+D to exit')
  titleCommands = ['echo ' + '\\"' + line + '\\"' for line in title]
  cmd = 'xterm -e "' + ("; ".join(titleCommands + cmds + ['bash'])) + '"'
  print cmd
  host.cmd(cmd)

def demo_learning_switch():
  runner = getRunner('--maclearning')

  host_output(runner.hosts[0],
              ['Ping Connectivity over MAC Learning:'],
              ['ping ' + runner.hosts[1].IP() + ' -c 10'])

  runner.destroy()

def demo_arp():
  runner = getRunner('--arp')

  target = runner.hosts[1].IP()

  host_output(runner.hosts[0],
              ['ARP caching using MAC Learning for connectivity'],
              ['echo \\"Unfortunately, arping doesn\'t work on mininet.\\"',
               'arp -n',
               'echo \\"At first, ping uses regular ARP over the MAC learning:\\"',
               'ping ' + target + ' -c 1',
               'arp -n',
               'echo \\"But if we clear the ARP table,'
                      ' we get a reply from the controller:\\"',
               'arp -d ' + target,
               'arp -n',
               'ping ' + target + ' -c 1',
              ])

  runner.destroy()

def demo_sink(user):
  runner = getRunner('--sink', topo=mininet.topo.SingleSwitchTopo())

  target = runner.hosts[1].IP()

  ping = Process (target=host_output,
                  args=(runner.hosts[0],
                    ['Pings get blocked when at high frequency,'],
                    ['echo \\"# ping ' + target + '-i 0.1 -c 50 -q\\"',
                     'ping ' + target + ' -i 0.1 -c 50 -q',
                     'echo \\"but SSH still works because it\'s on a different slice.\\"',
                     'echo \\"(but do give it a moment...)\\"',
                     'echo \\"# ssh ' + user + '@' + target + '\\"',
                     'ssh ' + user + '@' + target]))
  ping.start()
  sshd = Process (target=host_output,
                  args=(runner.hosts[1],
                        ['SSH Daemon on the other host'],
                        ['$(which sshd) -d']))
  sshd.start()

  ping.join()
  sshd.join()

  runner.destroy()

def main():
  parser = argparse.ArgumentParser("Run some NetCore demos")
  parser.add_argument('user', metavar='USER', type=str,
                      help='user to ssh to (usually you)')
  args = parser.parse_args()
  demo_learning_switch()
  demo_arp()
  demo_sink(args.user)

if __name__ == '__main__':
  main()
