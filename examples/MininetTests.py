#!/usr/bin/python
from subprocess import Popen, PIPE
import MininetDriver as md
import unittest
import time
import  re

class RepeaterTest(unittest.TestCase):

  def setUp(self):
    md.kill_controllers()
    self.mn = md.MininetRunner(md.TreeTopo(depth=2,fanout=2),
                             Popen([md.CONTROLLER_PATH,
                                       "--verbosity=DEBUG",
                                       "--log=repeater.log",
                                       "--repeater"]))

  def tearDown(self):
    self.mn.destroy()

  def testPing(self):
    time.sleep(2)
    for src in self.mn.hosts:
      for dst in self.mn.hosts:
        if src == dst:
          continue # OpenFlow flood doesn't send through inport
        self.assertEqual(self.mn.ping(src, dst, 0.5, 3), 0)


class Query1(unittest.TestCase):

  def setUp(self):
    md.kill_controllers()
    self.ctrl = Popen([md.CONTROLLER_PATH,
                          "--verbosity=DEBUG",
                          "--log=query1.log",
                          "--query1"],stdout=PIPE)
    self.mn = md.MininetRunner(md.TreeTopo(depth=1,fanout=2), self.ctrl)

  def tearDown(self):
    self.mn.destroy()

  def testQuery(self):
    src = self.mn.hosts[0]
    dst = self.mn.hosts[1]
    self.assertEqual(self.mn.ping(src, dst, 1, 5), 0)
    time.sleep(2) # let the query pickup the last pair
    self.ctrl.terminate()
    output = list(self.ctrl.stdout)
    # 2 for initial ARP and 10 from the pings
    m = re.match("Counter is: (\\d+)\n", output[-1])
    numPkts = int(m.group(1))
    if md.is_ipv6_enabled():
      self.assertIn(numPkts, range(18, 25))
    else:
      self.assertEqual(numPkts, 12)


class MacLearning(unittest.TestCase):

  def setUp(self):
    md.kill_controllers()
    self.ctrl = Popen([md.CONTROLLER_PATH,
                          "--verbosity=DEBUG",
                          "--log=learning1.log",
                          "--maclearning"])
    self.mn = md.MininetRunner(md.TreeTopo(depth=2,fanout=2), self.ctrl)

  def tearDown(self):
    self.mn.destroy()

  def testPinging(self):
    time.sleep(2)
    for src in self.mn.hosts:
      for dst in self.mn.hosts:
        if src == dst:
          continue # OpenFlow flood doesn't send through inport
        self.assertEqual(self.mn.ping(src, dst, 0.5, 3), 0)

if __name__ == '__main__':
    unittest.main()
