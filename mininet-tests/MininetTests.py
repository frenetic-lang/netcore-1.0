#!/usr/bin/python
from subprocess import Popen, PIPE
from MininetDriver import *
from time import sleep

class RepeaterTest(unittest.TestCase):
  
  def setUp(self):
    kill_controllers()
    self.mn = MininetTest(TreeTopo(depth=2,fanout=2),
                          Popen([CONTROLLER_PATH, "--repeater"]))

  def tearDown(self):
    self.mn.destroy()

  def testPing(self):
    for src in self.mn.hosts:
      for dst in self.mn.hosts:
        if src == dst:
          continue # OpenFlow flood doesn't send through inport
        self.assertEqual(self.mn.ping(src, dst, 0.5, 3), 0)


class Query1(unittest.TestCase):

  def setUp(self):
    kill_controllers()
    self.ctrl = Popen([CONTROLLER_PATH, "--query1"],stdout=PIPE)
    self.mn = MininetTest(TreeTopo(depth=1,fanout=2), self.ctrl)

  def tearDown(self):
    self.mn.destroy()

  def testQuery(self):
    src = self.mn.hosts[0]
    dst = self.mn.hosts[1]
    self.assertEqual(self.mn.ping(src, dst, 1, 5), 0)
    sleep(2) # let the query pickup the last pair
    self.ctrl.terminate()
    output = list(self.ctrl.stdout)
    # 2 for initial ARP and 10 from the pings
    self.assertTrue("Counter is: 12" in output[-1], "expected 10 packets")


if __name__ == '__main__':
    unittest.main()
