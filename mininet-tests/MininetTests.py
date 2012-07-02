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

class Reactive1Test(unittest.TestCase):

  def setUp(self):
    kill_controllers()
    self.ctrl = Popen([CONTROLLER_PATH, "--reactive1"],stdout=PIPE)
    self.mn = MininetTest(TreeTopo(depth=1,fanout=2), self.ctrl)

  def tearDown(self):
    self.mn.destroy()

  def testSpecialization(self):
    src = self.mn.hosts[0]
    dst = self.mn.hosts[1]
    self.assertEqual(self.mn.ping(src, dst, 0.01, 1), 0)
    sleep(1) # Give time to specialize
    self.assertEqual(self.mn.ping(src, dst, 0.01, 100), 0)
    self.ctrl.terminate()
    numPktIn = sum([1 for x in self.ctrl.stdout if "OFPT_PACKET_IN" in x])
    self.assertEqual(numPktIn, 4, "too many PACKET_IN messages")

# A sanity-check: Reactive1Test tests reactive specialization by asserting
# that the controller receives a small number of PACKET_IN messages for a large
# number of pings. But, if we don't give the controller time to install the
# specialized rule after the first ping, it should see more PACKET_INs.
class Reactive1TestSanityCheck(unittest.TestCase):

  def setUp(self):
    kill_controllers()
    self.ctrl = Popen([CONTROLLER_PATH, "--reactive1"],stdout=PIPE)
    self.mn = MininetTest(TreeTopo(depth=1,fanout=2), self.ctrl)

  def tearDown(self):
    self.mn.destroy()

  def testSpecialization(self):
    src = self.mn.hosts[0]
    dst = self.mn.hosts[1]
    self.mn.ping(src, dst, 0.01, 100)
    self.ctrl.terminate()
    numPktIn = sum([1 for x in self.ctrl.stdout if "OFPT_PACKET_IN" in x])
    self.assertTrue(numPktIn > 5, "too few PACKET_IN messages")


if __name__ == '__main__':
    unittest.main()
