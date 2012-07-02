#!/usr/bin/python
from subprocess import Popen
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
    self.mn = MininetTest(TreeTopo(depth=1,fanout=2),
                          Popen([CONTROLLER_PATH, "--reactive1"]))

  def tearDown(self):
    self.mn.destroy()

  # This test case ensures that packets are forwarded. In addition, the 
  # controller should only receive PacketIn messages for the first packet in
  # each flow.
  # TODO(arjun): we do not test how much traffic the controller receives, but
  # see Wireshark.
  def testSpecialization(self):
    for src in self.mn.hosts:
      for dst in self.mn.hosts:
        if src == dst:
          continue # OpenFlow flood doesn't send through inport
        self.assertEqual(self.mn.ping(src, dst, 0.5, 10), 0)

if __name__ == '__main__':
    unittest.main()
