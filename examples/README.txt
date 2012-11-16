To run multicast_tester.py, it is necessary to install the python
package py-multicast.

To get the error, build and run from the example folder:
./dist/build/frenetic-example-suite/frenetic-example-suite --multicast

In a separate terminal, run:
sudo  python arbitrary_topo.py --connections="[(1,101),(101,102),(102,103),(103,6),(102,104),(104,105),(103,105),(103,107),(105,106),(106,107),(107,2),(101,108),(105,108),(108,109),(107,109),(108,3),(108,4),(109,5)]"

run xterm h1 h2
in one of the xterms run python multicast_tester.py rec

in the other xterm run python multicast_tester.py send
type some messages

the first few messages do not register, and then on the third or fourth
the message will be received and the sender will be added to the tree
after this the receiver will be able to see messages from the sender.
