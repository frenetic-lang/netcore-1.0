#!/usr/bin/python

import socket

msg = '''{\"MsgPolicy\":{\"PolProcessIn\":[\"Any\",[{\"ActFwd\":[\"AllPorts\",{\"modifyDlSrc\":\"Nothing\",\"modifyDlDst\":\"Nothing\",\"modifyDlVlan\":\"Nothing\",\"modifyDlVlanPcp\":\"Nothing\",\"modifyNwSrc\":\"Nothing\",\"modifyNwDst\":\"Nothing\",\"modifyNwTos\":\"Nothing\",\"modifyTpSrc\":\"Nothing\",\"modifyTpDst\":\"Nothing\"}]}]]}}
'''

s = socket.socket()
s.connect(('127.0.0.1', 6634))
s.send(msg)
print 'Sent: ' + msg

print 'Received: ' + s.recv(4096)

s.shutdown(socket.SHUT_RDWR)
s.close()
