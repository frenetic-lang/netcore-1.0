#!/usr/bin/python
################################################################################
# The Frenetic Project                                                         #
# frenetic@frenetic-lang.org                                                   #
################################################################################
# Licensed to the Frenetic Project by one or more contributors. See the        #
# NOTICE file distributed with this work for additional information            #
# regarding copyright and ownership. The Frenetic Project licenses this        #
# file to you under the following license.                                     #
#                                                                              #
# Redistribution and use in source and binary forms, with or without           #
# modification, are permitted provided the following conditions are met:       #
# - Redistributions of source code must retain the above copyright             #
#   notice, this list of conditions and the following disclaimer.              #
# - Redistributions in binary form must reproduce the above copyright          #
#   notice, this list of conditions and the following disclaimer in            #
#   the documentation or other materials provided with the distribution.       #
# - The names of the copyright holds and contributors may not be used to       #
#   endorse or promote products derived from this work without specific        #
#   prior written permission.                                                  #
#                                                                              #
# Unless required by applicable law or agreed to in writing, software          #
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT    #
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the     #
# LICENSE file distributed with this work for specific language governing      #
# permissions and limitations under the License.                               #
################################################################################
# netcore_controller.py                                                        #
# Controller front-end for connecting to a NetCore hypervisor.                 #
################################################################################
"""Controller front-end for connecting to a NetCore hypervisor."""

import json


def runController(port, policyQueue, errorQueue):
    '''
        Connect to a NetCore hypervisor on port 'port', deploying Policy objects
        via the queue 'policyQueue'.  Errors are pushed onto the errorQueue.
    '''
    pass


# Predicate -> JSON String
# Policy    -> (JSON String, Queries, Injects)
# where Queries :: {id : queue}
# and   Injects :: {id : queue}
class PredV_JSON(PredicateVisitor):

    def __init__(self):
        self.id = 0

    ### Predicates

    def predTop(self, predTop):
        return '{"Any" : []}'

    def predBottom(self, predBottom):
        return '{"None" : []}'

    def pred(self, pred):
        # Handle IP addresses specially.
        if pred.field == 'NwSrc' || pred.field == 'NwDst':
            ip, mask = pred.pattern
            return '{"%s" : [%d, %d]}' % (pred.field, ip, mask)
        else:
            return '{"%s" : [%d]}' % (pred.field, pred.pattern)

    def predUnion(self, predUnion):
        left = predUnion.left.walk(self)
        right = predUnion.right.walk(self)
        return '{"Or" : [%s, %s]}' % (left, right)

    def predIntersection(self, predIntersection):
        left = predIntersection.left.walk(self)
        right = predIntersection.right.walk(self)
        return '{"And" : [%s, %s]}' % (left, right)

    def predNegation(self, predNegation):
        return '{"Not" : [%s]}' % predNegation.pred.walk(self)

    ### Action -> (JSON string, Queries)

    def actionForward(self, actionForward):
        mod = json.dumps(actionForward.modification)
        return ('{"ActFwd" : [%s, %s]}' % (mod, port), {}, {})

    def actionCountPackets(self, actionCountPackets):
        id = self.id
        self.id = self.id + 1
        interval = actionCountPackets.interval
        qs = {id : [actionCountPackets.queue]}
        return ('{"ActQueryPktCounter" : [%d, %d]}' % (interval, id), qs)

    def actionCountBytes(self, actionCountBytes):
        id = self.id
        self.id = self.id + 1
        interval = actionCountBytes.interval
        qs = {id : [actionCountBytes.queue]}
        return ('{"ActQueryByteCounter" : [%d, %d]}' % (interval, id), qs)

    def actionGetPacket(self, actionGetPacket):
        id = self.id
        self.id = self.id + 1
        qs = {id : [actionGetPacket.queue]}
        return ('{"ActGetPkt" : [%d]}' % id, qs)

    def actionMonitorSwitch(self, actionMonitorSwitch):
        id = self.id
        self.id = self.id + 1
        qs = {id : [actionMonitorSwitch.queue]}
        return ('{"ActMonSwitch" : [%d]}' % id, qs)

    ### Policy -> (JSON String, Queries, Injects)

    # {id : [queue]} -> {id : [queue]} -> {id : [queue]}
    # Add q2 to q1, using '+' to merge values with the same key.
    def _unionDicts(self, q1, q2):
        for k,v in q2.iteritems():
            if k in q1:
                q1[k] = q3[k] + v
            else:
                q1[k] = v
        return q1

    def polEmpty(self, polEmpty):
        return ('{"PolEmpty" : []}', {}, {})

    def polProcessIn(self, polProcessIn):
        pred = polProcessIn.predicate.walk(self)
        actStrs = []
        qs = {}
        for act in polProcessIn.actions:
            json, query = act.walk(self)
            actStrs.append(json)
            qs = _unionDicts(self, qs, query)
        acts = '[' + ','.join(actStrs) + ']'
        return ('{"PolProcessIn" : [%s, %s]}' % (pred, acts), qs, {})

    def polUnion(self, polUnion):
        json1, qs1, ins1 = polUnion.left.walk(self)
        json2, qs2, ins2 = polUnion.right.walk(self)
        json = '{"PolUnion" : [%s, %s]}' % (json1, json2)
        qs = _unionDicts(self, qs1, qs2)
        ins = _unionDicts(self, ins1, ins2)
        return (json, qs, ins)

    def polRestrict(self, polRestrict):
        pred = polRestrict.predicate.walk(self)
        pol, qs, ins = polRestrict.policy.walk(self)
        json = '{"PolRestrict" : [%s, %s]}' % (pol, pred)
        return (json, qs, ins)
        
    def polGenPacket(self, polGenPacket):
        id = self.id
        self.id = self.id + 1
        json = '{"PolGenPacket" : [%d]}' % id
        return (json, {}, {id : polGenPacket.queue})

