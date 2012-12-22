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
# netcore.py                                                                   #
# Netcore grammar objects and related functions                                #
################################################################################
"""Netcore grammar objects and related functions."""

from abc import ABCMeta, abstractmethod
import copy

class Port:

    ### switch_id -> port_id -> Port
    def __init__(self, switch_id, port_id):
        self.switch_id = switch_id
        self.port_id = port_id

    def key(self):
        return(self.__class__.__name__, self.switch_id, self.port_id)

    def __hash__(self):
        return hash(self.key())

    def __str__(self):
        return ("%s(%s:%d)" % self.key())

    def __repr__(self):
        return self.__str__()

    def __eq__(self,other):
        return str(self) == str(other)


class FabricPath:

    ### Port -> Port -> FabricPath
    def __init__(self, start, end):
        self.start = start
        self.end = end
        self.paths = {}

    def key(self):
        return(self.__class__.__name__, self.start, self.end)

    def __hash__(self):
        return hash(self.key())

    def __str__(self):
        return ("%s[%s->%s]%s" % (self.__class__.__name__,self.start,self.end,self.paths))

    def __repr__(self):
        return self.__str__()

    def set(self):
        return len(self.paths) > 0


class Switch:

    ### string -> Predicate -> [ Set Port ] -> Switch
    def __init__(self, name, flowspace, portsets):
        self.name = name
        self.flowspace = flowspace
        self.ports = {}
        self.fabric = {}
        for portset in portsets:
            self.ports[len(self.ports)+1] = portset
        for p1 in self.ports.iterkeys():
            self.fabric[p1] = {}
            for p2 in self.ports.iterkeys():
                self.fabric[p1][p2] = FabricPath(p1,p2)

                # hookup in-out
                if p1 == p2:
                    for uport in self.ports[p1]:
                        self.fabric[p1][p2].paths[self.flowspace] = [uport,uport]
                        break  # IN-OUT ON FIRST PORT - SHOULD BE ALL, FABRIC PATH ABSTRACTION NOT QUITE RIGHT

    def key(self):
        return(self.__class__.__name__, self.name, self.flowspace)

    def __hash__(self):
        return hash(self.key())

    def __str__(self):
        fabricStr = ''
        for (vport,portset) in self.ports.iteritems():
            for port in portset:
                fabricStr += "%s:\t%s\n" % (vport,portset)
                for (vport, fabricPath) in self.fabric[vport].iteritems():
                    fabricStr += "\t%s\n" % fabricPath
        return ("%s %s flowspace=%s\n%s" % (self.__class__.__name__, self.name, self.flowspace, fabricStr) )

    def __repr__(self):
        return self.__str__()

    def inherit_fabric(self):
        for p1 in self.ports.iterkeys():
            self.fabric[p1] = {}
            for p2 in self.ports.iterkeys():
                self.fabric[p1][p2] = FabricPath(p1,p2)
                for uport1 in self.ports[p1]:
                    for uport2 in self.ports[p2]:
                        if uport1.switch_id == uport2.switch_id:
                            self.fabric[p1][p2].paths[self.flowspace] = [uport1,uport2]
                        

    def make_bidirectional(self):

        # FOR EACH PORT PAIR
        for i in range(1,len(self.ports)+1):
            for j in range(1,len(self.ports)+1):

                # IF NON-TRIVIAL AND PATH FOR PORTS REVERSED UNSET
                if i != j and not self.fabric[i][j].set():

                    # FOR EACH PREDICATE PATH, INSTALL REVERSE PATH
                    for (pred,path) in self.fabric[j][i].paths.iteritems():
                        pathCopy = list(path)
                        pathCopy.reverse()
                        self.fabric[i][j].paths[pred] = pathCopy

### unit -> PredicateVisitor
class PredicateVisitor:
    """Top-level abstract class for."""
    __metaclass__ = ABCMeta

    ### PredTop -> a 
    @abstractmethod
    def predTop(self, predTop):
        """Called by walk on PredTop"""
        pass

    ### PredBottom -> a 
    @abstractmethod
    def predBottom(self, predBottom):
        """Called by walk on PredBottom"""
        pass

    ### Pred -> a 
    @abstractmethod
    def pred(self, pred):
        """Called by walk on Pred"""
        pass

    ### PredUnion -> a 
    @abstractmethod
    def predUnion(self, predUnion):
        """Called by walk on PredUnion"""
        pass

    ### PredIntersection -> a 
    @abstractmethod
    def predIntersection(self, predIntersection):
        """Called by walk on PredIntersection"""
        pass

    ### PredNegation -> a 
    @abstractmethod
    def predNegation(self, predNegation):
        """Called by walk on PredNegation"""
        pass

class ActionVisitor:
    """Top-level abstract class for the action visitor pattern."""
    __metaclass__ = ABCMeta

    ### ActionForward -> a
    @abstractmethod
    def actionForward(self, actionForward):
        """Called by walk on ActionForward."""
        pass

    ### ActionCountPackets -> a
    @abstractmethod
    def actionCountPackets(self, actionCountPackets):
        """Called by walk on ActionCountPackets."""
        pass

    ### ActionCountBytes -> a
    @abstractmethod
    def actionCountBytes(self, actionCountBytes):
        """Called by walk on ActionCountBytes."""
        pass

    ### ActionGetPackets -> a
    @abstractmethod
    def actionGetPacket(self, actionGetPacket):
        """Called by walk on ActionGetPacket."""
        pass

    ### ActionMonitorSwitch -> a
    @abstractmethod
    def actionGetPacket(self, actionGetPacket):
        """Called by walk on ActionGetPacket."""
        pass


class PolicyVisitor(PredicateVisitor, ActionVisitor):
    """Top-level abstract class for."""
    __metaclass__ = ABCMeta

    ### PolEmpty -> a 
    @abstractmethod
    def polEmpty(self, polEmpty):
        """Called by walk on PolEmpty"""
        pass

    ### PolProcessIn -> a 
    @abstractmethod
    def polProcessIn(self, polProcessIn):
        """Called by walk on PolProcessIn"""
        pass

    ### PolUnion -> a 
    @abstractmethod
    def polUnion(self, polUnion):
        """Called by walk on PolUnion"""
        pass

    ### PolRestrict -> a 
    @abstractmethod
    def polRestrict(self, polRestrict):
        """Called by walk on PolRestrict"""
        pass

    ### PolGenPacket -> a 
    @abstractmethod
    def polGenPacket(self, polGenPacket):
        """Called by walk on PolGenPacket"""
        pass

    ### PolicySequence -> a 
    @abstractmethod
    def policySequence(self, policySequence):
        """Called by walk on PolicySequence"""
        pass


### unit -> PredicateVisitor
class PV_len(PolicyVisitor):

    ### PredTop -> a 
    def predTop(self, predTop):
        return 1

    ### PredBottom -> a 
    def predBottom(self, predBottom):
        return 1

    ### Pred -> a 
    def pred(self, pred):
        return 1

    ### PredUnion -> a 
    def predUnion(self, predUnion):
        return predUnion.left.walk(self) + predUnion.right.walk(self)

    ### PredIntersection -> a 
    def predIntersection(self, predIntersection):
        return predIntersection.left.walk(self) + predIntersection.right.walk(self)

    ### PredNegation -> a
    def predNegation(self, predNegation):
        return 1

    ### PolEmpty -> a 
    def polEmpty(self, polEmpty):
        return polEmpty.predicate.walk(self)

    ### PolProcessIn -> a 
    def polProcessIn(self, polProcessIn):
        return polProcessIn.predicate.walk(self)

    ### PolUnion -> a 
    def polUnion(self, polUnion):
        return polUnion.left.walk(self) + polUnion.right.walk(self)

    ### PolRestrict -> a 
    def polRestrict(self, polRestrict):
        return polRestrict.policy.walk(self) + polRestrict.predicate.walk(self)

    ### PolGenPacket -> a
    def polGenPacket(self, polGenPacket):
        return 1

    ### PolicySequence -> a 
    def policySequence(self, policySequence):
        return policySequence.left.walk(self) + policySequence.right.walk(self)

    # Actions are not counted toward the total number of predicates.
    def actionForward(self, actionForward):
        pass
    def actionCountPackets(self, actionCountPackets):
        pass
    def actionCountBytes(self, actionCountBytes):
        pass
    def actionGetPacket(self, actionGetPacket):
        pass
    def actionMonitorSwitch(self, actionMonitorSwitch):
        pass


### unit -> Predicate
class Predicate:
    """Top-level abstract class for predicates."""
    __metaclass__ = ABCMeta

    ### Packet -> Bool
    @abstractmethod
    def match(self, packet):
        """Does this header match this located packet?"""
        pass

# Primitive predicates
### unit -> Predicate
class PredTop(Predicate):
    """The always-true predicate."""

    def __str__(self):
        return "*"

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __repr__(self):
        return self.__str__()

    def __and__(self,other):
        return PredIntersection(self,other)

    def __or__(self,other):
        return PredUnion(self,other)

    def __eq__(self,other):
        return str(self) == str(other)

    def match(self, packet):
        return True

    def walk(self,pv):
        return pv.predTop(self)

### unit -> Predicate
class PredBottom(Predicate):
    """The always-false predicate."""
    def __str__(self):
        return "!*"

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __repr__(self):
        return self.__str__()

    def __and__(self,other):
        return PredIntersection(self,other)

    def __or__(self,other):
        return PredUnion(self,other)

    def __eq__(self,other):
        return str(self) == str(other)

    def match(self, packet):
        return False

    def walk(self,pv):
        return pv.predBottom(self)
    
HEADER_FIELDS = list(set ([
  , 'DlSrc'         #  ^Match ethernet source address
  , 'DlDst'         #  ^Match ethernet destination address
  , 'DlTyp'         #  ^Match ethernet type code (e.g., 0x0800 for IP packets)
  , 'DlVlan'        #  ^Match VLAN tag
  , 'DlVlanPcp'     #  ^Match VLAN priority
  , 'NwSrc'         #  ^Match source IP address
  , 'NwDst'         #  ^Match destination IP address
  , 'NwProto'       #  ^Match IP protocol code (e.g., 0x6 indicates TCP segments)
  , 'NwTos'         #  ^Match IP TOS field
  , 'TpSrcPort'     #  ^Match IP source port
  , 'TpDstPort'     #  ^Match IP destination port
  , 'IngressPort'   #  ^Match the ingress port on which packets arrive
  , 'Switch'        #  ^Match only at this switch
  ]))

### Field -> Pattern -> Predicate
class Pred(Predicate):
    """A basic predicate matching against a single field"""
    def __init__(self, field, pattern):
        """
       ARGS:
            field: header field to match pattern against
            pattern: an exact match pattern for that field

            The 'NwSrc' and 'NwDst' fields are an exception
            in that their patterns must be pairs of type
            (ip, bitmask), where ip is a 32-bit IP address
            and bitmask is a 32-bit field containing values
            2^n-1 where n ranges from 0 to 32.
        """
        assert(field in HEADER_FIELDS)
        self.field = field
        self.pattern = pattern

    def __str__(self):
        return "%s:%s" % (self.field, self.pattern)

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __repr__(self):
        return self.__str__()

    def __and__(self,other):
        return PredIntersection(self,other)

    def __or__(self,other):
        return PredUnion(self,other)

    ### Predicate -> Bool
    def __eq__(self,other):
        return self.field == other.field and self.pattern == other.pattern

    ### Predicate -> Bool
    def __lt__(self,other):
        if self.field != other.field:
            return HEADER_FIELDS.index(self.field) > HEADER_FIELDS.index(other.field)
        else:
            return self.pattern < other.pattern

    ### Predicate -> Bool
    def __le__(self,other):
        if self.field != other.field:
            return HEADER_FIELDS.index(self.field) > HEADER_FIELDS.index(other.field)
        else:
            return self.pattern <= other.pattern

    ### Predicate -> Bool
    def __gt__(self,other):
        if self.field != other.field:
            return HEADER_FIELDS.index(self.field) < HEADER_FIELDS.index(other.field)
        else:
            return self.pattern > other.pattern

    ### Predicate -> Bool
    def __ge__(self,other):
        if self.field != other.field:
            return HEADER_FIELDS.index(self.field) < HEADER_FIELDS.index(other.field)
        else:
            return self.pattern >= other.pattern

    def wild_match(self, ip_str):
        prefix = ipstr_to_int(self.pattern.split('/')[0])
        mask = ""
        len_prefix = int(self.pattern.split('/')[1])
        for i in range(len_prefix):
            mask += '1'
        for i in range(32 - len_prefix):
            mask += '0'
        mask = int(mask, 2)
        ip = ipstr_to_int(ip_str)
        return (ip & mask) == (prefix & mask)

    def match(self, packet):
        # PREFIX MATCH
        if (self.field == 'srcip' or self.field == 'dstip') and self.pattern.find('/') != -1:
            return self.wild_match(packet.header.field[self.field])
        # EXACT MATCH
        else:
            return self.pattern == packet.header.field[self.field]

    def walk(self,pv):
        return pv.pred(self)
        

# Compound predicates
### Predicate -> Predicate -> Predicate
class PredUnion(Predicate):
    """A predicate representing the union of two predicates."""
    def __init__(self, left, right):
        """
        ARGS:
            left: first predicate to union
            right: second predicate to union
        """
        assert(isinstance(left, Predicate))
        assert(isinstance(right, Predicate))
        self.left = left
        self.right = right

    def __str__(self):
        lstr = str(self.left)
        rstr = str(self.right)
        lstr = '(' + lstr + ')'
        rstr = '(' + rstr + ')'
        return "%s | %s" % (lstr, rstr)

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __repr__(self):
        self.__str__()

    def __and__(self,other):
        return PredIntersection(self,other)

    def __or__(self,other):
        return PredUnion(self,other)

    ### Predicate -> Bool  TBD, RECURSIVE EQ LOGIC
    def __eq__(self,other):
        return str(self) == str(other)

    def match(self, packet):
        return self.left.match(packet) or self.right.match(packet)

    def walk(self,pv):
        return pv.predUnion(self)    


### Predicate -> Predicate -> Predicate
class PredIntersection(Predicate):
    """A predicate representing the intersection of two predicates."""
    def __init__(self, left, right):
        """
        ARGS:
            left: first predicate to intersection
            right: second predicate to intersection
        """
        assert(isinstance(left, Predicate))
        assert(isinstance(right, Predicate))
        self.left = left
        self.right = right

    def __str__(self):
        lstr = str(self.left)
        rstr = str(self.right)
        lstr = '(' + lstr + ')'
        rstr = '(' + rstr + ')'
        return "%s & %s" % (lstr, rstr)

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __repr__(self):
        return self.__str__()

    def __and__(self,other):
        return PredIntersection(self,other)

    def __or__(self,other):
        return PredUnion(self,other)

    ### Predicate -> Bool  TBD, RECURSIVE EQ LOGIC
    def __eq__(self,other):
        return str(self) == str(other)

    def match(self, packet):
        return self.left.match(packet) and self.right.match(packet)

    def walk(self,pv):
        return pv.predIntersection(self)    


# Predicate -> Predicate
class PredNegation(Predicate):
    """A predicate representing the negation of a predicate."""
    def __init__(self, pred):
        """
        ARGS:
            pred: the predicate to negate
        """
        assert(isinstance(pred, Predicate))
        self.pred = right

    def __str__(self):
        return "-(%s)" % self.pred

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __repr__(self):
        return self.__str__()

    def __and__(self,other):
        return PredIntersection(self,other)

    def __or__(self,other):
        return PredUnion(self,other)

    ### Predicate -> Bool  TBD, RECURSIVE EQ LOGIC
    def __eq__(self,other):
        return str(self) == str(other)

    def match(self, packet):
        return not self.pred.match(packet)

    def walk(self,pv):
        return pv.predNegation(self)    
    

class Mod:
    """An abstract modification comprising a field and value."""
    def __init__(self, field=None, val=None):
        self.field = field
        self.val = val

    def __str__(self):
        return "%s<=%s" % (self.field, self.val)

    def __repr__(self):
        return self.__str__()


### unit -> Predicate
class Action:
    """Top-level abstract class for actions."""
    __metaclass__ = ABCMeta

class ActionForward(Action):
    """A high-level action that consists of a modification and a port to forward out."""
    def __init__(self, modification={}, port):
        """
        ARGS:
            modification: a dictionary of field modifications to be applied
            port: port on which to forward packet
        """
        assert(isinstance(modification, type({})))
        self.modification = modification
        self.port = port

    def __str__(self):
        return "%s,=>%s" % (self.modification,self.port)

    def __repr__(self):
        return self.__str__()

    def walk(self, pv):
        pv.actionForward(self)

class ActionCountPackets(Action):
    """Periodically retrieve the number of packets that this action has been applied to."""
    def __init__(self, interval, queue):
        assert(isinstance(interval, type(1)))
        self.interval = interval
        self.queue = queue

    def __str__(self):
        return "CountPackets(%s)" % interval

    def __repr__(self):
        return self.__str__()

    def walk(self, pv):
        pv.actionCountPackets(self)

class ActionCountBytes(Action):
    """Periodically retrieve the number of bytes of packets that this action has been applied to."""
    def __init__(self, interval, queue):
        assert(isinstance(interval, type(1)))
        self.interval = interval
        self.queue = queue

    def __str__(self):
        return "CountBytes(%s)" % interval

    def __repr__(self):
        return self.__str__()

    def walk(self, pv):
        pv.actionCountBytes(self)

class ActionGetPacket(Action):
    """Send packets to which this action is applied to the controller."""
    def __init__(self, queue):
        self.queue = queue

    def __str__(self):
        return "GetPackets"

    def __repr__(self):
        return self.__str__()

    def walk(self, pv):
        pv.actionGetPacket(self)

class ActionMonitorSwitch(Action):
    """Monitor switch events."""
    def __init__(self, queue):
        self.queue = queue

    def __str__(self):
        return "MonitorSwitch"

    def __repr__(self):
        return self.__str__()

    def walk(self, pv):
        pv.actionMonitorSwitch(self)

class Policy:
    """Top-level abstract description of a static network program."""
    __metaclass__ = ABCMeta

#     @abstractmethod
#     # Packet -> [ Action ]
#     def get_actions(self, packet):
#         """Get order list of atomic actions this policy generates for a packet."""
#         pass

class PolEmpty(Policy):
    """Policy that drops everything."""
    def __init__(self):
        """
        ARGS:
            predicate: predicate under which to apply action
            actions: a list of Actions to apply to packets which match
                predicate.  Each action is applied to such a
                packet, first effecting any modifications in the action, then
                forwarding out any given ports, before applying the next
                action.  In this way, a PrimitivePolicy may produce multiple packets.
                Note that actions may be the empty list.
        """
        self.predicate = PredTop()
        self.actions = []

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __str__(self):
        return "PolEmpty"

    def __repr__(self):
        return self.__str__()

    def __or__(self,other):
        return PolUnion(self,other)

    def __sub__(self,pred):
        return PolRestrict(self,pred)

    def __eq__(self,other):
        return str(self) == str(other)

#     ### Packet -> [ Action ]
#     def get_actions(self, packet):
#         return self.actions

    def walk(self,pv):
        return pv.polEmpty(self)



# THE TOP POLICY CONTAINS EVERY POSSIBLE COMBINATION OF ACTIONS
# NOT TRACTABLY EXPRESSIBLE IN THIS LANGUAGE

class PolProcessIn(Policy):
    """Policy for mapping a single predicate to a list of actions."""
    def __init__(self, predicate, actions):
        """
        ARGS:
            predicate: predicate under which to apply action
            actions: a list of Actions to apply to packets which match
                predicate.  Each action is applied to such a
                packet, first effecting any modifications in the action, then
                forwarding out any given ports, before applying the next
                action.  In this way, a PrimitivePolicy may produce multiple packets.
                Note that actions may be the empty list.
        """
        assert(isinstance(predicate, Predicate))
        assert(isinstance(actions, type([])))
        self.predicate = predicate
        self.actions = actions

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __str__(self):
        return "%s -> %s" % (self.predicate, self.actions)

    def __repr__(self):
        return self.__str__()

    def __or__(self,other):
        return PolUnion(self,other)

    def __sub__(self,pred):
        return PolRestrict(self,pred)

    def __eq__(self,other):
        return str(self) == str(other)

#     ### Packet -> [ Action ]
#     def get_actions(self, packet):
#         if self.predicate.match(packet):
#             return self.actions
#         else:
#             return list([])

    def walk(self,pv):
        return pv.pol(self)

class PolUnion(Policy):
    """The union of two policies."""
    def __init__(self, left, right):
        """
        ARGS:
            left: first policy to union
            right: second policy to union
        """
        assert(isinstance(left, Policy))
        assert(isinstance(right, Policy))
        self.left = left
        self.right = right

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __str__(self):
        return "%s\n%s" % (self.left, self.right)

    def __repr__(self):
        return self.__str__()

    def __or__(self,other):
        return PolUnion(self,other)

    def __sub__(self,pred):
        return PolRestrict(self,pred)

#     ### Packet -> [ Action ]
#     def get_actions(self, packet):
#         left_actions = self.left.get_actions(packet)
#         right_actions = self.right.get_actions(packet)
#         return left_actions + right_actions

    def walk(self,pv):
        return pv.polUnion(self)

class PolRestrict(Policy):
    """A policy restricted by a predicate."""
    def __init__(self, policy, predicate):
        """
        ARGS:
            policy: policy to restrict
            predicate: predicate to restrict it by
        """
        assert(isinstance(policy, Policy))
        assert(isinstance(predicate, Predicate))
        self.policy = policy
        self.predicate = predicate

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __str__(self):
        return "Restrict[%s\n%s]" % (self.predicate, self.policy)

    def __repr__(self):
        return self.__str__()

    def __or__(self,other):
        return PolUnion(self,other)

    def __sub__(self,pred):
        return PolRestrict(self.policy, self.predicate | pred)

#     ### Packet -> [ Action ]
#     def get_actions(self, packet):
#         if self.predicate.match(packet):
#             return list([])
#         else:
#             return self.policy.get_actions(packet)

    def walk(self,pv):
        return pv.polRestrict(self)

class PolGenPacket(Policy):
    """Policy for injecting packets into the network."""
    def __init__(self, queue):
        """
        ARGS:
            queue: writing pairs of ((switch, port), bytestring) will inject
                the bytestring (packet) into the network at the port and
                switch specified.
        """
        self.queue = queue

    def key(self):
        return(self.__class__.__name__, str(self))

    def __hash__(self):
        return hash(self.key())

    def __str__(self):
        return "PolGenPacket"

    def __repr__(self):
        return self.__str__()

    def __or__(self,other):
        return PolUnion(self,other)

    def __sub__(self,pred):
        return PolRestrict(self,pred)

#     ### Packet -> [ Action ]
#     def get_actions(self, packet):
#         return list([])

    def walk(self,pv):
        return pv.polGenPacket(self)

# class PolicySequence(Policy):
#     """The sequential composition of two policies."""
#     def __init__(self, left, right):
#         """
#         ARGS:
#             left: first policy to apply
#             right: second policy to apply
#         """
#         assert(isinstance(left, Policy))
#         assert(isinstance(right, Policy))
#         self.left = left
#         self.right = right
# 
#     def key(self):
#         return(self.__class__.__name__, str(self))
# 
#     def __hash__(self):
#         return hash(self.key())
# 
#     def __str__(self):
#         return "%s\n%s" % (self.left, self.right)
# 
#     def __repr__(self):
#         return self.__str__()
# 
#     def __or__(self,other):
#         return PolUnion(self,other)
# 
#     def __sub__(self,pred):
#         return PolRestrict(self,pred)
# 
# #     ### Packet -> [ Action ]
# #     def get_actions(self, packet):
# #         return list([])
# 
#     def walk(self,pv):
#         return pv.policySequence(self)

