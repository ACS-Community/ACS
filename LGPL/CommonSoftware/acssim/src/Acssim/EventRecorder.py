#!/usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2005
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: EventRecorder.py,v 1.1 2010/10/01 17:20:48 javarias Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# rhiriart  2008-01-14  created
#

from Acspy.Nc.Consumer import Consumer
from Acspy.Nc.Supplier import Supplier
from xml.dom.minidom import parseString

import sys, time, pickle

class EventRecorder(Consumer):

    def __init__(self, channel_name):
        '''
        Constructor.

        Parameters:

        channel_name Channel name.
        '''
        Consumer.__init__(self, channel_name, None)

        self.channel_name = channel_name
        self.rec_file_name = self.channel_name + '.xml' 
        self.rec_file = None
        self.start_time = -1

    def begin(self):
        '''
        Begin the event recording session.
        '''
        self.start_time = time.time()
        self.rec_file = open(self.rec_file_name, 'w')
        self.rec_file.write('<?xml version="1.0" encoding="UTF-8"?>\n')
        self.rec_file.write('<Simulation-NCSession start-time="' + str(self.start_time) +
                            '" channel-name="' + self.channel_name + '">\n')
        self.rec_file.flush()
    
        self.consumerReady()

    def end(self):
        '''
        End the event recording session.
        '''
        self.disconnect()
        if self.rec_file:
            self.rec_file.write('</Simulation-NCSession>\n')
            self.rec_file.close()

    def replaceXMLEntities(self, s):
        '''
        Replaces special characters of its corresponding XML entities.

        & -> &amp;
        ' -> &apos;
        < -> &lt;
        > -> &gt;
        " -> &quot;
        '''
        return s.replace('&', '&amp;').replace('\'', '&apos;').replace('<', '&lt;').replace('>', '&gt;').replace('"', '&quot;')


    def processEvent(self, type_name=None, event_name=None, corba_any=None, se=None):
        '''
        This method gets called when an event is received.
        It is overriding Consumer.processEvent().
        '''
        t = time.time() - self.start_time
        s = '  <Event event-name="' + type_name + '" time="'+ str(t) + '">'
        print 'CORBA Any Value: ' + str(corba_any.value())
        print 'CORBA Any Value dir: ' + str(dir(corba_any.value()))
        s += self.replaceXMLEntities(pickle.dumps(self._transformUnknownStruct(corba_any.value())))
        # s += self.replaceXMLEntities(pickle.dumps(corba_any.value()))
        s += '</Event>\n'
        if self.rec_file:
            self.rec_file.write(s)

    def _transformUnknownStruct(self, us):
        '''
        Transforms the omniORB UnknownStruct (a hidden, internal type that is used to implement
        CORBA Anys in OmniORB and that can't be pickled).
        This method is recursive, nested UnknownStructs will be converted as well.

        Parameters:
        us - UnknownStruct instance.

        Retuns: The good, 'ol, known structure.
        '''
        id = us._NP_RepositoryId
        if not self._isUnknownStruct(us): return us

        members = us._members
        values = us._values
 
        kargs = {}
        for i in range(len(members)):
            arg = values[i]
            # if '_NP_RepositoryId' in dir(arg):
            if self._isUnknownStruct(arg):
                arg = self._transformUnknownStruct(arg)
            kargs[members[i]] = arg

        modName = id.split(':')[1].split('/')[1]
        clsName = id.split(':')[1].split('/')[2]
        mod = __import__(modName) 
        cls = mod.__dict__[clsName] 
        ks = apply(cls, [], kargs) # "known" struct instance
        return ks

    def _isUnknownStruct(self, us):
        d = dir(us)
        print 'We have a', str(us)
        print 'With dictionary:', str(d)
        r = ('_members' in d) and ('_values' in d) and (str(us)[:13] == 'UnknownStruct')
        print 'So it is', str(r)
        return r
       

class Event:

    def __init__(self, publisher, name, evst):
        self.publisher = publisher
        self.name = name
        self.evStruct = evst

    def getName(self):
        return self.name

    def getEventStruct(self):
        return self.evStruct


    def publish(self):
        self.publisher.publishEvent(self.evStruct)

class Player:

    def __init__(self, file_name):
        '''
        Constructor.

        Parameters:
        file_name - The name of a previously recorded event consumer session. 
                    This file should have been created with the EventRecorder
                    class.
        '''
        f = open(file_name)
        xml_doc = f.read()
        root_dom = parseString(xml_doc)

        self.events = []
        session_dom = root_dom.firstChild
        channel_name = session_dom.getAttribute('channel-name')

        self.supplier = Supplier(channel_name)

        for node in session_dom.childNodes:
            if node.nodeName == 'Event':
                event_name = node.getAttribute('event-name')
                # stringified event structure
                ses = self.restoreXMLEntities(str(node.firstChild.nodeValue))
                se = pickle.loads(ses) # event structure

                self.events.append(Event(self.supplier, str(event_name), se))

    def __getitem__(self, i):
        '''
        Overriden indexing operation.
        '''
        return self.events[i]

    def __len__(self):
        '''
        Overriden length operation.
        '''
        return len(self.events)

    def restoreXMLEntities(self, s):
        '''
        Utilitiy function. Replaces XML entities for its respective characters.

        &amp   -> &
        &apos  -> '
        &lt;   -> <
        &gt;   -> >
        &quot; -> "
        '''
        return s.replace('&quot;', '"').replace('&gt;', '>').replace('&lt;', '<').replace('&apos;', '\'').replace('&amp;', '&')



if __name__ == "__main__":
    recorder = EventRecorder(sys.argv[1])
    recorder.begin()
    print "Press a key to finish..."
    sys.stdin.read(1)
    recorder.end()

# __oOo__
