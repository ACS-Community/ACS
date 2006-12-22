#!/usr/bin/env python

# @(#) $Id: ACSEventAdmin.py,v 1.11 2006/12/22 23:34:43 sharring Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA, 2001
#    (c) European Southern Observatory, 2002
#    Copyright by ESO (in the framework of the ALMA collaboration)
#    and Cosylab 2002, All rights reserved
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#------------------------------------------------------------------------------
'''
Python script which starts the ACS Event Browser. The only requirement for the
GUI to run is that the ACS Manager is up and running.

TODO:
- provide some sort of sorting mechanism to filter the events within the GUI...
- provide a better exit command to run
- This CVS module probably is not the best place for this package...
- Modular test!
'''
__version__ = "$Id: ACSEventAdmin.py,v 1.11 2006/12/22 23:34:43 sharring Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
from time       import sleep
from thread     import start_new_thread
from traceback  import print_exc
#--CORBA STUBS-----------------------------------------------------------------
from CosNotifyChannelAdmin import EventChannelFactory
from acsnc__POA            import ACSEventAdmin
from ACS                   import CBDescOut
from ACSErr                import Completion
import acsnc
import acscommon
import CosNotification
#--ACS Imports-----------------------------------------------------------------
from Acspy.Clients.SimpleClient        import PySimpleClient
from Acspy.Nc.Consumer                 import Consumer
from Acspy.Util.ACSCorba               import getORB
from Acspy.Util                        import NameTree

from acsncImpl.ACSEventAdminGUI import ACSEventAdminGUI
from acsncImpl.ACSEventAdminGUI import EVENT_CACHE
#--GLOBALS---------------------------------------------------------------------
#The fields of a channel dictionary entry
CONSUMER = 'CONSUMER'    #refers to the reference to the Consumer object
CHANNEL  = 'CHANNEL'     #refers to the notification channel object

#This list conists of all callback objects that have been registered with the
#ACSEventAdmin. It's globally available because the Consumer objects need direct
#access to it also
CALLBACKS = []

#------------------------------------------------------------------------------
class AdminConsumer(Consumer):
    '''
    This is a high-level consumer designed to process each and every event
    from a given channel.  Each time an event is received, a formatted string
    is sent to any registered callbacks using the working method.
    '''
    def __init__(self, channelName):
        '''
        Constructor.

        Handles absolutely everything. Once the constructor has been called,
        there is absolutely nothing for the developer to do other than let
        it run.
        
        Parameters:
        - channelName is the name of the channel to connect to

        Raises: ???
        '''
        
        #this member is the total number of events that have been received for
        #this particular channel
        self.count = 0

        #a dictionary where event_type's are the keys and the values are the
        #total number of that type of event received
        self.typeCount = {}

        #just call the superclass constructor
        Consumer.__init__(self, channelName)
        
        #subscribe to ALL events. in theory we could just invoke addSubscription("*")
        #but TAO Notification Service does NOT adhere to the OMG specs in this case.
        self.consumerAdmin.subscription_change([CosNotification.EventType("*", "*")],[])

        #ready to begin processing events
        self.consumerReady()
        return
    #------------------------------------------------------------------------------
    def push_structured_event (self, event):
        '''
        CORBA method that is invoked by suppliers. push_structured_event is overriden
        instead of processEvent because there\'s all sorts of good info to be obtained
        from the entire StructuredEvent.

        Parameters: event is a CosNotification.StructuredEvent
        
        Returns: Nothing

        Raises: ???
        '''
        global CALLBACKS
        global EVENT_CACHE
        
        #increment the event counter
        self.count = self.count + 1
        
        #stuctured event name...normally this is empty
        #name = event.header.fixed_header.event_name

        #typeName...name of the ICD event (IDL structure)
        typeName = event.header.fixed_header.event_type.type_name

        #increase the total number of events of this type received
        if not self.typeCount.has_key(typeName):
            self.typeCount[typeName] = 1
        else:
            self.typeCount[typeName] = self.typeCount[typeName] + 1

        #always ALMA
        #domainName = event.header.fixed_header.event_type.domain_name

        #real data value
        data = event.filterable_data[0].value.value()

        #time the event occured
        time = event.remainder_of_body.value().timestamp

        #name of the component responsible for the supplier that sent the event
        component = event.remainder_of_body.value().name

        #event number from that particular supplier
        count = event.remainder_of_body.value().count

        #["Timestamp", "Channel", "Source", "SupplierEvent#", "ChannelEvent#", "Type", "TypeCount"]
        output = str(time) + " " + self.channelName + " " + component + " " + str(count) + " " + str(self.count) + " " + typeName + " " + str(self.typeCount[typeName])

        EVENT_CACHE[output] = data

        #there can be any number of callbacks registered so we must iterate through
        #all of them
        for cb in CALLBACKS:
            try:
                cb.working(output,
                           Completion(long(0), long(0), long(0), []),
                           CBDescOut(long(0), long(0)))
            except:
                #if the above invocation fails for any reason, it must be because
                #the callback no longer really exists...remove it.
                self.logger.logWarning("A callback instance for the '" + self.channelName +
                                       "' channel has mysteriously disappeared...removing.")
                print_exc()
                CALLBACKS.remove(cb)
        return

#------------------------------------------------------------------------------
class ACSEventAdmin(PySimpleClient):
    '''
    This class is basically the back-end of the GUI and does all the hard work:
    - finding channels
    - dynamically creating consumers for the channels
    - keeping track of events
    - etc.
    '''
    def __init__(self, name="ACS Event Admin Client"):
        '''
        Just call superclass constructors here.
        '''
        PySimpleClient.__init__(self, name)

        #dictionary which consists of all active channels
        #the keys here are the channelNames in string format
        self.channels = {}
        
        #so long as this evaluates to true, the thread continues executing
        self.running = 1

        #start a new thread to continuously look for new channels in the naming
        #service
        self.getLogger().logInfo("Creating a thread to poll the CORBA Naming Service for new channels...")

        #Get the Naming Service helper class
        self.nt = NameTree.nameTree(getORB())
        
        start_new_thread(self.pollNamingService, ())
        return    
    #------------------------------------------------------------------------------
    def disconnect(self):
        '''
        Override this method.
        '''
        global CALLBACKS
        
        #tell threads to stop
        self.running = 0
        #give them a few seconds to stop executing
        sleep(2)
        
        #first destroy all consumers
        self.getLogger().logInfo("Disconnecting all administrative consumers...")
        for key in self.channels.keys():
            self.channels[key][CONSUMER].disconnect()

        #next notify all callbacks that we're shutting down
        self.getLogger().logInfo("Disconnecting all registered callbacks...")
        for cb in CALLBACKS:
            try:
                cb.done("The ACSEventAdmin is shutting down.",
                        Completion(long(0), long(0), long(0), []),
                        CBDescOut(long(0), long(0)))
            except:
                self.getLogger().logWarning("Failed to invoke done method on a callback!")
                print_exc()

        #now delete member data
        self.channels = {}
        CALLBACKS = []

        PySimpleClient.disconnect(self)
        return
    #------------------------------------------------------------------------------
    def createChannel(self, channelName, initialQOS, initialAdmin):
        '''
        Python implementation of IDL method.
        void createChannel(in string channelName,
			   in CosNotification::QoSProperties initialQOS,
			   in CosNotification::AdminProperties initialAdmin);
        '''

        #first we see if the channel already exists...
        if self.channels.has_key(channelName):
            self.getLogger().logWarning("Cannot create the '" + channelName + "' channel because it already exists!")
            return

        try:
            #Get at the Notification Service first.
            cf = self.nt.getObject(acscommon.NOTIFICATION_FACTORY_NAME,"")._narrow(EventChannelFactory)
            (evtChan, chan_id) = cf.create_channel(initialQOS, initialAdmin)
            # Register the new channel w/ the naming service under the names &
            # type. The event channel is now ready for action.
            self.nt.putObject(channelName, acscommon.NC_KIND, evtChan)
        except Exception, e:
            self.getLogger().logWarning("Cannot create the '" + channelName + "' channel: " + str(e))
            print_exc()
            
        return
    #------------------------------------------------------------------------------
    def configChannelProperties(self, channelName, newQOS, newAdmin):
        '''
        Python implementation of IDL method.
        void configChannelProperties(in string channelName,
				     in CosNotification::QoSProperties newQOS,
				     in CosNotification::AdminProperties newAdmin);
        '''

        #first we see if the channel already exists...
        if not self.channels.has_key(channelName):
            self.getLogger().logWarning("Cannot reconfigure the '" + channelName +
                                        "' channel's properties  because it does not exist!")
            return

        try:
            self.channels[channelName][CHANNEL].set_qos(newQOS)
        except CosNotification.UnsupportedQoS, e:
            self.getLogger().logWarning("Failed to reconfigure the '" + channelName +
                                        "' channel's Q of S properties  because:" + str(e))
            print_exc()

        try:
            self.channels[channelName][CHANNEL].set_admin(newAdmin)
        except CosNotification.UnsupportedAdmin, e:
            self.getLogger().logWarning("Failed to reconfigure the '" + channelName +
                                        "' channel's admin properties  because:" + str(e))
            print_exc()
        
        return
    #------------------------------------------------------------------------------
    def destroyChannel(self, channelName):
        '''
        Python implementation of IDL method.
        void destroyChannel(in string channelName);
        '''

        #first we see if the channel already exists...
        if not self.channels.has_key(channelName):
            self.getLogger().logWarning("Cannot destroy the '" + channelName +
                                        "' channel because it does not exist!")
            return

        try:
            #Unregister our channel with the naming service
            self.nt.delObject(channelName, acscommon.NC_KIND)

            #disconnect our own consumer first
            self.channels[channelName][CONSUMER].disconnect()

            #next destroy the channel in the same process where the CORBA Notification
            #service is executing
            self.channels[channelName][CHANNEL].destroy()

            #finally update our list
            del self.channels[channelName]
            
        except Exception, e:
            self.getLogger().logWarning("Cannot destroy the '" + channelName +
                                        "' channel: " + str(e))
            print_exc()
            
        return
    #------------------------------------------------------------------------------
    def getChannelInfo(self, channelName):
        '''
        Python implementation of IDL method.
        void getChannelInfo(in string channelName, 
			    out unsigned short numSuppliers,
			    out unsigned short numConsumers,
			    out unsigned long long totalEvents,
			    out CosNotification::QoSProperties initialQOS,
			    out CosNotification::AdminProperties initialAdmin);
        '''
        #first we see if the channel does not exist...
        if not self.channels.has_key(channelName):
            self.getLogger().logWarning("Cannot get info on the '" + channelName +
                                        "' channel because it does not exist!")
            return

        #In theory the number of admins is the same as the number of proxies
        numSuppliers = len(self.channels[channelName][CHANNEL].get_all_supplieradmins()) - 1
        #should subtract out the event admin's consumer but what if there are multiple GUIs running...
        #in that case, it makes sense just to leave this as is.
        numConsumers = len(self.channels[channelName][CHANNEL].get_all_consumeradmins()) - 1
        
        #get the quality of service properties
        initialQOS =  self.channels[channelName][CHANNEL].get_qos()

        #get the admin properties
        initialAdmin = self.channels[channelName][CHANNEL].get_admin()

        #event counter for this channel
        totalEvents = self.channels[channelName][CONSUMER].count

        #even type counter for this channel
        eventTypes = []
        for key in self.channels[channelName][CONSUMER].typeCount.keys():
            eventTypeInfo = str(key) + ': ' + str(self.channels[channelName][CONSUMER].typeCount[key])
            eventTypes.append(eventTypeInfo)
        
        return (numSuppliers, numConsumers, totalEvents, initialQOS, initialAdmin, eventTypes)
    #------------------------------------------------------------------------------
    def getActiveChannels(self):
        '''
        Python implementation of IDL method.
        NCSeq getActiveChannels();
        '''
        #return a list containing all channels this administrator knows of
        return self.channels.keys()
    #------------------------------------------------------------------------------
    def monitorEvents(self, cb, desc):
        '''
        Python implementation of IDL method.
        void monitorEvents(in ACS::CBstring cb,
			   in ACS::CBDescIn desc);
        '''
        global CALLBACKS
        #save the callback so consumers can use it
        CALLBACKS.append(cb)
        return
    #------------------------------------------------------------------------------
    #--Helper methods--------------------------------------------------------------
    #------------------------------------------------------------------------------
    def pollNamingService(self):
        '''
        This method is designed to be run as a thread. All it does is constantly
        poll the naming service looking for new Notification Channels. Once a new
        channel is found, a new AdminConsumer is created.
        
        Parameters: None

        Returns: Nothing...this method is designed to be a separate thread

        Raises: ???
        '''

        #look for new channels while this component is not being shutdown
        while self.running:
            #get a list of all root naming contexts
            ncList = self.nt.listdir()
            #look at a single naming context
            for nc in ncList:
                #this is the string we're interested in
                nc = nc[0]  #i.e., "fridge.channels"
                #search for a ".channels"
                if nc.count(".channels") == 1:
                    #good we have a channel...split it.
                    nc = nc.replace(".channels", "")   #i.e., "fridge.channels" becomes "fridge"
                    #if this channel has not already been registered, it will
                    #be after this invocation completes
                    self.addChannel(nc)
            #sleep so this component does not hog too much CPU time
            sleep(1)

        return
    #------------------------------------------------------------------------------
    def addChannel(self, channelName):
        '''
        Method adds an AdminConsumer for the channel if it is not already registered
        with this component.
        
        Parameters:
        - channelName is the name of a channel registered with the Naming Service

        Returns: Nothing

        Raises: ???
        '''
        #make sure we don't already have a local consumer here!
        if not self.channels.has_key(channelName):
            #create a temporary dictionary 
            tDict = {}
            tDict[CONSUMER] = AdminConsumer(channelName)
            tDict[CHANNEL] = tDict[CONSUMER].evtChan

            #store the temporary dictionary into the channels dictionary
            self.channels[channelName] = tDict

#------------------------------------------------------------------------------
import Tkinter
import Pmw
from optparse import OptionParser
import signal

#-----------------------------------------------------------------------------
def signalHandler(signum, frame):
   '''
      Method to handle signals and make sure everything is cleaned up properly.
   
     Parameters:
     - signum the signal number
     - frame the frame object 

     Returns: Nothing

     Raises: ???
   '''
   global widget
   global eventAdmin
   widget.stopArchiving()
   eventAdmin.disconnect()

#------------------------------------------------------------------------------
#--Main 
#------------------------------------------------------------------------------
if __name__ == '__main__':
   usage = "acseventbrowser [options]"
   parser = OptionParser(usage)
   parser.add_option("-f", "--file", dest="filename", help="save events to FILE", metavar="FILE")
   #parser.add_option("-n", "--noGUI", action="store_true", dest="hideGUI", help="don't show (i.e. hide) the GUI")
   (options, args) = parser.parse_args()

   #use PySimpleClient to get  a default ACSEventAdmin component
   eventAdmin = ACSEventAdmin()
    
   #main widget
   root = Tkinter.Tk()
   Pmw.initialise(root)
   root.title("ACS Event Adminstrator")

   #make sure everything can shutdown properly
   exitButton = Tkinter.Button(root, text = 'Exit', command = root.destroy)
   exitButton.pack(side = 'bottom')
   widget = ACSEventAdminGUI(root, eventAdmin, options.filename)

   # install the signal handler(s)
   signal.signal(signal.SIGTERM, signalHandler)
   signal.signal(signal.SIGABRT, signalHandler)
   signal.signal(signal.SIGQUIT, signalHandler)
   signal.signal(signal.SIGHUP, signalHandler)

   #run the widget until the end-user clicks the Exit button
   root.mainloop()

   widget.stopArchiving()
   eventAdmin.disconnect()
