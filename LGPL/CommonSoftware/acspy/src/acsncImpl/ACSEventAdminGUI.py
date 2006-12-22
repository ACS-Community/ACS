# @(#) $Id: ACSEventAdminGUI.py,v 1.17 2006/12/22 23:34:43 sharring Exp $
#
# Copyright (C) 2001
# Associated Universities, Inc. Washington DC, USA.
#
# Produced for the ALMA project
#
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Library General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option) any
# later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
# details.
#
# You should have received a copy of the GNU Library General Public License
# along with this library; if not, write to the Free Software Foundation, Inc.,
# 675 Massachusetts Ave, Cambridge, MA 02139, USA.  Correspondence concerning
# ALMA should be addressed as follows:
#
# Internet email: alma-sw-admin@nrao.edu
# "@(#) $Id: ACSEventAdminGUI.py,v 1.17 2006/12/22 23:34:43 sharring Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/08/20  Created.
#------------------------------------------------------------------------------
'''
This module contains the implementation of a GUI which attaches to the
IDL ACSEventAdmin interface. Also it is the author\'s first attempt at a GUI
without a GUI builder so please don\'t bash it too hard;)

TODO LIST:
- Implementation
- Documentation
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from time       import sleep
from thread     import start_new_thread
from time       import gmtime, asctime
from os         import path
import string
import Tkinter
import Pmw
import pickle
from tkFileDialog import asksaveasfilename
from tkFileDialog import askopenfile
from traceback    import print_exc
#--CORBA STUBS-----------------------------------------------------------------
from ACS        import CBDescIn
import acstime
#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Callbacks            import CBstring
from Acspy.Common.TimeHelper           import TimeUtil
from Acspy.Clients.SimpleClient        import PySimpleClient
from Acspy.Util.Visualizer             import getTextRepresentation
#--GLOBALS---------------------------------------------------------------------
EVENT_CACHE = {}
#------------------------------------------------------------------------------
class ChannelInfo(Pmw.MegaToplevel):
    '''
    Simple widget class. Just prints out a predetermined message (generally listing
    some information about a notification channel. Class was cut-and-pasted from a
    Pmw example with very minor modifications.
    '''
    def __init__(self, parent=None, **kw):
        '''
        Standard Constructor.

        Parameters:
        - parent is the parent widget
        - kw are keyword arguments

        Returns: Nothing

        Raises: ???
        '''
	# Define the megawidget options.
	optiondefs = ()
	self.defineoptions(kw, optiondefs)

	# Initialise the base class (after defining the options).
	Pmw.MegaToplevel.__init__(self, parent)
        
	# Create the components.
	interior = self.interior()

        # Exit button
	self._dismiss = self.createcomponent('dismiss',
                                             (),
                                             None,
                                             Tkinter.Button,
                                             (interior),
                                             text = 'Dismiss',
                                             command = self.goodbye)
	self._dismiss.pack(side = 'bottom',
                           pady = 4)

        # Frame which separates an icon from the message
	self._separator = self.createcomponent('separator',
                                               (),
                                               None,
                                               Tkinter.Frame,
                                               (interior),
                                               height = 2,
                                               borderwidth = 1,
                                               relief = 'sunken')
	self._separator.pack(side = 'bottom',
                             fill = 'x',
                             pady = 4)

        # Icon
	self._icon = self.createcomponent('icon',
                                          (),
                                          None,
                                          Tkinter.Label,
                                          (interior))
	self._icon.pack(side = 'left',
                        padx = 8,
                        pady = 8)

        # Frame to hold the message
	self._infoFrame = self.createcomponent('infoframe',
                                               (),
                                               None,
                                               Tkinter.Frame,
                                               (interior))
	self._infoFrame.pack(side = 'left',
                             fill = 'both',
                             expand = 1,
                             padx = 4,
                             pady = 4)

        # Actual message
	self._message = self.createcomponent('message',
                                             (),
                                             None,
                                             Tkinter.Label,
                                             (interior))
	self._message.pack(expand = 1,
                           fill = 'both',
                           padx = 10,
                           pady = 10)

        #I have no idea on this one...
	self.bind('<Return>', self.goodbye)
        
	# Check keywords and initialise options.
	self.initialiseoptions()
        return
    #------------------------------------------------------------------------------   
    def goodbye(self, event = None):
        '''
        Invoked by user to destroy the panel.

        Parameters:
        - event???

        Returns: Nothing

        Raises: ???
        '''
	self.destroy()

#----------------------------------------------------------------------------------
class ACSEventAdminGUI(CBstring):
    '''
    ACSEventAdminGUI, derived from the IDL interface CBstring, is the primary GUI panel
    used with the ACSEventAdmin IDL interface.
    '''
    def __init__(self, parent, eventAdminRef, filename):
        '''
        Standard Constructor

        Paramters:
        - parent is the parent widget of this class
        - eventAdminRef is a reference to an ACSEventAdmin IDL interface

        Returns: Nothing

        Raises: ???
        '''

        self.archiveEventsFile = None 

        if filename != None:
           self.archiveEventsFile = file(filename, "w")

        #call superclass constructor
        CBstring.__init__(self)
        
        #time helper instance. used to format timestamps.
        self.timehelper = TimeUtil()
        
        #save the component reference
        self.eventAdminRef = eventAdminRef
        
        #activate ourself as a corba object
        self.corbaRef = self._this()

        #the fields that will be outputted for the Event browser
        self.ebFields = ["Timestamp", "Channel", "Source", "SupplierEvent#", "ChannelEvent#", "Type", "TypeEvent#"]
        #Layout of the columns
        self.ebFormat = '%25s %12s %12s %16s %15s %20s %15s'

        #Copy the parent widget
        self.parent = parent

        #place where we map events within the GUI to the real events
        self.outputEventMap = {}

        # Create and pack the MenuBar.
	menuBar = Pmw.MenuBar(parent,
                              hull_relief = 'raised',
                              hull_borderwidth = 1)
	menuBar.pack(fill = 'x')
	self.menuBar = menuBar
        
	menuBar.addmenu('Browser Options', 'Save, Clear, or Exit')
	menuBar.addmenuitem('Browser Options',
                            'command',
                            'Save events to a file',
                            command = self.saveEvents,
                            label = 'Save Events')
        menuBar.addmenuitem('Browser Options',
                            'command',
                            'Clear list of events from this browser',
                            command = self.clearEvents,
                            label = 'Clear Events')
        #--This does not work unless the modules for the events have been imported
        #--unfortunately. Commented out for now. DWF
        #menuBar.addmenuitem('Browser Options',
        #                    'command',
        #                    'Saves the events so they can be reloaded later.',
        #                    command = self.pickleEvents,
        #                    label = 'Pickle Events')
        #menuBar.addmenuitem('Browser Options',
        #                    'command',
        #                    'Loads pickled events into the browser.',
        #                    command = self.loadPickledEvents,
        #                    label = 'Load Pickled Events')
        
        #create the event browser group
        browserGroup = Pmw.Group(parent, tag_text='Event Browser')
	browserGroup.pack(fill='x', expand='1', side = 'top', padx = 5, pady = 3)
        
        headerLine = self.ebFormat % tuple(self.ebFields)
        
        #create the channel group
        channelGroup = Pmw.Group(parent, tag_text='Channels')
	channelGroup.pack(side = 'left', padx = 5, pady = 3)

        
        #create the admin group
        eventViewerGroup = Pmw.Group(parent, tag_text='Event Viewer')
	eventViewerGroup.pack(side = 'left', padx = 5, pady = 3)

        #create the admin group
        adminGroup = Pmw.Group(parent, tag_text='Channel Administration')
	adminGroup.pack(fill='both', expand='1', side = 'left', padx = 5, pady = 3)

        #-----------------------------------------------------------
        #--Event viewer box
        self.evST= Pmw.ScrolledText(eventViewerGroup.interior(),
                                    borderframe = 0,
                                    columnheader = 0,
                                    usehullsize = 1,
                                    hull_width = 300,
                                    hull_height = 300,
                                    text_wrap='none',
                                    text_font = Pmw.logicalfont('Fixed'),
                                    text_padx = 4,
                                    text_pady = 4)
        #make the scrolling text window big
	self.evST.pack(padx = 5, pady = 5, fill = 'both', expand = 1)
        
        # Prevent users' modifying text and headers
        #self.evST.configure(text_state = 'disabled')
        
        ############################################################
        ##Event Browser#############################################
        # Create the ScrolledText with headers.
        
        self.ebST = Pmw.ScrolledListBox(browserGroup.interior(),
                                        items=(),
                                        labelpos='nw',
                                        label_text = "   " + headerLine,
                                        usehullsize = 1,
                                        hull_width = 1100,
                                        hull_height = 300,
                                        selectioncommand=self.displayEvent
                                        )

        #make the scrolling text window big
	self.ebST.pack(padx = 5, pady = 5, fill = 'both', expand = 1)
        
        ############################################################
        #Active Channels############################################
	# Create and pack the simple ComboBox.
        self.channelNames = tuple(self.eventAdminRef.getActiveChannels())
	self.acScrolledLB = Pmw.ScrolledListBox(channelGroup.interior(),
                                                selectioncommand = self.displayActiveChannel,
                                                hscrollmode = 'dynamic',
                                                vscrollmode = 'dynamic',
                                                items = self.channelNames)
        #make it big
	self.acScrolledLB.pack(side = 'left',
                               fill = 'both',
                               expand = 0,
                               padx = 8,
                               pady = 8)

        ############################################################
        ##Admin Panel
        self.adminChannelName = Pmw.EntryField(adminGroup.interior(),
                                               labelpos = 'w',
                                               label_text = 'Channel name:',
                                               validate = None)
        self.adminChannelName.pack(expand=0,
                                   padx=10,
                                   pady=5,
                                   side = 'top')

        self.adminChannelMethod = Pmw.OptionMenu(adminGroup.interior(),
                                                 labelpos = 'w',
                                                 label_text = 'Choose administrative option:',
                                                 items = ('Create', 'Destroy'), #items = ('Create', 'Destroy', 'Reconfigure'),
                                                 menubutton_width = 10)
        self.adminChannelMethod.pack(anchor = 'w', padx = 10, pady = 10)
        
        
        createChannelSubmit = Pmw.ButtonBox(adminGroup.interior())
	createChannelSubmit.pack(side = 'bottom')
	createChannelSubmit.add('Submit', text = 'Submit', command = self.submit)
        ############################################################
        
        #finally we can begin receiving CORBA callbacks.
        self.eventAdminRef.monitorEvents(self.corbaRef, CBDescIn(0L, 0L, 0L))
        
        #constantly look for new channels
        self.running = 1
        start_new_thread(self.pollActiveChannels, ())
        return
    #------------------------------------------------------------------------------
    def displayEvent(self):
        '''
        Displays a single ICD-style event on the screen.
        '''
        #get the key
        t_key = self.ebST.getcurselection()[0]

        #get a textual representation
        t_list = getTextRepresentation(self.outputEventMap[t_key])

        #clear the box
        self.evST.clear()
        #write it to the box
        for line in t_list:
            self.evST.insert('end', line + '\n')
        
        return
    
    #------------------------------------------------------------------------------
    def displayActiveChannel(self):
        '''
        When someone clicks on a channel, a new widget is spawned displaying various
        pieces of information

        Paramters: None

        Returns: Nothing

        Raises: ???
        '''
        #just depend on a helper class to do the real work
        self.displayChannelHelper(list(self.acScrolledLB.getcurselection()))
    #------------------------------------------------------------------------------
    def displayChannelHelper(self, channelList):
        '''
        Responsibile for spawning a new widget displaying channel info. This is a
        recursive method so end-users can highlight multiple channels at the same time.
        Really this can never be the case given the current implementation though.

        Parameters:
        - channelList is a list of channel names

        Returns: Nothing

        Raises: ???
        '''
        #if this is the last call in the recursive chain...return control
        if len(channelList) == 0:
            return
        #pop a channel name; create a new widget; and make a recursive call
        else:
            #last channelName from the list
            channelName = channelList.pop()
            
            #get the channel info
            data = self.eventAdminRef.getChannelInfo(channelName)
            
            #create a new widget
            message  = "Channel name: " + str(channelName) + '\n'
            message += "Number of suppliers: " + str(data[0]) + '\n'
            message += "Number of consumers: " + str(data[1]) + '\n'
            message += "Total events recieved: " + str(data[2]) + '\n'
            message += "EventTypeInfo:\n"
            for eventType in data[5]:
                message += "     " + str(eventType) + '\n'
            ChannelInfo(self.parent, message_text = message, icon_bitmap = 'info')
            
            #recursive call
            self.displayChannelHelper(channelList)
            return        
    #------------------------------------------------------------------------------
    def working (self, value, completion, desc):
        '''
        Overriden method from the CBstring class which is invoked each time an event
        is received. The event is outputted to a scrolled textbox widget.

        Parameters:
        - value is the stringified event
        - completion is a CORBA completion structure
        - desc is callback struct description

        Returns: Nothing

        Raises: ???
        '''
        global EVENT_CACHE
        
        #to make pychecker happy
        del completion

        #to make pychecker happy
        del desc

        #backup
        orig_val = value
        
        # Create a new data row
        value = string.split(value)

        #convert the time to something human-readable
        epoch = acstime.Duration(long(value[0]))  #convert to an ACS epoch
        epoch = self.timehelper.epoch2py(epoch)  #convert to Python time
        epoch = gmtime(epoch)  #convert to gm time
        epoch = asctime(epoch)  #convert to nice string format
        
        dataLine = self.ebFormat % (epoch, value[1], value[2], value[3], value[4], value[5], value[6])  #tuple(value)

        #output the line to the GUI
        self.ebST.insert('end', dataLine)

        #if we are logging events to a file, save the event
        if self.archiveEventsFile != None:
           self.archiveEventsFile.write(dataLine)
           self.archiveEventsFile.write("\n")
           self.archiveEventsFile.flush()
    
        #save the original output for use with the cache
        try:
            self.outputEventMap[dataLine] = EVENT_CACHE[orig_val]
        except:
            self.outputEventMap[dataLine] = None
        
        return
    #------------------------------------------------------------------------------
    def pollActiveChannels(self):
        '''
        Gets an updated list of all notification channels and outputs them to the GUI.
        Designed to be invoked from a thread.

        Parameters: None

        Returns: Nothing

        Raises: ???
        '''
        while self.running:
            try:
                #list of all the channels the administrator knows about
                self.channelNames = tuple(self.eventAdminRef.getActiveChannels())
                
                #update the GUI
                self.acScrolledLB.setlist(self.channelNames)
            except:
                pass
            
            #sleep for awhile
            sleep(2)
    #------------------------------------------------------------------------------
    def saveEvents(self):
        '''
        Prompts user for a filename, then saves the entire list of all known events to the given text file.

        Parameters: None

        Returns: Nothing

        Raises: ???
        '''
        #save the events to the text file specified in the GUI
        filename = asksaveasfilename()
        if filename!=():
            # open the file
            temp_file = file(filename, "w")
            # write the events to the file, adding a new line after each event
            temp_file.writelines('\n'.join(self.ebST.get()))
            # close the file
            temp_file.close()
    
    #------------------------------------------------------------------------------
    def clearEvents(self):
        '''
        Saves the entire list of all known events to a given text file.

        Parameters: None

        Returns: Nothing

        Raises: ???
        '''
        global EVENT_CACHE
        
        #save the events to the text file specified in the GUI
        print dir(self.ebST)
        self.ebST.clear()
        self.evST.clear()
        #clear the caches
        EVENT_CACHE.clear()
        self.outputEventMap.clear()
    #------------------------------------------------------------------------------
    def pickleEvents(self):
        '''
        Pickles the list of all known events to a given file.

        Parameters: None

        Returns: Nothing

        Raises: ???
        '''
        #save the events to the text file specified in the GUI
        filename = asksaveasfilename()
        if filename!=():
            #open it
            temp_file = open(filename, "w")
            #dump the cache into the file
            pickle.dump(self.outputEventMap, temp_file)
            #close it
            temp_file.close()

    #------------------------------------------------------------------------------
    def loadPickledEvents(self):
        '''
        Loads a pickled list of all known events from a given file.

        Parameters: None

        Returns: Nothing

        Raises: ???
        '''
        #get the file containing the pickled cache from the user
        temp_file = askopenfile('r')
        #create a temporary dictionary with the cache
        t_cache = pickle.load(temp_file)
        #cleanup
        temp_file.close()
        
        #wipe out all of the old events
        self.clearEvents()

        #fill the cache again
        self.outputEventMap.update(t_cache)
        
        #populate the GUI with the events
        for key in self.outputEventMap.keys():
            self.ebST.insert('end', key)

        return
        
    #------------------------------------------------------------------------------
    def stopArchiving(self):
        '''
        Can be called to stop the archiving to a file.
        '''
        if self.archiveEventsFile != None:
           self.archiveEventsFile.close()
           self.archiveEventsFile = None

    #------------------------------------------------------------------------------
    def submit(self):
        '''
        Called each time the user tries to invoke some administrative function.
        DWF - needs a lot of work still.
        '''
        method = str(self.adminChannelMethod.getcurselection())
        parameter = str(self.adminChannelName.getvalue())

        #('Create', 'Destroy', 'Reconfigure')
        if method == 'Create':
            self.eventAdminRef.createChannel(parameter, [], [])
        elif method == 'Destroy':
            self.eventAdminRef.destroyChannel(parameter)
        elif method == 'Reconfigure':
            self.eventAdminRef.configChannelProperties(parameter, [], [])
            print "Reconfigure not yet implemented"
    
######################################################################
# Create demo in root window for testing.
if __name__ == '__main__':
    print "No testing here!"
