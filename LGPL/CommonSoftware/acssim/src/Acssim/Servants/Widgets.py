# @(#) $Id$
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
# "@(#) $Id$"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''

TODO LIST:

'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import CORBA
#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log       import getLogger

from Acssim.Servants.Goodies           import *
from Acssim.Servants.Representations.BehaviorProxy import BehaviorProxy
from Acssim.Servants.Goodies import getSimProxy

from thread     import start_new_thread
from os         import environ

import Tkinter
import Pmw

from Acspy.Util.ACSCorba import getClient
from Acspy.Util.ACSCorba import getManager

#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#--Everything from here on out just consists of GUI code-----------------------
#--In the future perhaps this can be moved to another module, but for now it---
#--is more convenient to keep it here because it directly uses global objects--
#--from this module.-----------------------------------------------------------


#------------------------------------------------------------------------------
class MethodInfo:
    '''
    Widget which provides end-user with the means to enter in arbitrary Python code
    to execute on given simulated method invocations.
    '''
    #------------------------------------------------------------------------------
    def __init__(self, parent, guiRef):
        '''
        Constructor.

        Parameters:
        - parent is the parent widget
        - guiRef is a reference to an MainWindow object
        '''
        #name of the component
        self.compName = guiRef.compNamesPanel.getvalue()

        #method or attribute
        self.opType = guiRef.opType
        
        #name of the method
        self.compMethod = guiRef.methodsSLB.getvalue()[0]
        ############################################################
        #Create the top level widget which is completely separate from
        #the parent (e.g., MainWindow) widget
        megaTopLevel = Pmw.MegaToplevel(parent, title = self.compName + ": " + self.compMethod)
	self.tl = megaTopLevel.interior()
        ############################################################
        #Scrolled test window for end-user to enter arbitrary Python
        #code to be executed.
        self.st = Pmw.ScrolledText(self.tl,
                                   borderframe = 1,
                                   labelpos = 'n',
                                   label_text=self.compMethod + " Implementation",
                                   usehullsize = 1,
                                   hull_width = 400,
                                   hull_height = 300,
                                   text_padx = 10,
                                   text_pady = 10,
                                   text_wrap='none')
        self.st.pack(padx = 5, pady = 5, fill = 'both', expand = 1)
        ############################################################
        #Sleep counter states how long the simulator should wait before
        #returning control on the method invocation.
        self.sleepCounter = Pmw.Counter(self.tl,
                                        labelpos = 'w',
                                        label_text = 'Sleep Time:',
                                        orient = 'horizontal',
                                        entry_width = 3,
                                        entryfield_value = getStandardTimeout(),
                                        entryfield_validate = {'validator' : 'real', 'min' : 0.0, 'max' : 10000.0})
        self.sleepCounter.pack(padx=10, pady=5)
        ############################################################
        #Button box to submit, cancel, etc operations
        self.buttonBox = Pmw.ButtonBox(self.tl,
                                       labelpos = 'nw',
                                       label_text = 'Choose:',
                                       frame_borderwidth = 2,
                                       frame_relief = 'groove')
	self.buttonBox.pack(fill = 'both', expand = 1, padx = 10, pady = 10)
	#Add the buttons
	self.buttonBox.add('Apply', command = self.submit)
        self.buttonBox.add('Clear', command = self.clear)
        self.buttonBox.add('Exit', command = self.tl.destroy)
	#Set the default button (the one executed when <Return> is hit).
	self.buttonBox.setdefault('Apply')
	self.tl.bind('<Return>', self.submit)
	self.tl.focus_set()
	# Make all the buttons the same width.
	self.buttonBox.alignbuttons()

	# Create the Balloon for this toplevel.
	#self.balloon = Pmw.Balloon(self.tl)
    #------------------------------------------------------------------------------
    def submit(self, event=None):
        '''
        Method invoked to alter the behavior of some simulated CORBA method/attribute

        Paremeters:
        - event This is ignored!
        '''
        methName = self.compMethod
        if self.opType=="Attributes":
            methName = "_get_" + methName
        
        #create the temporary dictionary
        temp_dict = { 'Value': self.st.getvalue().strip().split('\n'),
                      'Timeout': float(self.sleepCounter.getvalue())}
    
        #store it globally
        getSimProxy(self.compName).gui_handler.setMethod(methName, tDict)
        
    #------------------------------------------------------------------------------
    def clear(self):
        '''
        Clears all fields of the widget.
        '''
        #reset the timeout field
        self.sleepCounter.setvalue(getStandardTimeout())
        #clear the scroll text box for code entry
        self.st.clear()
#----------------------------------------------------------------------------------
class MainWindow:
    '''
    Primary GUI panel for the ACS Simulator GUI. Allows end-user to set global
    options as well as configure specific code to be executed on method invocations
    of various components.
    '''
    def __init__(self, parent):
        '''
        Standard Constructor

        Paramters:
        - parent is the parent widget of this class

        Returns: Nothing

        Raises: ???
        '''
        #Copy a reference to the parent widget
        self.parent = parent

        #List of simulated components
        self.compNames = []

        #Component currently being manipulated
        self.currComp = ""

        #Current operation type selected (e.g, method or attribute)
        self.opType = ""
        ############################################################
        #create the global options group
        optionsGroup = Pmw.Group(self.parent,
                                 tag_text='Global Options')
	optionsGroup.pack(fill='x',
                          expand='1',
                          side = 'top',
                          padx = 5,
                          pady = 3)
        ############################################################
        #panel for setting characters
        def stringToList():
            '''
            Generic function converts a string to a list of characters.
            Used only with globalCharsPanel.
            '''
            retList = []
            for charact in self.globalCharsPanel.getvalue():
                retList.append(charact)
            setCHARS(retList)
            return
            
        self.globalCharsPanel = Pmw.EntryField(optionsGroup.interior(),
                                               labelpos = 'w',
                                               label_text = "Pool of 'random' characters:",
                                               validate = None,
                                               command = stringToList)
        self.globalCharsPanel.pack(fill='x', expand=1, padx=10, pady=5)
        ############################################################
        #panel for setting global timeout
        def setNewTimeout():
            '''
            Generic function sets a new global timeout value.
            Used only with globalTimeoutPanel.
            '''
            setStandardTimeout(self.globalTimeoutPanel.getvalue())
            return
        
        self.globalTimeoutPanel = Pmw.EntryField(optionsGroup.interior(),
                                                 labelpos = 'w',
                                                 value = getStandardTimeout(),
                                                 label_text = "Timeout:",
                                                 validate = {'validator' : 'real','min' : 0.0, 'max' : 10000, 'minstrict' : 0},
                                                 command = setNewTimeout)
        self.globalTimeoutPanel.pack(fill='x',
                                     expand=1,
                                     padx=10,
                                     pady=5)
        ############################################################
        #panel for setting global max sequence size
        def setNewMaxSeqSize():
            '''
            Generic function sets a new global maximum sequence size.
            Used only with globalMaxSeqSizePanel.
            '''
            setMaxSeqSize(self.globalMaxSeqSizePanel.getvalue())
            return

        self.globalMaxSeqSizePanel = Pmw.EntryField(optionsGroup.interior(),
                                                    labelpos = 'w',
                                                    value = getMaxSeqSize(),
                                                    label_text = "Maximum Sequence Size:",
                                                    validate = {'validator' : 'integer','min' : 0, 'max' : 100000000L, 'minstrict' : 0},
                                                    command = setNewMaxSeqSize)
        self.globalMaxSeqSizePanel.pack(fill='x', expand=1, padx=10, pady=5)
        ############################################################
        #create the main components group
        compGroup = Pmw.Group(self.parent,
                              tag_text='Available Simulated Components')
	compGroup.pack(fill='x',
                       expand='1',
                       side = 'top',
                       padx = 5,
                       pady = 3)
        ############################################################
        #create an options panel for the simulated component's name
        #get a list from manager of all known components
        #note that this will NOT work with dynamic components!
        compInfoList = getManager().get_component_info(getClient().token.h, [], "*", "*", 0)
        compInfoList = compInfoList + getManager().get_component_info(getClient().token.h, [], "*", "*", 1)
        #add all components to the list iff they are actually simulator objects
        for compInfo in compInfoList:
            if compInfo.code == "Acssim.Servants.Simulator" and self.compNames.count(compInfo.name)==0:
                self.compNames.append(compInfo.name)

        #sort the list in place alphabetically
        self.compNames.sort()
        self.compNamesPanel = Pmw.OptionMenu(compGroup.interior(),
                                             command = self.compNameUpdate,
                                             labelpos = 'w',
                                             label_text = 'Component Name:',
                                             items = self.compNames,
                                             menubutton_width = 20)
        self.compNamesPanel.pack(anchor = 'w',
                                 padx = 10,
                                 pady = 10)
        ############################################################
        #radio box for selecting methods or attributes
        self.methAttrRbox = Pmw.RadioSelect(compGroup.interior(),
                                            labelpos = 'w',
                                            command = self.methAttrCommand,
                                            label_text = 'Operation Selection',
                                            frame_borderwidth = 2,
                                            frame_relief = 'ridge')
	self.methAttrRbox.pack(fill = 'x',
                               padx = 10,
                               pady = 10)
        
	# Add the appropriate buttons to the radio select
        self.methAttrRbox.add('Methods')
        self.methAttrRbox.add('Attributes')
        ############################################################
        #list of component methods or attributes available for the
        #given component
        self.methodsSLB = Pmw.ScrolledListBox(compGroup.interior(),
                                              selectioncommand = self.methodSelection,
                                              hscrollmode = 'dynamic',
                                              vscrollmode = 'dynamic',
                                              items = ())
        #make it big
	self.methodsSLB.pack(side = 'left',
                             fill = 'both',
                             expand = 1,
                             padx = 8,
                             pady = 8)
        ############################################################
        #final setup stuff
    #--------------------------------------------------------------------------
    def compNameUpdate(self, selection):
        '''
        When the user selects a component name, the widget containing the component
        methods is updated automatically.

        Parameters:
        - selection name of the component
        '''
        #Save the name
        self.currComp = selection
        #Update methods
        self.methAttrRbox.invoke('Methods')
    #--------------------------------------------------------------------------
    def methAttrCommand(self, selection):
        '''
        Allows the user to select a components IDL methods or attributes.

        Parameters:
        - selection ("Methods" or "Attributes")
        '''
        #update the component first
        self.currComp = self.compNamesPanel.getvalue()
        #reset the list of available methods/attributes
        self.methodsSLB.setlist(self.updateMethods(selection))

        self.opType=selection
    #--------------------------------------------------------------------------
    def methodSelection(self):
        '''
        Spawn a new widget to dynamically configure the behavior of a method/attribute.
        '''
        MethodInfo(self.parent, self)
    #--------------------------------------------------------------------------
    def updateMethods(self, selection):
        '''
        Helper function returns a list of methods or attributes the component has available.
        
        Parameters:
        - selection ("Methods" or "Attributes")

        Returns: a list of available methods/attributes.
        '''
        retList = []
        
        #component name
        compName = self.currComp

        #retrieve comp IDL type from manager
        compInfoList = []
        compInfoList = getManager().get_component_info(getClient().token.h, [], compName, "*", 0)
        compInfoList = compInfoList + getManager().get_component_info(getClient().token.h, [], compName, "*", 1)
        
        if len(compInfoList) > 1:
            print "Potential error: more than 1 matching component found - ", compName, len(compInfoList)
        elif len(compInfoList)==0:
            print "Bad...no matching component found:", compName
            return retList

        #need to know the IDL type in order to figure out the list of components
        compIDLType = compInfoList[0].type
        
        #Get a description from the IFR
        interf = IR.lookup_id(compIDLType)._narrow(CORBA.InterfaceDef).describe_interface()

        if selection == "Methods":
            for method in interf.operations:
                retList.append(method.name)
        else:
            #must be an attribute
            for method in interf.attributes:
                retList.append(method.name)

        retList.sort()
        return retList
#------------------------------------------------------------------------------
#Stuff to run the GUI
#Because the GUI can be started from a cronjob, we have to double-check to ensure
#$DISPLAY is defined!
if environ.has_key('DISPLAY'):
    #main widget
    root = Tkinter.Tk()
    Pmw.initialise(root)
    root.title("ACS Simulator Administrator")
    
    #make sure everything can shutdown properly
    exitButton = Tkinter.Button(root, text = 'Exit', command = root.destroy)
    exitButton.pack(side = 'bottom')
    widget = MainWindow(root)
    
    #run the widget until the end-user clicks the Exit button
    start_new_thread(root.mainloop, ())
