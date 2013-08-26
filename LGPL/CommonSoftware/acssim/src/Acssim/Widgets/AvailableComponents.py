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
Includes the implementation of a widget which displays all simulated
components and their methods.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
import Pmw
#--CORBA STUBS-----------------------------------------------------------------
import CORBA
#--ACS Imports-----------------------------------------------------------------
from Acssim.Goodies         import *
from Acspy.Util.ACSCorba    import getClient
from Acspy.Util.ACSCorba    import getManager
from Acssim.Widgets.Method  import Method

#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class AvailableComponents(Pmw.Group):
    '''
    Widget which shows all simulated components and their methods/
    attributes.
    '''
    def __init__(self, parent=None):
        '''
        Constructor
        
        Parameters: parent - parent widget
        '''
        
        self.parent = parent
        
        #List of simulated components
        self.comp_names = []
        
        #Component currently being manipulated
        self.current_comp_name = ""
        
        #Current operation type selected (e.g, method or attribute)
        self.operation_type = ""
        
        #create the main components group
        Pmw.Group.__init__(self,
                           parent,
                           tag_text='Available Simulated Components')
        self.pack(fill='x',
                  expand='1',
                  side = 'top',
                  padx = 5,
                  pady = 3)
        
        #setup the panel which lists the names of all the different
        #simulated components
        self.__setupNamesPanel()
        
        #setup the panel which chooses either CORBA attributes or
        #CORBA methods to display
        self.__setupOpTypeSelection()
        
        #setup the panel which displays a components methods/attributes
        self.__setupOpSelect()
        
    #--------------------------------------------------------------------------    
    def __setupNamesPanel(self):
        '''
        Helper method sets up the panel displaying the names of all
        components that can be simulated.
        '''
        #get a list from manager of all known components
        #note that this will NOT work with dynamic components!
        comp_info_list = getManager().get_component_info(getClient().token.h, 
                                                         [], 
                                                         "*", 
                                                         "*", 
                                                         0)
        #must add active components to this list as well
        comp_info_list = comp_info_list + getManager().get_component_info(getClient().token.h, 
                                                                          [],
                                                                          "*", 
                                                                          "*", 
                                                                          1)
                                                                          
        #add all components to the list iff they are actually simulator objects
        for comp_info in comp_info_list:
            if comp_info.code == "Acssim.Servants.GUISimulator" and self.comp_names.count(comp_info.name)==0:
                self.comp_names.append(comp_info.name)
        #tidy up the list
        self.comp_names.sort()
        
        #now add the widget
        self.comp_names_panel = Pmw.OptionMenu(self.interior(),
                                             command = self.compNameUpdate,
                                             labelpos = 'w',
                                             label_text = 'Component Name:',
                                             items = self.comp_names,
                                             menubutton_width = 20)
        self.comp_names_panel.pack(anchor = 'w',
                                 padx = 10,
                                 pady = 10)
                                 
    #--------------------------------------------------------------------------
    def __setupOpTypeSelection(self):
        '''
        Helper method creates a radio box to let the developer select
        whether they want to deal with CORBA attributes or CORBA methods
        of components.
        '''
        #radio box for selecting methods or attributes
        self.op_select_rbox = Pmw.RadioSelect(self.interior(),
                                            labelpos = 'w',
                                            command = self.methAttrCommand,
                                            label_text = 'Operation Selection',
                                            frame_borderwidth = 2,
                                            frame_relief = 'ridge')
        self.op_select_rbox.pack(fill = 'x',
                               padx = 10,
                               pady = 10)
        
        # Add the appropriate buttons to the radio select
        self.op_select_rbox.add('Methods')
        self.op_select_rbox.add('Attributes')
        
    #--------------------------------------------------------------------------
    def __setupOpSelect(self):
        '''
        Helper method creates a widget displaying a given component's
        methods and attributes
        '''
        
        #list of component methods or attributes available for the
        #given component
        self.opsSLB = Pmw.ScrolledListBox(self.interior(),
                                          selectioncommand = self.methodSelection,
                                          hscrollmode = 'dynamic',
                                          vscrollmode = 'dynamic',
                                          items = ())
        self.opsSLB.pack(side = 'left',
                             fill = 'both',
                             expand = 1,
                             padx = 8,
                             pady = 8)
                             
    #--------------------------------------------------------------------------
    def compNameUpdate(self, selection):
        '''
        When the user selects a component name, the widget containing the component
        methods is updated automatically.

        Parameters:
        - selection name of the component
        '''
        #Save the name
        self.current_comp_name = selection
        #Update methods
        self.op_select_rbox.invoke('Methods')
        
    #--------------------------------------------------------------------------
    def methAttrCommand(self, selection):
        '''
        Allows the user to select a components IDL methods or attributes.

        Parameters:
        - selection ("Methods" or "Attributes")
        '''
        #update the component first
        self.current_comp_name = self.comp_names_panel.getvalue()
        #reset the list of available methods/attributes
        self.opsSLB.setlist(self.updateMethods(selection))

        self.operation_type=selection
    #--------------------------------------------------------------------------
    def methodSelection(self):
        '''
        Spawn a new widget to dynamically configure the behavior of a method/attribute.
        '''
        Method(self.parent, self)
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
        compName = self.current_comp_name

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
