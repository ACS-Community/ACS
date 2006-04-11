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
import Pmw

#--CORBA STUBS-----------------------------------------------------------------
import CORBA

#--ACS Imports-----------------------------------------------------------------
from Acssim.Goodies         import *
from Acssim.Goodies         import getSimProxy
from Acssim.Corba.Utilities import listToCodeObj
from Acssim.Goodies         import getCompLocalNSList
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class Method:
    '''
    Widget which provides end-user with the means to enter in arbitrary Python code
    to execute on given simulated method invocations.
    '''
    #------------------------------------------------------------------------------
    def __init__(self, parent, gui_ref):
        '''
        Constructor.

        Parameters:
        - parent is the parent widget
        - gui_ref is a reference to an MainWindow object
        '''
        #name of the component
        self.comp_name = gui_ref.comp_names_panel.getvalue()

        #method or attribute
        self.operation_type = gui_ref.operation_type
        
        #name of the method
        self.comp_method = gui_ref.opsSLB.getvalue()[0]
        
        #Create the top level widget which is completely separate from
        #the parent (e.g., MainWindow) widget
        megaTopLevel = Pmw.MegaToplevel(parent, 
                                        title = self.comp_name + ": " + 
                                        self.comp_method)
        self.top_level = megaTopLevel.interior()
        
        #Scrolled test window for end-user to enter arbitrary Python
        #code to be executed.
        self.st = Pmw.ScrolledText(self.top_level,
                                   borderframe = 1,
                                   labelpos = 'n',
                                   label_text=self.comp_method + 
                                   " Implementation",
                                   usehullsize = 1,
                                   hull_width = 400,
                                   hull_height = 300,
                                   text_padx = 10,
                                   text_pady = 10,
                                   text_wrap='none')
        self.st.pack(padx = 5, pady = 5, fill = 'both', expand = 1)
        
        #Sleep counter states how long the simulator should wait before
        #returning control on the method invocation.
        self.sleepCounter = Pmw.Counter(self.top_level,
                                        labelpos = 'w',
                                        label_text = 'Sleep Time:',
                                        orient = 'horizontal',
                                        entry_width = 3,
                                        entryfield_value = getStandardTimeout(),
                                        entryfield_validate = {'validator' : 'real', 
                                                               'min' : 0.0, 
                                                               'max' : 10000.0}
                                       )
        self.sleepCounter.pack(padx=10, pady=5)
        
        #Button box to submit, cancel, etc operations
        self.buttonBox = Pmw.ButtonBox(self.top_level,
                                       labelpos = 'nw',
                                       label_text = 'Choose:',
                                       frame_borderwidth = 2,
                                       frame_relief = 'groove')
    
        self.buttonBox.pack(fill = 'both', expand = 1, padx = 10, pady = 10)
        #Add the buttons
        self.buttonBox.add('Apply', command = self.submit)
        self.buttonBox.add('Clear', command = self.clear)
        self.buttonBox.add('Exit', command = self.top_level.destroy)
        #Set the default button (the one executed when <Return> is hit).
        self.buttonBox.setdefault('Apply')
        self.top_level.bind('<Return>', self.submit)
        self.top_level.focus_set()
        # Make all the buttons the same width.
        self.buttonBox.alignbuttons()

    # Create the Balloon for this toplevel.
    #self.balloon = Pmw.Balloon(self.top_level)
    #------------------------------------------------------------------------------
    def submit(self, event=None):
        '''
        Method invoked to alter the behavior of some simulated CORBA method/attribute

        Paremeters:
        - event This is ignored!
        '''
        
        method_name = self.comp_method
        
        if self.operation_type=="Attributes":
            method_name = "_get_" + method_name
        
        #create the temporary dictionary
        temp_dict = { 'Value': None,
                      'Timeout': float(self.sleepCounter.getvalue())}
        
        #get the code to be executed yielding a return value
        temp_dict['Value'] = self.st.getvalue().strip().split('\n')
        temp_dict['Value'] = getCompLocalNSList(self.comp_name) + temp_dict['Value']
        temp_dict['Value'] = listToCodeObj(temp_dict['Value'], 
                                           {})
        
        
        
        #store it globally
        getSimProxy(self.comp_name).gui_handler.setMethod(method_name, 
                                                          temp_dict)
        
    #------------------------------------------------------------------------------
    def clear(self):
        '''
        Clears all fields of the widget.
        '''
        #reset the timeout field
        self.sleepCounter.setvalue(getStandardTimeout())
        
        #clear the scroll text box for code entry
        self.st.clear()