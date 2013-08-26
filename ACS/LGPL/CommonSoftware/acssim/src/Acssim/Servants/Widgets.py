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
The first time this module is imported, the IDL simulator graphical user
interface will pop up. If $DISPLAY is undefined however, the widget will never
start.

TODO LIST:
    - talk to Michele about using his GUI testing framework on this module
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from thread     import start_new_thread
from os         import environ

import Tkinter
import Pmw

#--ACS Imports-----------------------------------------------------------------
from Acssim.Widgets.Interpreter         import Interpreter
from Acssim.Widgets.GlobalOptions       import GlobalOptions
from Acssim.Widgets.AvailableComponents import AvailableComponents

#------------------------------------------------------------------------------
class MainWindow:
    '''
    Primary GUI panel for the ACS Simulator GUI. Allows end-user to set global
    options as well as configure specific code to be executed on method 
    invocations of various components.
    '''
    def __init__(self, parent):
        '''
        Standard Constructor

        Paramters:
        - parent is the parent widget of this class

        Returns: Nothing

        Raises: ???
        '''
        
        self.parent = parent
        
        #create the components group attaching it to the left side
        #of the screen
        self.components_group = Pmw.Group(self.parent)
        self.components_group.pack(fill='x',
                                   expand='1',
                                   side = 'left',
                                   padx = 5,
                                   pady = 3)
        
        #create the global options widget inside the components
        #group
        self.global_options_group = GlobalOptions(self.components_group.interior())
        
        #create the available, simulated components widget 
        self.sim_components_group = AvailableComponents(self.components_group.interior())
                          
        #create the interpreter widget
        self.interpreter_group = Interpreter(self.parent)
        
#------------------------------------------------------------------------------
#Stuff to run the GUI
#Because the GUI can be started from a cronjob, we have to double-check to 
#ensure $DISPLAY is defined!
if environ.has_key('DISPLAY'):
    
    #main widget
    root = Tkinter.Tk()
    Pmw.initialise(root)
    root.title("ACS Simulator Administrator")
    
    #make sure everything can shutdown properly
    exit_button = Tkinter.Button(root, 
                                 text = 'Exit', 
                                 command = root.destroy)
    exit_button.pack(side = 'bottom')
    
    #create the simulator widget
    main_window = MainWindow(root)
    
    #run the widget until the end-user clicks the Exit button
    start_new_thread(root.mainloop, ())
    #root.mainloop()
    