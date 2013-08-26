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
The implementation of a widget which houses a Python shell.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from code import InteractiveConsole

import Tkinter
import Pmw
#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acssim.Goodies           import *
from Acspy.Util.Console import Console
#--GLOBALS---------------------------------------------------------------------

#--------------------------------------------------------------------------
class ACSConsole(InteractiveConsole):
    '''
    '''
    def __init__(self, intr_widget):
        '''
        Constructor
        
        Parameters:
            intr_widget - interpreter widget
        '''
        self.intr_widget = intr_widget
        
        InteractiveConsole.__init__(self)
        
    def write(self, data):
        '''
        Overridden.
        '''
        #strip out all whitespace from data
        formatted_data = '\n' + data.strip()
        
        #change the text color of the text shell
        
        self.intr_widget.text_shell.insert(Tkinter.END,
                                           formatted_data,
                                           ("ERROR"))
        
#--------------------------------------------------------------------------
class OldInterpreter(Console):
    '''
    '''
    def __init__(self, parent=None):
        '''
        '''
        Console.__init__(self, 
                         parent=parent, 
                         local_dict={})
                         
        self.dict["console"] = self
        self.pack(fill=Tkinter.BOTH, 
                  expand=1)
        self.master.title("ACS Python Console")
#--------------------------------------------------------------------------
class Interpreter(Pmw.Group):
    '''
    Interpreter houses a Python interactive shell.
    '''
    def __init__(self, parent=None):
        '''
        Constructor
        
        Param: parent - the parent widget
        '''
        
        #create a console for ourselves
        self.console = ACSConsole(self)
        
        
        
        
        Pmw.Group.__init__(self,
                           parent,
                           tag_text='Simulator Interpreter')
        self.pack(fill='y',
                  expand='1',
                  side = 'right',
                  padx =5,
                  pady =3)
        
        #Create a shell to enter python commands into
        self.text_shell = Tkinter.Text(self.interior(), 
                                       insertontime=200,
                                       insertofftime=150,
                                       bg="white", 
                                       height = 40,
                                       width = 80)
        self.text_shell.pack(padx = 5, 
                             pady = 5, 
                             fill = 'both', 
                             expand = 1)
        
        self.text_shell.tag_config("ERROR", 
                                    background="white", foreground="red")
        
        #auto import some things for them
        self.console.push("from Acssim.Goodies import *")
        
        #bind enter key presses to a handler function
        self.text_shell.bind("<Return>", self.handle_return)
        
    #--------------------------------------------------------------------------
    def handle_return(self, event):
        '''
        '''
        #get the current cursor position
        cursor = self.text_shell.index("insert")
        
        #get the line number
        [line, pos] = map(int, cursor.split("."))
        
        #get the command to send to the interpreter...
        command = self.text_shell.get("%d.0" % line, "%d.end" % line)
        #...and send it.
        self.console.push(command)