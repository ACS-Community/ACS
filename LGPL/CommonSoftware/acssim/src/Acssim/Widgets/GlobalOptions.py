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

#--ACS Imports-----------------------------------------------------------------
from Acssim.Goodies           import *

#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class GlobalOptions(Pmw.Group):
    '''
    Helper function sets up widget to display globally settable options.
    '''
    def __init__(self, parent=None):
        '''
        '''
        Pmw.Group.__init__(self, parent, tag_text='Global Options')
        
        self.pack(fill='x',
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
            
        self.globalCharsPanel = Pmw.EntryField(self.interior(),
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
        
        self.globalTimeoutPanel = Pmw.EntryField(self.interior(),
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

        self.globalMaxSeqSizePanel = Pmw.EntryField(self.interior(),
                                                    labelpos = 'w',
                                                    value = getMaxSeqSize(),
                                                    label_text = "Maximum Sequence Size:",
                                                    validate = {'validator' : 'integer','min' : 0, 'max' : 100000000L, 'minstrict' : 0},
                                                    command = setNewMaxSeqSize)
        self.globalMaxSeqSizePanel.pack(fill='x', expand=1, padx=10, pady=5) 
                                     