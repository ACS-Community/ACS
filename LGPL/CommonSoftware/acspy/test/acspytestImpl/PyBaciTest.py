#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002 
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307  USA
#
#------------------------------------------------------------------------------
'''
TODO:
- All!!!
'''
__version__ = "$Id: PyBaciTest.py,v 1.5 2007/10/04 21:56:15 agrimstrup Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
import acspytest__POA
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.CharacteristicComponent import CharacteristicComponent
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Util.BaciHelper             import addProperty
from ACSImpl.DevIO                     import DevIO
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------

class timestampDevIO(DevIO):
    '''
    DevIO that returns a timestamp with its data.
    '''
    def __init__(self):
        DevIO.__init__(self, 0)
        return
    def read(self):
        return tuple((0,1191516502))

class PyBaciTest(CharacteristicComponent,
                 acspytest__POA.PyBaciTest,
                 ContainerServices,
                 ComponentLifecycle):
    #------------------------------------------------------------------------------
    '''
    '''
    #------------------------------------------------------------------------------
    def __init__(self):
        '''
        Just call superclass constructors here.
        '''
        CharacteristicComponent.__init__(self)
        ContainerServices.__init__(self)
        self.timestampIO = timestampDevIO()
        return
    #------------------------------------------------------------------------------  
    def initialize(self):
        '''
        '''
        addProperty(self, "stringROProp")
        addProperty(self, "stringRWProp")

        addProperty(self, "doubleROProp")
        addProperty(self, "doubleRWProp")

        addProperty(self, "longROProp")
        addProperty(self, "longRWProp")

        addProperty(self, "longLongROProp")
        addProperty(self, "longLongRWProp")

        addProperty(self, "uLongLongROProp")
        addProperty(self, "uLongLongRWProp")

        addProperty(self, "patternROProp")
        addProperty(self, "patternRWProp")

        addProperty(self, "doubleSeqROProp")
        addProperty(self, "doubleSeqRWProp")

        addProperty(self, "longSeqROProp")
        addProperty(self, "longSeqRWProp")

        addProperty(self, "strSeqProp")

        addProperty(self, "blarROProp")
        addProperty(self, "blarRWProp")

        addProperty(self, "timestampROProp", self.timestampIO)
        
        return
    #------------------------------------------------------------------------------

