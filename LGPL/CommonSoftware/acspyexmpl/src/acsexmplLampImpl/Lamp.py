#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2005 
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
# "@(#) $Id: Lamp.py,v 1.1 2005/11/23 06:02:13 dfugate Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# dfugate  2005-11-23  created
#

#----------------------------------------------------------------------------
'''
This example is just a prototype. Please take anything you see here with a
grain of salt so to speak. Still to be determined if the Python BACI
prototype will be completed!
TODO:
- everything!
'''
#--CORBA STUBS-----------------------------------------------------------------
import acsexmplLamp__POA
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.CharacteristicComponent import CharacteristicComponent
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Util.BaciHelper             import addProperty
#------------------------------------------------------------------------------
class Lamp(CharacteristicComponent,
           acsexmplLamp__POA.Lamp,
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
        return
    #------------------------------------------------------------------------------  
    def initialize(self):
        '''
        '''
        addProperty(self, "brightness")
        return
    #------------------------------------------------------------------------------
    def on(self, cb_void, cb_desc_in):
        '''
        CORBA Method
        '''
        #no-op
        return
    #------------------------------------------------------------------------------
    def off(self, cb_void, cb_desc_in):
        '''
        CORBA Method
        '''
        #no-op
        return
    


#
# ___oOo___
