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
# @(#) $Id$
#-----------------------------------------------------------------------------

'''
Includes complex class definitions, functions, etc that will be
used by simulated IDL components. 

There are several intended purposes of this module and the 
first is to place code relating to complex simulations outside 
of the domain of the ACS CDB. In doing so, code checking tools 
can be used to analyze the desired simulator behavior looking for
Python misuses.
'''
#-----------------------------------------------------------------------------
from ACSImpl.DevIO import DevIO

#-----------------------------------------------------------------------------
class CurrentDevIO(DevIO):
    '''
    A readwrite BACI double property DevIO implementation for the
    "current" property of the PS::PowerSupply IDL interface. This
    particular implementation mimics the C++ acsexmplPowerSupplyCurrentImpl.h
    header which:
        writes out the value being written to it (when being set) to the
        "readback" readonly BACI double property also of the PS::PowerSupply
        IDL interface.
    
    As an example, this is quite useful as it shows how to mimic the C++
    PowerSupply example precisely without ever coding a full component 
    implementation.
    '''

    def __init__(self, comp_ref):
        '''
        Constructor
        
        Parameters:
            comp_ref - a reference to the component which is creating
            "us"
            
        Returns: Nothing
        
        Raises: Nothing
        '''
        #intialize the superclass providing a suitable default 
        #double value
        DevIO.__init__(self, 3.14)
        
        #save a copy of the component for later
        self.comp_ref = comp_ref
        
        #reference to the current property
        #no guarantee this has been created yet so we set it 
        #to None and get it on demand later
        self.current_prop = None
        
    def write(self, value):
        '''
        Overridden. 
        
        Calls the super implementation and sets the "current" readonly
        BACI double property to value.
        '''
        #call super
        DevIO.write(self, value)
        
        #get the REAL current property implementation if
        #write has not been called before
        if self.current_prop == None:
            #to access the readback property, it's necessary
            #to use this weird machination as the readbackObject
            #is a so-called private member of self.comp_ref
            self.current_prop = self.comp_ref.__dict__['__readbackObject']
            
        #now set the current's value
        self.current_prop.value.write(value)
        return
#-----------------------------------------------------------------------------