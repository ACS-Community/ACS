# @(#) $Id: Components.py,v 1.1 2004/12/15 14:29:41 dfugate Exp $
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
# "@(#) $Id: Components.py,v 1.1 2004/12/15 14:29:41 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
This module provides access to simulated components in their entirety.

TODO LIST:
- all
'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------

#--GLOBALS---------------------------------------------------------------------
_DEBUG = 0
_compDict = {}
#------------------------------------------------------------------------------
def addComponent(compName, compRef):
    '''
    Adds a component to the singled dictionary contained within this module.

    Parameters:
    compName - name of the component in string format
    compRef - reference to the component.

    Raises: Nothing

    Returns: Nothing
    '''
    _compDict[compName] = compRef
    return
#------------------------------------------------------------------------------
def removeComponent(compName):
    '''
    Removes a component from the singled dictionary contained within this module.

    Parameters:
    compName - name of the component in string format

    Raises: ???

    Returns: Nothing
    '''
    _compDict.pop(compName)
    return
#------------------------------------------------------------------------------
def getComponent(compName):
    '''
    Returns a reference to a simulated component which has been activated.

    Parameters:
    compName - name of the component in string format

    Raises: ???

    Returns: reference to a simulated component.
    '''
    return _compDict[compName]
#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Testing"

    try:
        getComponent("dkfjkdf")
        print "Should not see this!"
    except:
        print "Good."
    
    addComponent("my", 7)

    print "Good:", getComponent("my")

    removeComponent("my")
    try:
        removeComponent("dfdfd")
        print "Should not see this!"
    except:
        print "Good."    
        
         
    print "Done:", _compDict
