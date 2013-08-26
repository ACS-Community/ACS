# @(#) $Id: Long.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
# "@(#) $Id: Long.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the Plong IDL interface
'''

__version__ = "$Id: Long.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import ACS__POA
#--ACS Imports-----------------------------------------------------------------
from ACSImpl.GenericProperty     import GenericProperty
from ACSImpl.Monitors            import Monitorlong
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
#--P property------------------------------------------------------------------
#------------------------------------------------------------------------------
class Plong(GenericProperty):
    '''
    Properties can be derived from Plong only if their IDL derives from
    ACS::Plong.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, name, charCompRef, devIORef):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property
        - devIORef is a reference to a DevIO to be used with this property

        Returns: Nothing

        Raises: Nothing.
        '''
        GenericProperty.__init__(self, name, charCompRef, devIORef)
        return
    #--------------------------------------------------------------------------
    def coerceToPropertyType(self, value=None):
        '''
        Overriden.
        '''
        #something went wrong. Return default value
        if value==None:
            return 0
        
        try:
            #coerce into an int type
            return eval("int(" + value + ")")
        except:
            #warn them about CDB access
            self.getLogger().logAlert("Unble to coerce '" + str(value) + "' into the correct type!")
            print_exc()
            #return an acceptable default value instead...an empty sequence
            return 0
    #--------------------------------------------------------------------------
    def getMonitorObject(self, scheduler, timeoutID):
        '''
        Helper method returns a monitor of the correct type.
        '''
        return Monitorlong(scheduler, timeoutID)
#------------------------------------------------------------------------------
#--RO property-----------------------------------------------------------------
#------------------------------------------------------------------------------
class ROlong(ACS__POA.ROlong, Plong):
    '''
    Properties can be derived from ROlong only if their IDL derives from
    ACS::ROlong.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, name, charCompRef, devIORef=None):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property
        - devIORef is a reference to a DevIO to be used with this property

        Returns: Nothing

        Raises: Nothing.
        '''
        Plong.__init__(self, name, charCompRef, devIORef)
        return
#-----------------------------------------------------------------------------
#--RW property----------------------------------------------------------------
#-----------------------------------------------------------------------------
class RWlong(ACS__POA.RWlong, ROlong):
    '''
    Properties can be derived from ROlong only if their IDL derives from
    ACS::ROlong.
    '''
    #-------------------------------------------------------------------------
    def __init__(self, name, charCompRef, devIORef=None):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property
        - devIORef is a reference to a DevIO to be used with this property

        Returns: Nothing

        Raises: Nothing.
        '''
        ROlong.__init__(self, name, charCompRef, devIORef)
        return
#---------------------------------------------------------------------------
