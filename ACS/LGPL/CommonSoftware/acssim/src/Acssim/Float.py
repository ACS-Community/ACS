# @(#) $Id: Float.py,v 1.1 2007/10/03 20:44:03 agrimstrup Exp $
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
# adouble with this library; if not, write to the Free Software Foundation, Inc.,
# 675 Massachusetts Ave, Cambridge, MA 02139, USA.  Correspondence concerning
# ALMA should be addressed as follows:
#
# Internet email: alma-sw-admin@nrao.edu
# "@(#) $Id: Float.py,v 1.1 2007/10/03 20:44:03 agrimstrup Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the Pdouble IDL interface:
'''

__version__ = "$Id: Float.py,v 1.1 2007/10/03 20:44:03 agrimstrup Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import ACS__POA
#--ACS Imports-----------------------------------------------------------------
from ACSImpl.GenericProperty     import GenericProperty
from Acssim.Monitors            import Monitorfloat
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
#--P property------------------------------------------------------------------
#------------------------------------------------------------------------------
class Pfloat(GenericProperty):
    '''
    Properties can be derived from Pdouble only if their IDL derives from
    ACS::Pdouble.
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
            return float(0)
        
        try:
            #coerce into an int type
            return eval("float(" + value + ")")
        except:
            #warn them about CDB access
            self.getLogger().logAlert("Unble to coerce '" + str(value) + "' into the correct type!")
            print_exc()
            #return an acceptable default value instead...an empty sequence
            return float(0)
    #--------------------------------------------------------------------------
    def getMonitorObject(self, scheduler, timeoutID):
        '''
        Helper method returns a monitor object of type double.
        '''
        return Monitorfloat(scheduler, timeoutID)
#------------------------------------------------------------------------------
#--RO property-----------------------------------------------------------------
#------------------------------------------------------------------------------
class ROfloat(ACS__POA.ROfloat, Pfloat):
    '''
    Properties can be derived from ROdouble only if their IDL derives from
    ACS::ROdouble.
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
        Pfloat.__init__(self, name, charCompRef, devIORef)
        return
#-----------------------------------------------------------------------------
#--RW property----------------------------------------------------------------
#-----------------------------------------------------------------------------
class RWfloat(ACS__POA.RWfloat, ROfloat):
    '''
    Properties can be derived from ROdouble only if their IDL derives from
    ACS::ROdouble.
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
        ROdouble.__init__(self, name, charCompRef, devIORef)
        return
#---------------------------------------------------------------------------

