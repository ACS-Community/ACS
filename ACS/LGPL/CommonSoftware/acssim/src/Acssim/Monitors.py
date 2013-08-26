# @(#) $Id: Monitors.py,v 1.1 2007/10/03 20:44:03 agrimstrup Exp $
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
# "@(#) $Id: Monitors.py,v 1.1 2007/10/03 20:44:03 agrimstrup Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the Monitor IDL interface

TODO:
- on-change monitors not implemented yet
'''

__version__ = "$Id: Monitors.py,v 1.1 2007/10/03 20:44:03 agrimstrup Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
import ACS__POA
#--ACS Imports-----------------------------------------------------------------
from ACSImpl.Monitors import GenericMonitor

#--GLOBALS---------------------------------------------------------------------

class Monitorfloat(ACS__POA.Monitorfloat, GenericMonitor):
    '''
    Properties can be derived from Monitordouble only if their IDL derives from
    ACS::Monitordouble.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, scheduler, timeoutID):
        '''
        Constructor

        Params:
        - scheduler is a scheduler used to schedule timeouts
        - timeoutID is quite literally a timeout ID which uniquely identifies
        this monitor

        Returns: Nothing

        Raises: ???
        '''
        GenericMonitor.__init__(self, scheduler, timeoutID)
        return
