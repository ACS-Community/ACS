# @(#) $Id: Subscription.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
# "@(#) $Id: Subscription.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the Subscription IDL interface:
'''

__version__ = "$Id: Subscription.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
#--ACS Imports-----------------------------------------------------------------
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class Subscription(object):
    '''
    Properties can be derived from Subscription only if their IDL derives from
    ACS::Subscription.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, scheduler, timeoutID):
        '''
        Constructor

        Params:
        - scheduler is an object used to schedule timeouts
        - timeoutID is quite literally the unique ID for timeouts

        Returns: Nothing

        Raises: Nothing.
        '''
        self.scheduler = scheduler
        self.timeoutID = timeoutID
        return
    #--------------------------------------------------------------------------
    def suspend(self):
        '''
        Implementation of IDL method.

        void suspend ();
        '''
        self.scheduler.suspendTimeout(self.timeoutID)
        return
    #--------------------------------------------------------------------------
    def resume(self):
        '''
        Implementation of IDL method.

        void resume ();
        '''
        self.scheduler.resumeTimeout(self.timeoutID)
        return
    #--------------------------------------------------------------------------
    def destroy(self):
        '''
        Implementation of IDL method.

        void destroy ();
        '''
        self.scheduler.cancelTimeout(self.timeoutID)
        return
#------------------------------------------------------------------------------
if __name__=="__main__":
    print
