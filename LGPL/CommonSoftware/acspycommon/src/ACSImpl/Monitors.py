# @(#) $Id: Monitors.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
# "@(#) $Id: Monitors.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
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

__version__ = "$Id: Monitors.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
import ACS__POA
from CORBA                import NO_IMPLEMENT
#--ACS Imports-----------------------------------------------------------------
from ACSImpl.Subscription          import Subscription
from Acspy.Util.Scheduler          import Scheduler
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class Monitor(Subscription):
    '''
    Properties can be derived from Monitor only if their IDL derives from
    ACS::Monitor.
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
        Subscription.__init__(self, scheduler, timeoutID)
        return
    #--------------------------------------------------------------------------
    def _get_start_time(self):
        '''
        Implementation of IDL attribute.

        readonly attribute Time start_time;
        '''
        return self.scheduler.getTimeout(self.timeoutID)['timeToOccur']
    #--------------------------------------------------------------------------
    def set_timer_trigger(self, timer):
        '''
        Implementation of IDL method.

        void set_timer_trigger (in TimeInterval timer);
        '''
        self.scheduler.changeTimeoutFrequency(self.timeoutID, timer)
        return
    #--------------------------------------------------------------------------
    def get_timer_trigger(self):
        '''
        Implementation of IDL method.

        void get_timer_trigger (out TimeInterval timer);
        '''
        return self.scheduler.getTimeout(self.timeoutID)['frequency']
#------------------------------------------------------------------------------
class GenericMonitor(Monitor):
    '''
    Generic monitor baseclass.
    Warning - this does not support on-change monitors yet.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, scheduler, timeoutID):
        '''
        Constructor

        Params: None

        Returns: Nothing

        Raises: Nothing.
        '''
        Monitor.__init__(self, scheduler, timeoutID)
        return
    #--------------------------------------------------------------------------
    def set_value_trigger(self, delta, enable):
        '''
        Implementation of IDL method.

        void set_value_trigger (in stringSeq delta, in boolean enable);

        Warning - not presently implemented!
        '''
        del delta   #to make pychecker happy
        del enable   #to make pychecker happy
        raise NO_IMPLEMENT()
        return
    #--------------------------------------------------------------------------
    def get_value_trigger(self):
        '''
        Implementation of IDL method.

        void get_value_trigger(out stringSeq delta, out boolean enable);

        Warning - not presently implemented!
        '''
        raise NO_IMPLEMENT()
        return
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
class Monitordouble(ACS__POA.Monitordouble, GenericMonitor):
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
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
class Monitorpattern(ACS__POA.Monitorpattern, GenericMonitor):
    '''
    Properties can be derived from Monitorpattern only if their IDL derives from
    ACS::Monitorpattern.
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

        Raises: Nothing.
        '''
        GenericMonitor.__init__(self, scheduler, timeoutID)
        return
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
class Monitorstring(ACS__POA.Monitorstring, GenericMonitor):
    '''
    Properties can be derived from Monitorstring only if their IDL derives from
    ACS::Monitorstring.
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

        Raises: Nothing.
        '''
        GenericMonitor.__init__(self, scheduler, timeoutID)
        return
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
class MonitorstringSeq(ACS__POA.MonitorstringSeq, GenericMonitor):
    '''
    Properties can be derived from MonitorstringSeq only if their IDL derives from
    ACS::MonitorstringSeq.
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

        Raises: Nothing.
        '''
        GenericMonitor.__init__(self, scheduler, timeoutID)
        return
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
class Monitorlong(ACS__POA.Monitorlong, GenericMonitor):
    '''
    Properties can be derived from Monitorlong only if their IDL derives from
    ACS::Monitorlong.
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

        Raises: Nothing.
        '''
        GenericMonitor.__init__(self, scheduler, timeoutID)
        return
#------------------------------------------------------------------------------
    
