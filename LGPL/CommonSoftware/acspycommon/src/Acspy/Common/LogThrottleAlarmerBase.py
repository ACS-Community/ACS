# @(#) $Id: Log.py,v 1.3 2012/04/23 22:46:03 javarias Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA,  2001
#    (c) European Southern Observatory, 2002
#    Copyright by ESO (in the framework of the ALMA collaboration)
#    and Cosylab 2002, All rights reserved
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
import abc

class LogThrottleAlarmerBase(object):
    '''
    Abstract base class for the LogThrottle to raise/clear alarms
    '''
    __metaclass__ = abc.ABCMeta
    
    @abc.abstractmethod
    def sendThrottleAlarm(self, active):
        '''
        Send/Clear the alarm for the LogThrottle
        
        Raise the alarm if active=True and clear otherwise
        '''
        return

