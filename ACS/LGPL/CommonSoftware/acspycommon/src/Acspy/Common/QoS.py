#******************************************************************************
#    E.S.O. - ACS project
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
#
# "@(#) $Id: QoS.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# bjeram  2004-08-31  created
#
'''
Python ACS QoS class(es) and functions: Timeout
'''
__revision__ = "$Id: QoS.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#------------------------------------------------------------------------------
from omniORB import setClientCallTimeout, setClientThreadCallTimeout
#------------------------------------------------------------------------------
def setObjectTimeout(objref, timeout):
    '''
    Sets timeout (in milliseconds) on the given CORBA object. Future calls on 
    the CORBA object will have this timeout, receiving an exception if the call
    takes longer than the timeout value.
    '''
    setClientCallTimeout(objref, timeout)
    return

#------------------------------------------------------------------------------
def resetObjectTimeout(objref):
    '''
    Resets the timeout (to zero) for calls to the given CORBA object (sets to
    no timeout).
    '''
    setClientCallTimeout(objref, 0)
    return

#------------------------------------------------------------------------------
def setORBTimeout(timeout):
    '''
    Sets the timeout (in milliseconds) at the ORB level, affecting all method 
    invocations within the ORB.
    '''
    setClientCallTimeout(timeout)  
    return

#------------------------------------------------------------------------------
def resetORBTimeout():
    '''
    Resets (to zero) the ORB timeout, affecting all method invocations within 
    the ORB (sets to no timeout).
    '''
    setClientCallTimeout(0)  
    return

#------------------------------------------------------------------------------
class Timeout:
    '''
    Timeout is nearly identical to the C++ Timeout class (acsQoS module). 
    However, it does not work properly at the present time, because there is 
    not proper support from the python orb (OmniORB). For example, setting a 
    timeout with the Timeout object will (re)set the timeout of the ORB, which
    also affects method calls in other threads (undesirable side effect). For 
    this reason, it is currently recommanded to use (re)setObjectTimeout 
	which sets a timeout that affects calls to just that one CORBA object.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, timeout):
        '''
        The constructor for the Timeout object, which sets the timeout.
        
        Parameters:
        - timeout the timeout in milliseconds (a long).
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        self.timeout = None
        self.set(timeout)

    #--------------------------------------------------------------------------
    def __del__(self):
        '''
		The destructor for the Timeout object.
        '''
        self.reset()

    #--------------------------------------------------------------------------
    def get(self):
        '''
        Returns the timeout value (expressed in milliseconds).
        
        Parameters: None
        
        Returns: timeout, the timeout value expressed in milliseconds.
      
        Raises: Nothing
        '''
        return self.timeout

    #--------------------------------------------------------------------------
    def set(self, timeout=0):
        '''
        Sets the timeout value.
        
        Parameters:
        - timeout the timeout to be set (a long), expressed in milliseconds.
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        self.timeout = timeout
        setClientCallTimeout(self.timeout)
        return

    #--------------------------------------------------------------------------
    def reset(self):
        '''
        Resets the timeout (presently forces it to zero). 
        It should work in such a way that it resets the timeout 
        to the previous value, but the orb implementation does 
        not support functionality to retrieve the previously set timeout.
        So it sets timeout to 0 which means no timeout.
        
        Parameters: Nothing
        
        Returns: Nothing
      
        Raises: Nothing
        '''
        setClientCallTimeout(0)
        return

    #--------------------------------------------------------------------------
    def sset(self):
        '''
        Sets object\'s timeout
        
        Parameters: Nothing
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        setClientThreadCallTimeout(self.timeout)
        return
   
#------------------------------------------------------------------------------
#
# ___oOo___
