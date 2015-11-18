#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2011 
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
# "@(#) $Id: ACSPorts.py,v 1.9 2012/01/20 22:07:44 tstaig Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# dfugate   2003/010/15  Created.
#------------------------------------------------------------------------------
'''
This module is used to find out which ports ACS is running on

TODO:
- nada
'''
#------------------------------------------------------------------------------
__revision__ = "$Id: ACSPorts.py,v 1.9 2012/01/20 22:07:44 tstaig Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
from os      import environ
from socket  import getfqdn, gethostbyname
from sys     import argv
#----------------------------------------------------------------------------
def getBasePort():
    '''
    Returns the "ACS Base Port". This is just an integer ranging from 0-10. Tries
    to determine the value in the following ways:
    1. Searches the command-line for --baseport option
    2. Searches the command-line for the -b option
    3. Looks for the ACS_INSTANCE environment variable
    4. Defaults to 0.
  
    Parameters: None
  
    Return: base port number
    
    Raises: Nothing
    '''

    #look for --baseport in the command-line
    try:
        base_port = int(argv[ argv.index("--baseport") + 1 ])
        return base_port
    except:
        pass

    #look for -b in the command-line
    try:
        base_port = int(argv[ argv.index("-b") + 1 ])
        return base_port
    except:
        pass
    
    try:
        return int(environ['ACS_INSTANCE'])
    except KeyError, excp:
        return int(0)
    except ValueError, excp:
        #DWF-log this!
        return int(0)

#----------------------------------------------------------------------------
def getManagerPort():
    '''
    Returns the port manager is running on.

    Parameters: None
  
    Return: manager port
  
    Raises: Nothing
    '''  
    return str(getBasePort()*100 + 3000 + 0)
#----------------------------------------------------------------------------
def getNamingServicePort():
    '''
    Returns the port the CORBA Naming Service is running on.
    
    Parameters: None
    
    Return: port the CORBA Naming Service is running on
  
    Raises: Nothing
    '''  
    return str(getBasePort()*100 + 3000 + 1)
#----------------------------------------------------------------------------
def getNotifyServicePort():
    '''
    Returns the port the CORBA Notification Service is running on.
    
    Parameters: None
  
    Return: port the CORBA Notification Service is running on
  
    Raises: Nothing
    '''  
    return str(getBasePort()*100 + 3000 + 2)
#----------------------------------------------------------------------------
def getLoggingServicePort():
    '''
    Returns the port the CORBA Logging Service is running on.
  
    Parameters: None
  
    Return: port the CORBA Logging Service is running on

    Raises: Nothing
    '''  
    return str(getBasePort()*100 + 3000 + 3)
#----------------------------------------------------------------------------
def getIRPort():
    '''
    Returns the port the CORBA Interface Repository is running on.
    
    Parameters: None
    
    Return:  port the CORBA Interface Repository is running on
    
    Raises: Nothing
    '''  
    return str(getBasePort()*100 + 3000 + 4)
#----------------------------------------------------------------------------
def getLogPort():
    '''
    Returns the port the ACS Logging Service is running on.
    
    Parameters: None
    
    Return: port the ACS Logging Service is running on
    
    Raises: Nothing
    '''  
    return str(getBasePort()*100 + 3000 + 11)
#----------------------------------------------------------------------------
def getCDBPort():
    '''
    Returns the port the ACS CDB is running on.
    
    Parameters: None
    
    Return: port the ACS CDB is running on
    
    Raises: Nothing
    '''  
    return str(getBasePort()*100 + 3000 + 12)
#----------------------------------------------------------------------------
def getContainerDaemonPort():
    '''
    Returns the port the ACS Container Daemon is running on.
    
    Parameters: None
    
    Return: port the ACS Container Daemon is running on
    
    Raises: Nothing
    '''  
    return str(3000 + 13)
#----------------------------------------------------------------------------
def getServicesDaemonPort():
    '''
    Returns the port the ACS Services Daemon is running on.
    
    Parameters: None
    
    Return: port the ACS Services Daemon is running on
    
    Raises: Nothing
    '''  
    return str(3000 + 14)
#----------------------------------------------------------------------------
def getIP():
    '''
    Returns the stringified version of the local hosts IP or the fully qualified
    domain name if there is an error.
    
    Parameters: None

    Return: stringified version of the local hosts IP
  
    Raises: Nothing
    '''
    #first we check if we have ACS_HOST defined
    try:
        return str(gethostbyname(environ['ACS_HOST']))
    except:
        pass
    
    #determine the local hosts IP address in string format
    localhost = ""
    if (environ['OSYSTEM'] == environ['CYGWIN_VER']):
        localhost = str(gethostbyname(getfqdn().split(".")[0]))
    else:
        localhost = str(gethostbyname(getfqdn()))
    #if what's above failed for some reason...
    if (localhost == None) or (localhost == ""):
        #take the HOST environment variable
        localhost = str(environ['HOST'])
    return localhost
#------------------------------------------------------------------------------
