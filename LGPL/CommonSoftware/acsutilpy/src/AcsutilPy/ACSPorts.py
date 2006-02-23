# @(#) $Id: ACSPorts.py,v 1.4 2005/10/17 15:55:43 dfugate Exp $
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
# "@(#) $Id: ACSPorts.py,v 1.4 2005/10/17 15:55:43 dfugate Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2003/010/15  Created.
#------------------------------------------------------------------------------
'''
This module is used to find out which ports ACS is running on

TODO:
- nada
'''
#------------------------------------------------------------------------------
__revision__ = "$Id: ACSPorts.py,v 1.4 2005/10/17 15:55:43 dfugate Exp $"
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
def getIP():
    '''
    Returns the stringified version of the local hosts IP or the fully qualified
    domain name if there is an error.
    
    Parameters: None

    Return: stringified version of the local hosts IP
  
    Raises: Nothing
    '''
    #determine the local hosts IP address in string format
    localhost = ""
    localhost = str(gethostbyname(getfqdn()))
    #if what's above failed for some reason...
    if (localhost == None) or (localhost == ""):
        #take the HOST environment variable
        localhost = str(environ['HOST'])
    return localhost
#------------------------------------------------------------------------------
