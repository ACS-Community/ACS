#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsNotifysStatus.py,v 1.3 2009/09/18 06:39:11 hyatagai Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA, 2001
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
###############################################################################
'''
This script is designed to emit information about all known notify services including
those living on remote hosts for a given $ACS_INSTANCE.

Paramters: None

Assumptions:
- $ACS_TMP/ACS_INSTANCE.$ACS_INSTANCE/USED_NOTIFY_PORTS exists

TODO:
- accept "-b" switch for $ACS_INSTANCE???
'''
###############################################################################
from os      import environ
from os      import chdir
from os      import access, R_OK, W_OK, X_OK, F_OK
from os.path import exists
from fcntl   import lockf
from fcntl   import LOCK_SH
from sys     import stderr
from sys     import exit

import socket
from AcsutilPy.ACSDirectory import getAcsTmpDirectoryPath

#-----------------------------------------------------------------------------
#--Functions
def getPortsFile():
    '''
    Returns the file containing a list of NSs and used ports.
    In the event of error, bails out of Python.
    '''
    
    #initialize the return value
    ret_val = None
    
    #directory where all the process IDs of this particular instance of 
    #ACS are stored
    ACS_TMP_DIR = getAcsTmpDirectoryPath()
    ACS_INSTANCE_DIR = ACS_TMP_DIR + '/ACS_INSTANCE.' + str(cl_baseport)

    #make sure the acs instance directory exists
    if not exists(ACS_INSTANCE_DIR):
        stderr.write("$ACS_TMP_DIR/ACS_INSTANCE.$ACS_INSTANCE does not exist!\n")
        exit(1)
            
    #make sure the user actually has write access to this ACS instance
    elif not access(ACS_INSTANCE_DIR, R_OK & W_OK & X_OK & F_OK):
        stderr.write("The ACS instance in '" +
                     str(ACS_INSTANCE_DIR) +
                     "' is not accessible by this user!\n")
        exit(1)
    
    #make sure the file full of port numbers exists. if it does not, create it
    if not exists(ACS_INSTANCE_DIR + '/USED_NOTIFY_PORTS'):
        stderr.write("The USED_NOTIFY_PORTS file for the ACS instance in '" +
                     str(ACS_INSTANCE_DIR) +
                     "' does not exist!\n")
        exit(1)

    #go to it
    chdir(ACS_INSTANCE_DIR)
    
    #open the file
    ret_val = open('USED_NOTIFY_PORTS', 'r')
    
    #lock it
    lockf(ret_val.fileno(), LOCK_SH)
    
    return ret_val

#-----------------------------------------------------------------------------
def portIsFree(ip_addr, tcp_port):
    '''
    Simple helper function returns true if the TCP port, tcp_port, of the
    host, ip_addr, is free to use.
    '''
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(1)
    #returns true when the port is open and false if it's not
    ret_val = s.connect_ex((ip_addr, tcp_port))
    s.close()
    return ret_val

#-----------------------------------------------------------------------------
def getNotifyDict(portsFile):
    '''
    Returns a dictionary where each key is the name of a NS and the 
    value is the absolute TCP port number it should use.

    Params:
    - portsFile A file which contains "someNotifyName 1234" on each line 
    where 1234 is a TCP port number.

    Returns: a dictionary where each key is the name of a NS and the 
    value is the absolute TCP port number it should use
    '''
    #return value
    ret_val_ports = {}
    ret_val_hosts = {}
    
    #build up the list of ports that have already been used
    lines = portsFile.readlines()
        
    for line in lines:  #line = 'someNotifyName portnumber'
        line = line.strip()
        cont_name = line.split(' ')[0]
        host      = line.split(' ')[2]
        port_number    = int(line.split(' ')[1])
        
        ret_val_ports[cont_name] = port_number
        ret_val_hosts[cont_name] = host

    return (ret_val_ports, ret_val_hosts)

#--------------------------------------------------------------------------
#--determine the ACS_INSTANCE
cl_baseport = int(environ['ACS_INSTANCE'])

#get the file which shows us which ports are currently taken up
container_file = getPortsFile()

#get dictionaries mapping container names to TCP port numbers and hosts
container_ports, container_hosts = getNotifyDict(container_file)

container_file.close()
#--------------------------------------------------------------------
print "Emitting information about all known local and remote notify services:"

for cont_name in container_hosts.keys():
    
    host = container_hosts[cont_name]
    port = container_ports[cont_name]

    if not portIsFree(host, port):
        print "The", cont_name, "notify service is running on", host, "using the TCP port:", port
    else:
        print "The", cont_name, "notify service is not currently running but was at some point in time."    
################################################################    
