#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsstartupNotifyPort.py,v 1.4 2009/11/24 08:47:01 hyatagai Exp $
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
This script is designed to pick a free notify service (NS) port for ACS to run on using 
ALL the arguments passed to the NS. If theres a free port to run under,
it prints that to standard out. If the script encounters some error, it exits
with status 1.

Parameters: too many to list. Run "acsstarupNotifyPort -h" to see them all

Assumptions: None. This script should handle most errors graciously returning
with an error exit code.

TODO:
- 
'''  
###############################################################################
from os      import environ
from os      import chdir
from os      import system
from os      import fstat 
from os      import access, R_OK, W_OK, X_OK, F_OK
from os.path import exists
from fcntl   import lockf
from fcntl   import LOCK_EX
from fcntl   import LOCK_UN
from sys     import stderr
from sys     import argv
from sys     import exit
from time    import sleep
import atexit

from optparse import OptionParser

import subprocess
import socket

from AcsutilPy.ACSPorts import getIP
from AcsutilPy.ACSDirectory import getAcsTmpDirectoryPath

__DEBUG__ = False
container_file = None
BASESLEEPTIME = 127

#-----------------------------------------------------------------------------
#--Functions
def cleanUp():
    global container_file
    global BASESLEEPTIME
    
    maxsleepperiod = int(environ['ACS_STARTUP_TIMEOUT_MULTIPLIER']) * BASESLEEPTIME
    sleepperiod = 1    
    while container_file is not None and sleepperiod <= maxsleepperiod:
        try:
            container_file.flush()
            lockf(container_file.fileno(), LOCK_UN)
        except Exception, e:
            stderr.write("WARNING: acsstartupNotifyPort is unable to release lock.  Reason: %s Retrying in %d seconds." % (e, sleepperiod))
            sleep(sleepperiod)
            sleepperiod *= 2
            continue
        try:
            container_file.close()
        except Exception, e:
            stderr.write("WARNING: acsstartupNotifyPort file close operation failed.  Reason: %s." % e)
        container_file = None
            
    if container_file is not None:
        stderr.write("FATAL: acsstartupNotifyPort was unable to release lock")
        exit(2)
        
def getPortsFile(baseport):
    '''
    Returns the file containing a list of notify services and used ports
    '''
    
    #initialize the return value
    ret_val = None
    
    #directory where all the process IDs of this particular instance of 
    #ACS are stored
    ACS_TMP_DIR = getAcsTmpDirectoryPath()
    ACS_INSTANCE_DIR = ACS_TMP_DIR + '/ACS_INSTANCE.' + str(baseport)

    #make sure the acs instance directory exists
    if not exists(ACS_INSTANCE_DIR):
        #assume everything is running remotely
        ACS_INSTANCE_DIR = ACS_TMP_DIR
        if not exists(ACS_INSTANCE_DIR):
            stderr.write("$ACS_TMP_DIR does not exist!\n")
            exit(1)
            
    #make sure the user actually has write access to this ACS instance
    elif not access(ACS_INSTANCE_DIR, R_OK & W_OK & X_OK & F_OK):
        stderr.write("The ACS instance in '" +
                     str(ACS_INSTANCE_DIR) +
                     "' is not accessible by this user!\n")
        exit(1)
    
    #make sure the file full of port numbers exists. if it does not, create it
    if not exists(ACS_INSTANCE_DIR + '/USED_NOTIFY_PORTS'):
        system('touch ' + ACS_INSTANCE_DIR + '/USED_NOTIFY_PORTS')
        system ('chmod 774 ' + ACS_INSTANCE_DIR + '/USED_NOTIFY_PORTS')
        if __DEBUG__:
            stderr.write("USED_NOTIFY_PORTS did not exist\n")

    #go to it
    chdir(ACS_INSTANCE_DIR)
    
    #open the file
    ret_val = open('USED_NOTIFY_PORTS', 'r+')
    
    #lock it
    lockf(ret_val.fileno(), LOCK_EX)
    
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
    
    #make sure we don't end up with an empty list
    if lines == []:
        lines.append('xxx 0 yyy')
        
    for line in lines:  #line = 'someNotifyName portnumber'
        line = line.strip()
        cont_name = line.split(' ')[0]
        host      = line.split(' ')[2]
        port_number    = int(line.split(' ')[1])
        
        ret_val_ports[cont_name] = port_number
        ret_val_hosts[cont_name] = host

    return (ret_val_ports, ret_val_hosts)


#-----------------------------------------------------------------------------
def getExistingPort(cont_name, port_dict, host_dict):
    '''
    If the port dictionary already has an entry stating that this NS 
    should be using some port, returns that port. If not, returns None.
    '''
    
    if port_dict.has_key(cont_name):
        
        #get the TCP port and hostname
        port_number = port_dict[cont_name]
        host_name   = host_dict[cont_name]
        
        return (port_number, host_name)

    return (None, None)

#-----------------------------------------------------------------------------
def portNumberAlreadyUsed(port_number, host, 
                          port_dict, host_dict):
    '''
    Function returns 1 if the port number is already in use and false
     otherwise.
    '''
    
    #cycle through all the container names...
    for cont_name in port_dict.keys():
        #looking for a container such that it's TCP port matches what we want
        #to use and so does its hostname
        if port_dict[cont_name]==port_number and host_dict[cont_name]==host:
            if __DEBUG__:
                stderr.write("Port for -port option is already being used:" +
                             str(port_number) + "\n")
            return 1
    
    #now a real check to see if the port is physcially free
    if portIsFree(host, port_number):
        return 0
    else:
        stderr.write("Port is already in use:" +
                     str(port_number) + "\n")
        stderr.write("Kill the process listening on this port and try again!\n")
        return 1

#-----------------------------------------------------------------------------
def coercePortNumber(port_number):
    '''
    This helper function takes a port number which can be in string or integer
    format and converts it to an integer. In the event of any failure, it 
    returns None. Also, in the case that the port number is set to be an offset
    from the ACS_INSTANCE, this method handles that as well.
    
    Params: port_number port number. Can be absolute (i.e., "3025") or dynamic 
    (i.e., "0"-"9").
    
    Returns: the port number in integer format or None if there was some sort
    of failure
    '''
    
    #make sure it's really a number
    try:
        port_number = int(port_number)
    except:
        return None
    
    #we allow the possibility the port is specified but still dynamic
    if port_number>=0 and port_number<10:
        port_number = port_number + 20 + cl_baseport*100 + 3000
        if __DEBUG__:
            stderr.write("port_number is dynamic:" +
                         str(port_number) + "\n")
        return port_number
            
    return port_number

#-----------------------------------------------------------------------------
def getNextAvailablePort(host, ports_dict, hosts_dict, baseport):
    '''
    Returns the next available port
    '''
#    global cl_baseport
    
    ret_val = None

    min_tcp = baseport*100 + 3000 + 20
    max_tcp = baseport*100 + 3000 + 30
    
    #ignore odd-numbered ports
    for i in range(min_tcp,
                   max_tcp,
                   1):
        
        #found a port that can be used
        if portNumberAlreadyUsed(i, host,
                                 ports_dict, hosts_dict) == 0:
            ret_val = i
            break

    return ret_val

def main(prog_args):
    #------------------------------------------------------------------------------
    #--Parse the command-line options.
    global __DEBUG__
    
    usage_msg='''
This script is used to generate a command-line to be passed to container 
startup scripts. It will include such information as the TCP port the 
container should be run under. Do not call this script directly from your own
code!.
'''
    parser = OptionParser(usage=usage_msg)
    
    #end-users can specify a specific port number to run under 
    port_help_msg='''
TCP port to run the notify service under.
If this integer value is in the inclusive range of 0-9, it is assumed
that the intended TCP port is really an offset equivalent to the 
following:   REAL TCP PORT = port + 3020 + $ACS_INSTANCE*100.
If this integer value is greater than 9, the TCP port is used as
provided.
'''
    parser.add_option("--port",
                      dest="port",
                      help=port_help_msg)
    
    #end-users can use this parameter to specify the name of the container
    name_help_msg='''
Name of the notify service.
Users can optionally specify the name of the notify service directly using
this command-line switch. If this switch is not used, it is assumed the
first argument after all command-line switches is the name of the
notify service.
'''
    parser.add_option("--name",
                      dest="name",
                      help=name_help_msg)
    
    #Debug flag
    parser.add_option("--debug",
                      action="store_true",
                      dest="debug",
                      default=0,
                      help="Prints out special debugging info to standard error.")
    
    #baseport
    baseport_help_msg='''
ACS baseport (i.e., 0-9).
Setting this flag overrides the value of $ACS_LOG_STDOUT.
'''
    parser.add_option("-b", "--baseport",
                      dest="baseport",
                      help=baseport_help_msg,
                      default=None)
    
    #--------------------------------------------------------------------------
    #--Make commands backwards compatible with pre ACS 6.0 usage.
    #--Basically this means that commands of the form -xy... should still 
    #--be supported. This manual manipulation of prog_args is necessary as 
    #--optparse only supports switches of the form --xy...
    
    #go through every potential command-line switch
    for i in range(0, len(prog_args)):
        #if the switch is something like -managerReference change it
        #to --managerReference
        if len(prog_args[i])>2 and prog_args[i][0]=='-' and prog_args[i][1]!='-':
            prog_args[i] = "-" + prog_args[i]
    
    #--------------------------------------------------------------------------
    #--Parse everything and throw the results into global variables
    (options, parsed_argv) = parser.parse_args(args=prog_args)
    
    cl_port              = options.port
    cl_name              = options.name
    cl_baseport          = options.baseport
    __DEBUG__            = options.debug

    #--------------------------------------------------------------------------
    #--Go through the command-line arguments:
    #--  creating a few global variables based on them
    #--  do a little preprocessing on their values
    
    #--determine the ACS_INSTANCE
    if cl_baseport==None:
        cl_baseport = int(environ['ACS_INSTANCE'])
    else:
        cl_baseport=int(cl_baseport)
    
    #no notify name specified
    if cl_name==None:
        if len(parsed_argv)==0:
            stderr.write("Must specificy a notify service name using the -name option!\n")
            return 5
        #error is recoverable - NS name specified as a general argument
        else:
            cl_name = parsed_argv.pop(0)
    
    #turn the TCP port into the real TCP port
    if cl_port!=None:
        
        if __DEBUG__:
            stderr.write("-port option was specified:" + 
                          str(cl_port) + "\n")
        
        #string to int
        newPort = int(cl_port)
        
        #try to coerce the port number into a proper TCP port number
        newPort = coercePortNumber(newPort)
    else:
        newPort=None
    
    # local
    container_host = getIP()

    #--------------------------------------------------------------------
    #--GLOBALS-----------------------------------------------------------
    
    #get the file which shows us which ports are currently taken up
    container_file = getPortsFile(cl_baseport)
    
    #get dictionaries mapping container names to TCP port numbers and hosts
    container_ports, container_hosts = getNotifyDict(container_file)
    
    #--------------------------------------------------------------------
    #--SANITY CHECKS-----------------------------------------------------
    
    #recorded host is different from commanded host
    if container_hosts.has_key(cl_name) and container_hosts[cl_name]!=container_host:
        stderr.write("Trying to run the '" + cl_name + "' notify service on '" +
                      container_host + "' instead of '" + container_hosts[cl_name] +
                      "' impossible!\n")
        stderr.write("Changing hosts after a notify service is restarted is not permitted!\n")
        return 8
        
    #recorded TCP port requested is different from commanded TCP port
    if newPort!=None and container_ports.has_key(cl_name) and container_ports[cl_name]!=newPort:
        stderr.write("Trying to change the '" + cl_name + "' notify's TCP port from '" +
                      str(container_ports[cl_name]) + "' to '" + str(newPort) +
                      "' is impossible!\n")
        stderr.write("Changing TCP ports after a notify service is restarted is not permitted!\n")
        return 9
        
    #ensure developer hasn't picked something that's already being used
    elif newPort!=None and (not container_ports.has_key(cl_name)) and portNumberAlreadyUsed(newPort, container_host,
                                                                                            container_ports, container_hosts):
        stderr.write("Port number specified via -port switch, " + 
                     str(newPort) + 
                     ", is already assigned!\n")
        return 10
    
    #the desired TCP port is in use by some other process
    if newPort!=None and not portIsFree(container_host, newPort):
        stderr.write("Cannot use the '" + str(newPort) + 
                      "' TCP port as it's being tied up by some other process!\n")
        stderr.write("Use the netstat command to find the offending process!\n")
        return 11
        
    #-----------------------------------------------------------------------------
    #debugging purposes only!        
    if __DEBUG__:
        stderr.write("Command-line TCP port:" + str(cl_port) + "\n")
        stderr.write("Command-line notify name:" + str(cl_name) + "\n")
 
#-----------------------------------------------------------------------------
#--MAIN-----------------------------------------------------------------------
    #see if it's been set before...
    temp_port, temp_host = getExistingPort(cl_name, 
                                           container_ports, 
                                           container_hosts)

    #NS already had a port assigned to it. fine.
    if temp_port != None:
        #overwrite any value the user may have tried to specify using
        #the port switch. if it's been set before, it must remain the same.    
        newPort = temp_port
        container_host = temp_host
        
        #if this NS is being run for the first time and
        #the user doesn't care which port it runs on
    elif newPort is None:
        #just get the next available TCP port. This will always
        #work unless there are 9+ NSs 
        newPort = getNextAvailablePort(container_host,
                                       container_ports, container_hosts, cl_baseport)
   
    #NS does not already have a port assigned to it and the 
    #new port is not nil...
    if temp_port==None and newPort!=None:
        container_file.writelines([cl_name + ' ' + 
                                   str(newPort) + ' ' +
                                   str(container_host) + '\n'])

    cleanUp()

    #at this point we should have found a free port number.
    #we can now close up the container_file so that other containers
    #can be started immediately.
    if newPort==None:
        #could not find a free port. no point in going on.
        stderr.write("All ports are taken!\n")
        return 2

    print newPort;
    print
    ################################################################
    return 0

if __name__ == '__main__':
    atexit.register(cleanUp)
    exit(main(argv[1:]))
