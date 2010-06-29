#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsstartupContainerPort.py,v 1.49 2010/06/29 12:31:44 rtobar Exp $
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
This script is designed to pick a free container port for ACS to run on using 
ALL the arguments passed to the container. If theres a free port to run under,
it prints that to standard out. If the script encounters some error, it exits
with status 1.

Parameters: too many to list. Run "acsstarupContainerPort -h" to see them all

Assumptions: None. This script should handle most errors graciously returning
with an error exit code.

TODO:
- 
'''  
###############################################################################
import os
if os.name != 'nt':
    from os      import link
from os      import environ
from os      import chdir
from os      import getcwd
from os      import system
from os      import fstat
from os      import access, R_OK, W_OK, X_OK, F_OK
from os      import rename
from os      import remove
from os      import getpid
from os.path import exists
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
        except Exception, e:
            stderr.write("WARNING: acsstartupContainerPort is unable to release lock.  Reason: %s Retrying in %d seconds." % (e, sleepperiod))
            sleep(sleepperiod)
            sleepperiod *= 2
            continue
        try:
            container_file.close()
        except Exception, e:
            stderr.write("WARNING: acsstartupContainerPort file close operation failed.  Reason: %s." % e)

        try:
            rename('USED_CONTAINER_PORTS.lock', 'deleteme.%d' % getpid())
            remove('deleteme.%d' % getpid())
        except OSError:
            stderr.write("Warning: USED_CONTAINER_PORTS lock file is missing.")

        container_file = None
            
    if container_file is not None:
        stderr.write("FATAL: acsstartupContainerPort was unable to release lock")
        exit(2)
        
def getPortsFile(baseport):
    '''
    Returns the file containing a list of containers and used ports
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
    if not exists(ACS_INSTANCE_DIR + '/USED_CONTAINER_PORTS'):
        system('touch ' + ACS_INSTANCE_DIR + '/USED_CONTAINER_PORTS')
        system ('chmod 774 ' + ACS_INSTANCE_DIR + '/USED_CONTAINER_PORTS')
        if __DEBUG__:
            stderr.write("USED_CONTAINER_PORTS did not exist\n")

    #go to it
    chdir(ACS_INSTANCE_DIR)

    #lock it
    gotlock = False
    while not gotlock:
        try:
            if os.name != 'nt':
                link('USED_CONTAINER_PORTS', 'USED_CONTAINER_PORTS.lock')
            else:
                if exists('USED_CONTAINER_PORTS.lock'):
                    raise OSError;
                system('touch USED_CONTAINER_PORTS.lock')
            gotlock = True
        except OSError:
            continue
    
    #open the file
    ret_val = open('USED_CONTAINER_PORTS', 'r+')
    
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
def getContainerDict(portsFile):
    '''
    Returns a dictionary where each key is the name of a container and the 
    value is the absolute TCP port number it should use.

    Params:
    - portsFile A file which contains "someContainerName 1234" on each line 
    where 1234 is a TCP port number.

    Returns: a dictionary where each key is the name of a container and the 
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
        
    for line in lines:  #line = 'someContainerName portnumber'
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
    If the port dictionary already has an entry stating that this container 
    should be using some port, returns that port. If not, returns None.
    '''
    
    if port_dict.has_key(cont_name):
        
        #get the TCP port and hostname
        port_number = port_dict[cont_name]
        host_name   = host_dict[cont_name]

        #if it's free to use just return that
        if portIsFree(host_name, port_number):
            return (port_number, host_name)

        else:
            stderr.write("Port '" + str(port_number) + 
                         "' is already in use on " + host_name +
                         "!\n")
            stderr.write("This is an unrecoverable error and you must shutdown or kill " +
                         "the original container to continue!\n")
            exit(1)
    
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
    
    Params: port_number port number. Can be absolute (i.e., "3075") or dynamic 
    (i.e., "0"-"24").
    
    Returns: the port number in integer format or None if there was some sort
    of failure
    '''
    
    #make sure it's really a number
    try:
        port_number = int(port_number)
    except:
        return None
    
    #we allow the possibility the port is specified but still dynamic
    if port_number>=0 and port_number<25:
        port_number = port_number*2 + 50 + cl_baseport*100 + 3000
        if __DEBUG__:
            stderr.write("port_number is dynamic:" +
                         str(port_number) + "\n")
        return port_number
            
    #port is OK to use but it's an odd number (reserved)
    if (port_number % 2) == 1 and port_number>3000 and port_number<4000:
        port_number = None
        if __DEBUG__:
            stderr.write("port_number is an odd number (i.e., reserved):" +
                         str(port_number) + "\n")


    return port_number

#-----------------------------------------------------------------------------
def getNextAvailablePort(host, ports_dict, hosts_dict, baseport):
    '''
    Returns the next available port
    '''
#    global cl_baseport
    
    ret_val = None

    if baseport != 0:
        min_tcp = baseport*100 + 3000 + 50
        max_tcp = baseport*100 + 3000 + 100
    else:
        min_tcp = 4000
        max_tcp = 5000
    
    #ignore odd-numbered ports
    for i in range(min_tcp,
                   max_tcp,
                   2):
        
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
    global container_file
    
    usage_msg='''
This script is used to generate a command-line to be passed to container 
startup scripts. It will include such information as the TCP port the 
container should be run under. Do not call this script directly from your own
code!.
'''
    parser = OptionParser(usage=usage_msg)
    
    #end-users can specify a specific port number to run under 
    port_help_msg='''
TCP port to run the container under.
If this integer value is in the inclusive range of 0-24, it is assumed
that the intended TCP port is really an offset equivalent to the 
following:   REAL TCP PORT = port*2 + 3050 + $ACS_INSTANCE*100.
If this integer value is greater than 24, the TCP port is used as
provided. The only stipulation to this is that odd TCP port numbers
from 3000-4000 are not available. These are reserved exclusively for
so-called remote debuggable containers (Java-only). Also, it is important
to note that $ACS_INSTANCE==0 is a special case in which ports 4000-5000
are used implying a total number of 500 allowable containers.
'''
    parser.add_option("--port",
                      dest="port",
                      help=port_help_msg)
    
    #end-users can use this parameter to specify the name of the container
    name_help_msg='''
Name of the container.
Users can optionally specify the name of the container directly using
this command-line switch. If this switch is not used, it is assumed the
first argument after all command-line switches is the name of the
container.
'''
    parser.add_option("--name",
                      dest="name",
                      help=name_help_msg)
    
    #Java containers
    parser.add_option("--java",
                      action="store_true",
                      dest="java",
                      default=0,
                      help="Specifies the container is Java.")
    
    #Alternative Java container class
    parser.add_option("--custom_java",
                      dest="custom_container",
                      default="alma.acs.container.AcsContainerRunner",
                      help="Alternative Java class to use for the container implementation.")
    
    #C++ containers
    parser.add_option("--cpp",
                      action="store_true",
                      dest="cpp",
                      default=0,
                      help="Specifies the container is C++.")
    
    #Python containers
    parser.add_option("--py",
                      action="store_true",
                      dest="py",
                      default=0,
                      help="Specifies the container is Python.")
    
    #Debug flag
    parser.add_option("--debug",
                      action="store_true",
                      dest="debug",
                      default=0,
                      help="Prints out special debugging info to standard error.")
    
    #Reference to manager.
    mgr_help_msg='''
Set's manager's reference.
Tells the container where its manager is.
E.g., corbaloc::127.0.0.1:3100/Manager
'''
    parser.add_option("-m", "--managerReference",
                      dest="manager_reference",
                      help=mgr_help_msg)
    
    #Reference to the CDB.
    cdb_help_msg='''
Sets a reference to the configuration database.
Tells the container where to find its own configuration data.
This is currently only applicable to C++ containers.
E.g., corbaloc::127.0.0.1:3012/CDB
'''
    parser.add_option("-d", "--DALReference",
                      dest="cdb_ref",
                      help=cdb_help_msg)
    
    #Custom executable
    exe_help_msg='''
Sets a custom executable to be run.
The value given here replaces the standard command to start a 
container in a particular programming language. E.g., you
might want to replace the C++ executable, maciContainer, 
with some specialized container you wrote - myMaciContainer.
'''
    parser.add_option("-e", "--executable",
                      dest="executable",
                      help=exe_help_msg)
    
    #Remote host
    remote_help_msg='''
Sets a remote PC to run the container under.
When this flag is given, the container is run under the host specified
from the command-line rather than the localhost. Minimal checks are
performed to ensure the remote host actually exists and is reachable.
For this to work, it is necessary that SSH be configured in such a way
that $USER can ssh into the remote host without being asked for a 
password.
'''
    parser.add_option("--remoteHost",
                      dest="remote_host",
                      help=remote_help_msg)
    
    remote_debug_msg='''
Makes the Java container accessible by a remote debugger. 
Deprecated. The remote debuggable Java container feature is now activated
by setting $ACS_LOG_SDTOUT less than or equal to the DEBUG log level.
'''
    parser.add_option("--remoteDebuggable",
                      dest="remote_debuggable",
                      action="store_true",
                      default=0,
                      help=remote_debug_msg)
    
    #baseport
    baseport_help_msg='''
ACS baseport (i.e., 0-9).
Setting this flag overrides the value of $ACS_LOG_STDOUT.
'''
    parser.add_option("-b", "--baseport",
                      dest="baseport",
                      help=baseport_help_msg,
                      default=None)
    
    #Container flags
    flags_help_msg='''Sets flags that are to be passed to the actual container
    start executables.'''
    parser.add_option("-p", "--passthrough",
                      dest="container_flags",
                      default=None,
                      help=flags_help_msg)
    
    #Start Options
    flags_help_msg='''Sets options that are to be passed to the actual container
    start executables.'''
    parser.add_option("--passthroughProcessStart",
                      dest="start_options",
                      default=None,
                      help=flags_help_msg)
    
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
    cl_java              = options.java
    cl_java_container    = options.custom_container
    cl_cpp               = options.cpp
    cl_py                = options.py
    cl_remote_host       = options.remote_host
    cl_remote_debuggable = options.remote_debuggable
    cl_cdb_ref           = options.cdb_ref
    cl_baseport          = options.baseport
    cl_flags             = options.container_flags
    cl_start_options     = options.start_options
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
    
    #--set manager's reference
    if options.manager_reference==None:
        if environ.has_key('MANAGER_REFERENCE'):
            #Found it!  OK to set now.
            cl_manager = environ['MANAGER_REFERENCE']
        else:
            #Assume manager is running under the local machine
            manager_port = cl_baseport*100 + 3000 + 0
            cl_manager = 'corbaloc::' + str(getIP()) + ':' + str(manager_port) + '/Manager'
    else:
        cl_manager = options.manager_reference
    
    #--set the custom executable
    if options.executable==None:
        if cl_java:
            cl_executable = "acsStartJavaContainer"
        elif cl_cpp:
            cl_executable = "maciContainer"
        elif cl_py:
            cl_executable = "ACSStartContainerPy"
        else:
            stderr.write("Unable to continue. No container type specified!\n")
            return 3
    else:
        cl_executable = options.executable
       
    #--if the remote host is set, setup the command to reflect this
    if cl_remote_host != None:
        container_host = cl_remote_host
        cl_executable = "ssh -f " + environ['USER'] + "@" + cl_remote_host + " " + cl_executable
        
        #sanity check to ensure this host exists
        stuff = subprocess.Popen('ping -c 1 ' + container_host,
                                 shell=True,
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE).stderr.read()
    
        if ('unknown host' in stuff) or ('request timed out' in stuff):
            stderr.write("The remote host, " + container_host +
                         ", is not accessible from this machine!\n")
            return 1
    
    #just use the local address
    else:
        container_host = getIP()
    
    #no container name specified
    if cl_name==None:
        if len(parsed_argv)==0:
            stderr.write("Must specificy a container name using the -name option!\n")
            return 5
        #error is recoverable - container name specified as a general argument
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
    
    #--------------------------------------------------------------------
    #--GLOBALS-----------------------------------------------------------
    
    #get the file which shows us which ports are currently taken up
    container_file = getPortsFile(cl_baseport)
    
    #get dictionaries mapping container names to TCP port numbers and hosts
    container_ports, container_hosts = getContainerDict(container_file)
    
    #--------------------------------------------------------------------
    #--SANITY CHECKS-----------------------------------------------------
    
    #multiple container types specified
    if (cl_java and cl_cpp) or (cl_java and cl_py) or (cl_cpp and cl_py):
        stderr.write("Too many -java/-cpp/-py options supplied!\n")
        stderr.write("Choose only one and try again!\n")
        return 6
    
    #no container type specified
    if not(cl_java or cl_cpp or cl_py):
        stderr.write("You must choose some container type using one of the -java/-cpp/-py options!\n")
        return 7
    
    #recorded host is different from commanded host
    if container_hosts.has_key(cl_name) and container_hosts[cl_name]!=container_host:
        stderr.write("Trying to run the '" + cl_name + "' container on '" +
                      container_host + "' instead of '" + container_hosts[cl_name] +
                      "' impossible!\n")
        stderr.write("Changing hosts after a container is restarted is not permitted!\n")
        return 8
        
    #recorded TCP port requested is different from commanded TCP port
    if newPort!=None and container_ports.has_key(cl_name) and container_ports[cl_name]!=newPort:
        stderr.write("Trying to change the '" + cl_name + "' container's TCP port from '" +
                      str(container_ports[cl_name]) + "' to '" + str(newPort) +
                      "' is impossible!\n")
        stderr.write("Changing TCP ports after a container is restarted is not permitted!\n")
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
        stderr.write("Command-line container name:" + str(cl_name) + "\n")
        stderr.write("New args:" + str(parsed_argv) + "\n")
    
        if cl_java:
            stderr.write("Dealing with a Java container\n")
            stderr.write("Container implementation class:" + 
                          str(cl_java_container) + "\n")
        elif cl_cpp:
            stderr.write("Dealing with a C++ container\n")
        elif cl_py:
            stderr.write("Dealing with a Python container\n")
        else:
            stderr.write("Unknown container type. This is very bad!\n")
    
        stderr.write("Manager corbaloc is:" + str(cl_manager) + "\n")
        stderr.write("Executable to be run is:" + str(cl_executable) + "\n")
        stderr.write("Remote host to run the container under is:" + 
                      str(cl_remote_host) + "\n")
        stderr.write("Remote debuggable is:" + str(cl_remote_debuggable) + "\n")

#-----------------------------------------------------------------------------
#--MAIN-----------------------------------------------------------------------
    #see if it's been set before...
    temp_port, temp_host = getExistingPort(cl_name, 
                                           container_ports, 
                                           container_hosts)

    #container already had a port assigned to it. fine.
    if temp_port != None:
        #overwrite any value the user may have tried to specify using
        #the port switch. if it's been set before, it must remain the same.    
        newPort = temp_port
        container_host = temp_host
    
        #if this container is being run for the first time and
        #the user doesn't care which port it runs on
    elif newPort is None:
        #just get the next available TCP port. This will always
        #work unless there are 25+ containers
        newPort = getNextAvailablePort(container_host,
                                       container_ports, container_hosts, cl_baseport)
   
    #container does not already have a port assigned to it and the 
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

    ################################################################
    #generate a new commandline argument

    i = 0

    #name of the executable
    parsed_argv.insert(i, cl_executable)
    i = i + 1

    #name of the container
    parsed_argv.insert(i, cl_name)
    i = i + 1

    #add any container flags passed
    if cl_start_options:
        #parsed_argv.insert(i, "--passthroughProcessStart")
        #i = i + 1
        parsed_argv.insert(i, cl_start_options)
        i = i + 1
        
    #figure out the correct language first
    if cl_java:
        parsed_argv.insert(i, cl_java_container)
        i = i + 1

        parsed_argv.insert(i, "-OAIAddr")
        i = i + 1
    
        parsed_argv.insert(i, container_host)
        i = i + 1
    
    
        parsed_argv.insert(i, "-OAport")
        i = i + 1
    
        parsed_argv.insert(i, str(newPort))
        i = i + 1

    elif cl_cpp:
        parsed_argv.insert(i, "-ORBEndpoint")
        i = i + 1
    
        parsed_argv.insert(i, "iiop://" + container_host + ':' + str(newPort))
        i = i + 1

        if cl_cdb_ref!=None:
            stderr.write("This container will use " +
                         str(cl_cdb_ref) +
                         " for the CDB.\n")
            parsed_argv.insert(i, "-d " + str(cl_cdb_ref))
            i = i + 1
    
    elif cl_py:
        parsed_argv.insert(i, "-ORBendPoint")
        i = i + 1
    
        parsed_argv.insert(i, "giop:tcp:" + container_host + ':' +  str(newPort))
        i = i + 1
    
    #ACS 2.1 usage retained for backwards compatiablity
    else:
        stderr.write("ACS 2.1 usage of this script deprecated and removed\n")
        return 1

    #add the manager reference
    parsed_argv.insert(i, "-m " + str(cl_manager))
    i = i + 1

    #add any container flags passed
    if cl_flags:
        parsed_argv.insert(i, cl_flags)
        i = i + 1
        
    ################################################################    
    #Finally print out the command.
    for arg in parsed_argv:
        print arg,

    print
    ################################################################
    return 0

if __name__ == '__main__':
    atexit.register(cleanUp)
    exit(main(argv[1:]))
