#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsstartupContainerPort.py,v 1.26 2006/05/23 23:45:07 dfugate Exp $
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
################################################################################################
'''
This script is designed to pick a free container port for ACS to run on using ALL the arguments
passed to the container. If theres a free port to run under, it prints that to standard out.
'''
################################################################################################
from os      import environ
from os      import chdir
from os      import chmod
from os      import system
from os      import access, R_OK, W_OK, X_OK, F_OK
from os.path import exists
from os      import getlogin
from fcntl   import flock
from fcntl   import LOCK_EX
from sys import exit
from sys import stderr
from sys import argv

from optparse import OptionParser

from AcsutilPy.ACSPorts import getIP
#-----------------------------------------------------------------------------------------------

parser = OptionParser(usage="This script is used to generate a command-line to be passed to container startup scripts. It will include such information as the TCP port the container should be run under. Do not call this script from your own code!.")

parser.add_option("--port",
                  dest="port",
                  help="TCP port to run under.")

parser.add_option("--name",
                  dest="name",
                  help="TCP port to run under.")

parser.add_option("--java",
                  action="store_true",
                  dest="java",
                  default=0,
                  help="Specifies the container is Java.")

parser.add_option("--custom_java",
                  dest="custom_container",
                  default="alma.acs.container.AcsContainerRunner",
                  help="Alternative Java class to use for the container implementation.")

parser.add_option("--cpp",
                  action="store_true",
                  dest="cpp",
                  default=0,
                  help="Specifies the container is C++.")

parser.add_option("--py",
                  action="store_true",
                  dest="py",
                  default=0,
                  help="Specifies the container is Python.")

parser.add_option("--debug",
                  action="store_true",
                  dest="debug",
                  default=0,
                  help="Prints out special debugging info.")

parser.add_option("-m", "--managerReference",
                  dest="manager_reference",
                  help="Sets manager's reference (i.e., corbaloc).")

parser.add_option("-d", "--DALReference",
                  dest="cdb_ref",
                  help="Sets a reference to the configuration database (i.e., corbaloc).")

parser.add_option("-e", "--executable",
                  dest="executable",
                  help="Sets a custom executable to run for the container.")

parser.add_option("--remoteHost",
                  dest="remote_host",
                  help="Sets a remote PC to run the container under.")

parser.add_option("--remoteDebuggable",
                  dest="remote_debuggable",
                  action="store_true",
                  default=0,
                  help="Makes the java container accessible by a remote debugger. Deprecated.")

parser.add_option("-b", "--baseport",
                  dest="baseport",
                  help="ACS baseport (i.e., 0-9).",
                  default=None)

#--------------------------------------------------------------------------
#--Make commands backwards compatible with pre ACS 6.0 usage.

#strictly for debugging purposes...
if 0:
    stderr.write("Original argv was:" + str(argv) + "\n")

#go through every potential command-line switch
for i in range(1, len(argv)):
    #if the switch is something like -managerReference change it
    #to --managerReference
    if len(argv[i])>2 and argv[i][0]=='-' and argv[i][1]!='-':
        argv[i] = "-" + argv[i]

#--------------------------------------------------------------------------
#parse everything
(options, parsed_argv) = parser.parse_args()

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
__DEBUG__            = options.debug


if cl_baseport==None:
    cl_baseport = int(environ['ACS_INSTANCE'])
else:
    cl_baseport=int(cl_baseport)

#set manager reference
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

manager_port = cl_manager.split(":")[3].split("/")[0]

#set the custom executable
if options.executable==None:
    if cl_java:
        cl_executable = "acsStartJavaContainer"
    elif cl_cpp:
        cl_executable = "maciContainer"
    elif cl_py:
        cl_executable = "ACSStartContainerPy"
    else:
        stderr.write("Unable to continue. No container type specified!\n")
else:
    cl_executable = options.executable
    
if cl_remote_host != None:
    cl_executable = "ssh -f " + getlogin() + "@" + cl_remote_host + " " + cl_executable
    container_host = cl_remote_host
else:
    container_host = getIP()

#--------------------------------------------------------------------
#--Run through a few sanity checks
if (cl_java and cl_cpp) or (cl_java and cl_py) or (cl_cpp and cl_py):
    stderr.write("Too many -java/-cpp/-py options supplied!\n")
    stderr.write("Choose only one and try again!\n")
    exit(1)

elif not(cl_java or cl_cpp or cl_py):
    stderr.write("You must choose some container type using one of the -java/-cpp/-py options!\n")
    exit(1)

if cl_name==None:
    
    if len(parsed_argv)==0:
        stderr.write("Must specificy a container name using the -name option!\n")
        exit(1)
    else:
        cl_name = parsed_argv.pop(0)
        
if __DEBUG__:
    stderr.write("Command-line TCP port:" + str(cl_port) + "\n")
    stderr.write("Command-line container name:" + str(cl_name) + "\n")
    stderr.write("New args:" + str(parsed_argv) + "\n")

    if cl_java:
        stderr.write("Dealing with a Java container\n")
        stderr.write("Container implementation class:" + str(cl_java_container) + "\n")
    elif cl_cpp:
        stderr.write("Dealing with a C++ container\n")
    elif cl_py:
        stderr.write("Dealing with a Python container\n")
    else:
        stderr.write("Unknown container type. This is very bad!\n")

    stderr.write("Manager corbaloc is:" + str(cl_manager) + "\n")
    stderr.write("Executable to be run is:" + str(cl_executable) + "\n")
    stderr.write("Remote host to run the container under is:" + str(cl_remote_host) + "\n")
    stderr.write("Remote debuggable is:" + str(cl_remote_debuggable) + "\n")

#--------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------
#--Functions
def getPortsFile():
    '''
    Returns the file containing a list of containers and used ports
    '''
    
    #initialize the return value
    ret_val = None
    
    #directory where all the process IDs of this particular instance of ACS are stored
    ACS_INSTANCE_DIR = str(environ['ACSDATA']) + '/tmp/ACS_INSTANCE.' + str(cl_baseport)

    #make sure the acs instance directory exists
    if not exists(ACS_INSTANCE_DIR):
        #assume everything is running remotely
        ACS_INSTANCE_DIR = str(environ['ACSDATA']) + '/tmp'
        if not exists(ACS_INSTANCE_DIR):
            stderr.write("ERROR ==> $ACSDATA/tmp does not exist!\n")
            exit(1)
            
    #make sure the user actually has write access to this ACS instance
    elif not access(ACS_INSTANCE_DIR, R_OK & W_OK & X_OK & F_OK):
        stderr.write("ERROR ==> The ACS instance in '" +
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
    
    #open the file
    ret_val = open('USED_CONTAINER_PORTS', 'r+')
    
    #lock it
    flock(ret_val.fileno(), LOCK_EX)
    
    return ret_val

#-----------------------------------------------------------------------------------------------
def getPortsDict(portsFile):
    '''
    Returns a dictionary where each key is the name of a container and the value is the absolute
    TCP port number it should use.

    Params:
    - portsFile A file which contains "someContainerName 1234" on each line where 1234 is a TCP
    port number.

    Returns: a dictionary where each key is the name of a container and the value is the absolute
    TCP port number it should use
    '''
    #return value
    ret_val = {}
    
    #build up the list of ports that have already been used
    lines = portsFile.readlines()
    
    #make sure we don't end up with an empty list
    if lines == []:
        lines.append('xxx 0')
        
    for line in lines:  #line = 'someContainerName portnumber'
        container_name = line.split(' ')[0]
        port_number    = int(line.split(' ')[1])
        
        ret_val[container_name] = port_number

    return ret_val

#-----------------------------------------------------------------------------------------------
def getExistingPort(container_name, port_dict):
    '''
    If the port dictionary already has an entry stating that this container should be using some
    port, returns that port. If not, returns None.
    '''
    
    if port_dict.has_key(container_name):
        return port_dict[container_name]
    else:
        return None

#-----------------------------------------------------------------------------------------------
def portNumberAlreadyUsed(port_number, port_dict):
    '''
    Function returns 1 if the port number is already in use and false otherwise.
    DWF-this should also check the output of "netstat -l" command.
    '''
    
    #if no other entries are using this port number it's completely safe to use it
    if (port_dict.values().count(port_number)==0):
        return 0
        
    else:
        if __DEBUG__:
            stderr.write("Port for -port option is already being used:" +
                         str(port_number) + "\n")
        return 1

#-----------------------------------------------------------------------------------------------
def coercePortNumber(port_number):
    '''
    This helper function takes a port number which can be in string or integer format and converts
    it to an integer. In the event of any failure, it returns None. Also, in the case that the
    port number is set to be an offset from the ACS_INSTANCE, this method handles that as well.
    
    Params: port_number port number. Can be absolute (i.e., "3075") or dynamic (i.e., "0"-"24").
    
    Returns: the port number in integer format or None if there was some sort of failure
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
#-----------------------------------------------------------------------------------------------
def getPortFromArgv(ports_dict, cl_port):
    '''
    Returns a port value extracted from the command-line if provided and valid.
    '''
    
    retVal = None
    
    #sanity check
    if cl_port == None:
        return retVal
    
    elif __DEBUG__:
        stderr.write("-port option was specified:" + str(cl_port) + "\n")

    #try to coerce the cl_port number into a proper TCP port number
    retVal = coercePortNumber(cl_port)
    #if what's above fails, just return None
    if retVal == None:
        return retVal
    
    #cycle through all the used ports to ensure developer
    #hasn't picked something that's already being used
    if portNumberAlreadyUsed(retVal, ports_dict):
        retVal = None
        
    return retVal

#-----------------------------------------------------------------------------------------------
def getNextAvailablePort(ports_dict):
    '''
    Returns the next available port
    '''
    
    ret_val = None
    
    #ignore odd-numbered ports
    for i in range(cl_baseport*100 + 3000 + 50,
                   cl_baseport*100 + 3000 + 100,
                   2):
        
        #found a port that can be used
        if portNumberAlreadyUsed(i, ports_dict) == 0:
            ret_val = i
            break

    return ret_val

#-----------------------------------------------------------------------------------------------
#--GLOBALS

try:
    newPort = int(cl_port)
except:
    newPort = None

containerName = cl_name

#get the file which shows us which ports are currently taken up
usedPortsFile = getPortsFile()

#get a dictionary mapping container names to TCP port numbers
usedContainerPortsDict = getPortsDict(usedPortsFile)


#see if it's been set before...
temp_port = getExistingPort(containerName, usedContainerPortsDict)

if temp_port != None:
    #overwrite any value the user may have tried to specify using
    #the port switch. if it's been set before, it must remain the same.
    if newPort!=None and newPort != temp_port:
        stderr.write("Warning - you're trying to change '" +
                     str(containerName) + "'s port from " +
                     str(temp_port) +
                     " to " + str(newPort) + "\n")
        
    newPort = getExistingPort(containerName, usedContainerPortsDict)
    usedPortsFile.close()
    
#try to set it from argv
elif newPort!=None:
    newPort = getPortFromArgv(usedContainerPortsDict, newPort)

#if this container is being run for the first time and
#the user doesn't care which port it runs on
if newPort==None:
    #just get the next available TCP port. This will always
    #work unless there are 25+ containers
    newPort = getNextAvailablePort(usedContainerPortsDict)

#at this point we should have found a free port number.
#we can now close up the usedPortsFile so that other containers
#can be started immediately.
if newPort==None:
    #could not find a free port. no point in going on.
    stderr.write("ERROR ==> All ports are taken!\n")
    usedPortsFile.close()
    exit(1)
    
if getExistingPort(containerName, usedContainerPortsDict)==None:
    #close up the file so other containers can be started.
    #no need to write anything to disk because we're using
    #a predefined port number
    usedPortsFile.writelines([containerName + ' ' + str(newPort) + '\n'])
    usedPortsFile.close()

################################################################
#generate a new commandline argument

i = 0

#name of the executable
parsed_argv.insert(i, cl_executable)
i = i + 1

#name of the container
parsed_argv.insert(i, containerName)
i = i + 1

#figure out the correct language first
if cl_java:
    parsed_argv.insert(i, cl_java_container)
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
    exit(1)

#add the manager reference
parsed_argv.insert(i, "-m " + str(cl_manager))
i = i + 1
    
################################################################    
#Finally print out the command.
for arg in parsed_argv:
    print arg,

print
################################################################
