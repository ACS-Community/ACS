#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsstartupContainerPort.py,v 1.25 2005/11/07 20:45:49 dfugate Exp $
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
from sys     import argv
from time    import sleep
from fcntl   import flock
from fcntl   import LOCK_EX

from AcsutilPy.ACSPorts import getBasePort
from AcsutilPy.ACSPorts import getManagerPort
from AcsutilPy.ACSPorts import getIP
#-----------------------------------------------------------------------------------------------
#if  __name__!="__main__":
#    #pydoc -w is running this script!
#    from sys import exit
#    print "This script should not be imported as a module!"
#    exit(0)
#-----------------------------------------------------------------------------------------------
#--GLOBALS--
#----------------

__DEBUG__ = 0
usedContainerPortsDict = {}
newPort = None
containerName = argv[4]

#-----------------------------------------------------------------------------------------------
#--Functions--
#------------------

def getPortsFile():
    '''
    Returns the file containing a list of containers and used ports
    '''

    #initialize the return value
    ret_val = None
    
    #directory where all the process IDs of this particular instance of ACS are stored
    ACS_INSTANCE_DIR = str(environ['ACSDATA']) + '/tmp/ACS_INSTANCE.' + str(getBasePort())

    #make sure the acs instance directory exists
    if not exists(ACS_INSTANCE_DIR):
        #assume everything is running remotely
        ACS_INSTANCE_DIR = str(environ['ACSDATA']) + '/tmp'
        if not exists(ACS_INSTANCE_DIR):
            print "echo 'ERROR ==> $ACSDATA/tmp does not exist!'"
            exit(1)
            
    #make sure the user actually has write access to this ACS instance
    elif not access(ACS_INSTANCE_DIR, R_OK & W_OK & X_OK & F_OK):
        print "echo 'ERROR ==> The ACS instance in '", ACS_INSTANCE_DIR, "' is not accessible by this user!'"
        exit(1)
    
    #make sure the file full of port numbers exists. if it does not, create it
    if not exists(ACS_INSTANCE_DIR + '/USED_CONTAINER_PORTS'):
        system('touch ' + ACS_INSTANCE_DIR + '/USED_CONTAINER_PORTS')
        system ('chmod 774 ' + ACS_INSTANCE_DIR + '/USED_CONTAINER_PORTS')
        if __DEBUG__:
            print "USED_CONTAINER_PORTS did not exist"

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
            print "Port for -port option is already being used:", port_number
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
        port_number = port_number*2 + 50 + int(getManagerPort())
        if __DEBUG__:
            print "port_number is dynamic:", port_number
            
    #port is OK to use but it's an odd number (reserved)
    if (port_number % 2) == 1:
        port_number = None
        if __DEBUG__:
            print "port_number is an odd number (i.e., reserved):", port_number


    return port_number
#-----------------------------------------------------------------------------------------------
def getPortFromArgv(ports_dict):
    '''
    Returns a port value extracted from the command-line if provided and valid.
    '''
    
    retVal = None
    
    #check command-line args for a port number
    for i in range(0, len(argv) - 1):
        
        #user has defined it...make sure it's free
        if argv[i].lower().find('-port') != -1:
            if __DEBUG__:
                print "-port option was specified"
                
            #coerce it into a port number
            retVal = coercePortNumber(argv[i+1])

            #cycle through all the used ports to ensure developer
            #hasn't picked something that's already being used
            if portNumberAlreadyUsed(retVal, ports_dict):
                retVal = None
                
            else:
                #user-specified port is OK to use.
                break

    return retVal

#-----------------------------------------------------------------------------------------------
def getNextAvailablePort(ports_dict):
    '''
    Returns the next available port
    '''
    
    ret_val = None
    
    #ignore odd-numbered ports
    for i in range(int(getManagerPort())+50, int(getManagerPort())+100, 2):
        
        #found a port that can be used
        if portNumberAlreadyUsed(i, ports_dict) == 0:
            ret_val = i
            break

    return ret_val

#-----------------------------------------------------------------------------------------------
#get the file which shows us which ports are currently taken up
usedPortsFile = getPortsFile()

#get a dictionary mapping container names to TCP port numbers
usedContainerPortsDict = getPortsDict(usedPortsFile)

#try to set it to a preexisting port number for this container defined within the
#ACS_INSTANCE.$ACS_INSTANCE directory
if newPort==None:
    newPort = getExistingPort(containerName, usedContainerPortsDict)

#try to set it from argv
if newPort==None:
    newPort = getPortFromArgv(usedContainerPortsDict)             

#now try to set it by iterating through all possibilities
if newPort==None:
    newPort = getNextAvailablePort(usedContainerPortsDict)


#at this point we should have found a free port number.
#we can now close up the usedPortsFile so that other containers
#can be started immediately.
if newPort==None:
    #could not find a free port. no point in going on.
    print "echo \"ERROR ==> All ports are taken!\""
    exit(1)
    
elif getExistingPort(containerName, usedContainerPortsDict)!=None:
    #close up the file so other containers can be started.
    #no need to write anything to disk because we're using
    #a predefined port number
    usedPortsFile.close()

else:
    #close up the file so other containers can be started.
    #must write out the chosen port number to file.
    usedPortsFile.writelines([containerName + ' ' + str(newPort) + '\n'])
    usedPortsFile.close()

################################################################
#reformat command-line arguments

#get rid of "-port" "someNumber" if it exists
while argv.count("-port"):
    tIndex = argv.index("-port")
    argv.pop(tIndex)  #removes -port
    argv.pop(tIndex)  #removes someNumber

#figure out the correct language first
if argv[1].lower().find('-java') != -1:
    argv.pop(0)  #pop acsstartupContainerPort
    argv.pop(0)  #pop -java 
    argv.insert(0, "-endorsed")
    argv.insert(1, "-Dalma.MicroArchive.DbDir=" + str(environ['ACSDATA']) + "/config/MicroArchive")
    argv.insert(2, "alma.acs.container.AcsContainerRunner")
    argv.insert(3, "-containerName")
    containerName = argv[4]
    argv.insert(5, "-OAport")
    argv.insert(6, str(newPort))

## msc(2004-12-17): we want to allow user-specified executables. for cpp and python this can
## be done in acsStartContainer, for java it must be done here since this script builds the
## command line that acsStartContainer will use.
## solution: i'm smuggling a custom class name into the command line arguments
## passed from acsStartContainer to this script (my major target is avoiding to interfere
## with the order or number of arguments that this script presumes).
##
## TODO: clean up the interweaving of this script and acsStartContainer
##
elif argv[1].lower().find('-customjava:') != -1:
    customclass = argv[1].split(':')[1]
    argv.pop(0)  #pop acsstartupContainerPort
    argv.pop(0)  #pop -customjava 
    argv.insert(0, "-endorsed")
    argv.insert(1, "-Dalma.MicroArchive.DbDir=" + str(environ['ACSDATA']) + "/config/MicroArchive")
    argv.insert(2, customclass)
    argv.insert(3, "-containerName")
    containerName = argv[4]
    argv.insert(5, "-OAport")
    argv.insert(6, str(newPort))
    
elif argv[1].lower().find('-cpp') != -1:
    argv.pop(0)  #remove this script's name
    argv.pop(0)  #argv[0] = "maciContainer"  #remove '-cpp' 
    containerName = argv[0]
    argv.insert(1, "-ORBEndpoint")
    argv.insert(2, "iiop://" + getIP() + ':' + str(newPort))
    #argv.append("&")
    
elif argv[1].lower().find('-py') != -1:
    argv.pop(0)  #remove this script's name
    argv.pop(0)  #argv[0] = "acsStartContainerPy"  #remove '-py' 
    containerName = argv[0]
    argv.insert(1, "-ORBendPoint")
    argv.insert(2, "giop:tcp:" + getIP() + ':' +  str(newPort))
    #argv.append("&")
    
#ACS 2.1 usage retained for backwards compatiablity
else:
    argv.pop(0)  #argv[0] = "acsStartJava"  #remove '-java' 
    argv.insert(0, "-endorsed")
    argv.insert(1, "-Dalma.MicroArchive.DbDir=" + str(environ['ACSDATA']) + "/config/MicroArchive")
    argv.insert(2, "alma.acs.container.AcsContainerRunner")
    argv.insert(3, "-containerName")
    containerName = argv[4]
    argv.insert(5, "-OAport")
    argv.insert(6, str(newPort))
    #argv.append("&")
    
################################################################    
commandLine = ''
for arg in argv:
    commandLine = commandLine + arg + ' '

print commandLine
    

################################################################

