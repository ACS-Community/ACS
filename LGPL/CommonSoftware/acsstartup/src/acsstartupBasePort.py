#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsstartupBasePort.py,v 1.9 2005/07/18 23:46:35 dfugate Exp $
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
This script is designed to pick a base port for for ACS to run on.  If theres a free port to run
under, it prints that to standard out.
'''
################################################################################################
from os import environ, listdir, mkdir, system
import sys
################################################################################################
if  __name__!="__main__":
    #pydoc -w is running this script!
    from sys import exit
    print "This script should not be imported as a module!"
    exit(0)
################################################################################################
basePort = None
try:
    tempList = listdir(str(environ['ACSDATA']) + '/tmp')
except:
    exit(1)

# Look for $ACS_INSTANCE first
if environ.has_key('ACS_INSTANCE'):

    #Get the developer-defined base tcp port
    basePort = environ['ACS_INSTANCE']

    try:
        #make sure it's actually a number
        basePort = long(basePort)

        #make sure it's within range
        if (basePort<0) or (basePort>9):
            basePort = None

        #make sure there isn't an ACS instance using this baseport
        #already running
        for directory in tempList:
            if directory == 'ACS_INSTANCE.' + str(basePort):
                #the port is already being used
                basePort = None
                
    except:
        basePort = None

#either $ACS_INSTANCE was not defined in the environment or was defined incorrectly.
#in either case, we must now find a basePort that does work
if basePort==None:

    #iterate through all valid TCP port offsets
    for i in range(10):

        #a true value means that the basePort is not being used
        OKtoReturn = 1
        
        #check to make sure the TCP port is not currently being used
        for directory in tempList:
            if directory == 'ACS_INSTANCE.' + str(i):
                OKtoReturn = 0

        if OKtoReturn:
            basePort=i
            break

#one final check
if basePort!=None:
    try:
        mkdir(str(environ['ACSDATA']) + '/tmp/ACS_INSTANCE.' + str(basePort))
        system('touch ' + str(environ['ACSDATA']) + '/tmp/ACS_INSTANCE.' + str(basePort) + '/USED_CONTAINER_PORTS')
        system ('chmod 774 ' + str(environ['ACSDATA']) + '/tmp/ACS_INSTANCE.' + str(basePort) + '/USED_CONTAINER_PORTS')
        print basePort
    except:
        sys.stderr.write("==> ERROR - $ACSDATA/tmp/ does not exist or $USER does not have write permissions on it.")
        print "==> ERROR - $ACSDATA/tmp/ does not exist or $USER does not have write permissions on it."
        pass
else:
    sys.stderr.write("\n==> ERROR - no free ACS_INSTANCE.x directories in $ACSDATA/tmp/")
    print "==> ERROR - no free ACS_INSTANCE.x directories in $ACSDATA/tmp/"
