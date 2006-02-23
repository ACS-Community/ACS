#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsdataClean.py,v 1.5 2005/12/07 19:08:59 dfugate Exp $
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
This script is designed to clean-up $ACSDATA/tmp for modular tests.
Normally ACS_INSTANCE directories are NOT cleaned up.
The -a or -all command line option forces ALL files and directories
to be cleaned up
'''
################################################################################################
from os import environ
from os import chdir
from os import listdir
from os import remove, system

from os.path import exists
from os.path import isfile, isdir

from sys import argv

#------------------------------------------------------------------------------
__version__ = "$Id: acsdataClean.py,v 1.5 2005/12/07 19:08:59 dfugate Exp $"
#------------------------------------------------------------------------------

print "Cleaning up ACS temporary directories"

################################################################################################
# Check from the command line if -a (-all) option si give and ALL
# directories have to be cleaned up
safeEntries = {}

if argv.count("-all")==0 and argv.count("-a")==0:
   #these files/directories will NOT be removed.
   safeEntries = {
       'ACS_INSTANCE.0'       : '',
       'ACS_INSTANCE.1'       : '',
       'ACS_INSTANCE.2'       : '',
       'ACS_INSTANCE.3'       : '',
       'ACS_INSTANCE.4'       : '',
       'ACS_INSTANCE.5'       : '',
       'ACS_INSTANCE.6'       : '',
       'ACS_INSTANCE.7'       : '',
       'ACS_INSTANCE.8'       : '',
       'ACS_INSTANCE.9'       : '',
       'USED_CONTAINER_PORTS' : ''
    }
   print "Safe directories (i.e., ACS_INSTANCE.*'s) and a few files used by ACS will be"
   print "preserved. To remove everything, provide the '-all' switch to this script."
else:
   print "Cleaning up ALL directories"
   print "It is ONLY SAFE and recommended to do this after a machine has been rebooted or"
   print "killACS has been run! Use at your own risk."

################################################################################################
try:
    myDir = str(environ['ACSDATA']) + '/tmp'

    #make sure acs temp exists
    if not exists(myDir):
        print "'", myDir, "' does not exist!"
        exit(1)

    #get a list of all entries in it.
    tempList = listdir(myDir)

    #move to it
    chdir(myDir)

    #iterate through every entry
    for entry in tempList:
        #if OK to remove...
        if (not safeEntries.has_key(entry)) and (isdir(entry)):
            try:
                system("rm -rf " + entry)
            except:
                print "Unable to remove this directory:", entry
                
        elif (not safeEntries.has_key(entry)) and (isfile(entry)):
            try:
                remove(entry)
            except:
                print "Unable to remove this file:", entry  

except Exception, e:
    print "An exception occured in acsdataClean.py's main:", e
