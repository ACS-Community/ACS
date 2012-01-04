#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsdataClean.py,v 1.8 2012/01/04 00:06:26 tstaig Exp $
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
This script is designed to clean-up $ACS_TMP for modular tests.
Normally ACS_INSTANCE directories are NOT cleaned up.
The -a or -all command line option forces ALL files and directories
to be cleaned up
'''

# This script is part of LGPL/CommonSoftware/acsstartup module

################################################################################################
from os import environ
from os import chdir
from os import listdir
from os import remove, system

from os.path import exists
from os.path import isfile, isdir, islink

from sys import argv

import socket

from AcsutilPy.ACSDirectory import getAcsTmpDirectoryPath

#------------------------------------------------------------------------------
__version__ = "$Id: acsdataClean.py,v 1.8 2012/01/04 00:06:26 tstaig Exp $"
#------------------------------------------------------------------------------

################################################################################################
# Check from the command line if -a (-all) option si give and ALL
# directories have to be cleaned up
def cleanDirectory(dir):
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
          'USED_CONTAINER_PORTS' : '',
          '.acs_command_history' : '',
          'ifr_cache.1'         : '',
          'ifr_cache.2'         : '',
          'ifr_cache.3'         : '',
          'ifr_cache.4'         : '',
          'ifr_cache.5'         : '',
          'ifr_cache.6'         : '',
          'ifr_cache.7'         : '',
          'ifr_cache.8'         : '',
          'ifr_cache.9'         : ''
       }
      print "Safe directories (i.e., ACS_INSTANCE.*'s) and a few files used by ACS will be"
      print "preserved. To remove everything, provide the '-all' switch to this script."
   else:
      print "Cleaning up ALL directories"
      print "It is ONLY SAFE and recommended to do this after a machine has been rebooted or"
      print "killACS has been run! Use at your own risk."
   
   ################################################################################################
   try:
       myDir = dir
   
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


if argv.count("-h")!=0 or argv.count("--help")!=0:
   print "Clean the temporal directories of ACS"
   print "Options:"
   print "-h or --help : Show this message and then exit.\n"
   print "-all : acsdataClean will remove all the directory entries under the tmp directory. By default the ACS_INSTANCE.* directories are preserved.\n"
   print "-other_hosts : acsdataClean will clean all the directories located under $ACSDATA/tmp/ this will clean the tmp directories of the other hosts sharing the $ACSDATA in the same machine.\n"
   exit(0)

print "Cleaning up ACS temporary directories"
myDir = getAcsTmpDirectoryPath()
if argv.count("-other_hosts")==0:
   cleanDirectory(myDir)
else:
   print "Cleanning all the possible hosts' temporary directories"
   myDir = myDir + "/../"
   if not exists(myDir):
      print "'", myDir, "' does not exist!"
      exit(1)
   fileList = listdir(myDir)
   for file in fileList:
      if isdir(myDir + file):
         print "Cleanning " + file
         cleanDirectory(myDir + file)

