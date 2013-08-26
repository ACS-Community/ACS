#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsGetSpecificJars.py,v 1.1 2006/09/01 09:34:07 gchiozzi Exp $
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
This is a helper script to be used by acsStartJava to obtain the exact locations of jars files
(passed from the command-line) within the ALMA SE-prescribed directory structure (i.e., ../,
$INTROOT, $ACSROOT) by using the standard acsFindFile file search routine.

Parameters:
- the first parameter to this script is the separator to be used between the jar files (when passed
to the JVM). Under UNIX, this is normally ":" and in Windows it is ";"
- the second parameter is a 'separator' separated list of directories to be searched for jar files. It does not
matter whether the directories actually exist or not (the script will figure this out on its own).

Sample usage of this script could be something like:
     acsGetSpecificJars : baci.jar:acsnc.jar:someOtherJar.jar

Assumptions:
- 

TODO:
- 
'''
################################################################################################
from sys                 import argv
from os                  import listdir
from os.path             import abspath, basename
from AcsutilPy.FindFile  import findFile
################################################################################################
if  __name__!="__main__":
    #pydoc -w is running this script!
    from sys import exit
    print "This script should not be imported as a module!"
    exit(0)
################################################################################################
#this should be : or ;. In any event, it's the first parameter to this script used as the
#classpath seperator
cpsep = argv[1]

#to be a list of all jar files in the directories specified from the command-line
retString = ""

#for each jar specified in the command-line
for jar in argv[2].split(cpsep):
    #look up the location
    location = findFile("lib/" + jar)[0]

    #if it's valid; add it
    if location != "":
        retString = retString + location + cpsep

################################################################################################
#print the $CLASSPATH to standard out
print retString
