#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsGetAllJars.py,v 1.1 2006/09/01 09:34:07 gchiozzi Exp $
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
This is a helper script to be used by acsStartJava to obtain a list of all jar files in directories
specified as parameters to this script. Once it finds those jar files, it prints them to standard
out in a format suitable for use with a JVM.

Parameters:
- the first parameter to this script is the separator to be used between jar files (when passed
to the JVM). Under UNIX, this is normally ":" and in Windows it is ";"
- the second parameter is a 'separator' separated list of directories to be searched for jar files. It does not
matter whether the directories actually exist or not (the script will figure this out on its own).
Do be careful with the order the directories are passed though because if duplicate jar files
are found, only the jar file found in the first directory specified will be passed to the JVM.

Sample usage of this script in the ALMA directory structure should be:
     acsGetAllJars : ../lib:$INTROOT/lib:$ACSROOT/lib

Assumptions:
-

TODO:
-

'''
################################################################################################
from sys     import argv
from os      import listdir
from os.path import abspath, basename
import string
################################################################################################
if  __name__!="__main__":
    #pydoc -w is running this script!
    from sys import exit
    print "This script should not be imported as a module!"
    exit(0)
################################################################################################
def addJar(file, dir, list):
    '''
    Adds a jar file to the list parameter is the file parameter actually specifies a jar and
    the jar does not currently exist in the list parameter.

    Paramters:
    file - a file name
    dir - the directory the file was found in
    list - a Python list where each element in the full location of a jar file

    Returns: Nothing

    Raises: ???
    '''
    #if it's not even a jar file just return
    if not file.endswith(".jar"):
        return
    #add it only if it does not already exist in the list
    elif map(basename, list).count(file)==0:
        list.append(dir+file+cpsep)
################################################################################################
#a list of all jar files in the directories specified from the command-line (not yet though)
retList = []

#this should be : or ;. In any event, it's the first parameter to this script used as the
#classpath seperator
cpsep = argv[1]

#for each directory specified in the command-line
for dir in string.split(argv[2],cpsep):
    #create a list of jars
    try:
        #attempt to get a list of all files in the directory
        dir = abspath(dir) + '/'
        tempList = listdir(dir)
    except:
        #if it fails...the directory does not really exist. so what. just continue with
        #the next directory specified
        continue
    
    #for each file in the directory attempt to add it.
    for file in tempList:
        addJar(file, dir, retList)
################################################################################################
#print the $CLASSPATH to standard out
retString = ""
for element in retList:
    retString = retString + element

print retString
