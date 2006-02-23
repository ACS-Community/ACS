#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsstartupRemovePID.py,v 1.5 2005/06/07 00:12:55 dfugate Exp $
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
This script is designed to strip a PID from a given file.  This could just have easily been done
in bash but there would be no guarantee that two processes do not try to access the same file at
the same time.

There are two parameters passed to this script: fileName and PID.  fileName is the name of the file
that we want to strip instances of PID from.  A sample usage is "acstartupRemovePID ACS_PIDS 3000".
'''
################################################################################################
from os      import access, R_OK, W_OK, X_OK, F_OK
from os.path import exists
from sys     import argv, exit
from time    import sleep
################################################################################################
__DEBUG__=0
################################################################################################
if  __name__!="__main__":
    #pydoc -w is running this script!
    from sys import exit
    print "This script should not be imported as a module!"
    exit(0)
################################################################################################
fileName = argv[1]
pID      = argv[2]

#make sure the file exists
if not exists(fileName):
    print "Error - cannot strip instances of '", pID, "' when file '", fileName, "' does not exist!"
    exit(1)

#make sure the user actually owns this ACS instance and it's writable
if not access(fileName, R_OK & W_OK & X_OK & F_OK):
    print "Error - cannot strip instances of '", pID, "' when file '", fileName, "' is not accessible by this user!"
    exit(1)

#old list of ports
usedPorts = []

#it's possible this file is being modified so we must do it this way.
for i in range(15):
    try:
        usedPortsFile = open(fileName, 'r')
        break
    except:
        sleep(1)
        if __DEBUG__:
            print "File '", fileName, "' is not accessible..."

    if i==14:
        print "Error - file '", fileName, "' is not accessible..."
        exit(1)

#build up the list of ports that have already been used
lines = usedPortsFile.readlines()
if __DEBUG__:
    print "All lines before modifications are:", lines
    
#make sure we don't end up with an empty list
if lines == []:
    if __DEBUG__:
        print "File '", fileName, "' is empty..."
    usedPortsFile.close()
    exit(0)
        
#strip the PID we're looking for
i = 0
while i < len(lines):
    
    # split the line into single parts
    parts = lines[i].split()
    
    if __DEBUG__:
        print "split line is:", parts
        
    # check if any of the single parts matches the search string
    match = 0
    j = 0
    while j < len(parts):
        part = parts[j]

        if pID[len(pID)-1] == '*':
            match = (part[0:len(pID)-1] == pID[0:len(pID)-1])
        else:
            match = (part == pID)

        if match:
            break
        else:
            j = j +1

    # if any part of the line matched, remove the line	
    if match:
        temp = lines.pop(i)
        if __DEBUG__:
            print "Removed instance of:", temp
    else:
        i = i + 1

if __DEBUG__:
    print "All lines after modification are:", lines

#close the file and immediately overwrite it
usedPortsFile.close()
usedPortsFile = open(fileName, 'w')
usedPortsFile.writelines(lines)
usedPortsFile.close()
    
