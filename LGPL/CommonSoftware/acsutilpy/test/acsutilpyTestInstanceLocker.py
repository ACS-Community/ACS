#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2014 
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
#
# who       when      what
# --------  --------  ----------------------------------------------
# acaproni  2014-02-10  created
#
from os import environ, listdir

from AcsutilPy.AcsInstanceLockHelper import AcsInstanceLockHelper

def printListOfLockFiles(prefix,suffix):
    '''
    Print the lock files in $ACSDATA/tmp
    
    @param prefix: The prefix of the name of each lock file
    @param sufix:  The suffix of the name of each lock file
    @return The number of lock files found in the folder
    '''
    acsdata=environ["ACSDATA"]
    lockFilesFolder=acsdata+"/tmp"
    n=0
    for file in listdir(lockFilesFolder):
        if file.startswith(prefix) and file.endswith(suffix):
            print "Lock file found in",lockFilesFolder+":",file
            n=n+1
    if n==0:
        print "No lock file found in",lockFilesFolder
    return n

if __name__ == "__main__":
    instanceLocker = AcsInstanceLockHelper()
    prefix=instanceLocker.lockFilenamePrefix
    suffix=instanceLocker.lockFilenameSuffix
    
    # Get the actual ACS_INSTANCE
    # None if ACS_INSTANCE not defined in the environment
    try:
        instance=environ['ACS_INSTANCE']
    except:
        instance=None
    
    # There should be no lock file at the beginning of the test
    if printListOfLockFiles(prefix,suffix)!=0:
        print "ERROR: the folder should not contain any lock file at this stage!"
    
    # Lock a instance
    print "----- Test 1: lock of a ACS instance -----"
    print "Locking instance 3"
    instanceLocker.lock(3)
    printListOfLockFiles(prefix,suffix)
    print "Unlocking instance 3"
    instanceLocker.unlock(3)
    if printListOfLockFiles(prefix,suffix)!=0:
        print "ERROR: the folder should not contain any lock file at this stage!"
    print "----- Test 1 done -----"
    
    # Lock ALL possible instances
    print "----- Test 2: locking all possible instances -----"
    print "Locking all possible instances"
    for i in range(10):
        if instanceLocker.lock(i)!=0:
            print "ERROR locking instance",i
    if printListOfLockFiles(prefix,suffix)!=10:
        print "ERROR: some lock file is missing!"
    print "Freeing instances"
    for i in range(10):
        if instanceLocker.unlock(i)!=0:
            print "ERROR releasing instance",i
    if printListOfLockFiles(prefix,suffix)!=0:
        print "ERROR: the folder should not contain any lock file at this stage!"
    print "----- Test 2 done -----"
    
    print "----- Test 3: lock a invalid instance -----"
    print "Locking instance 11"
    # We expect a exception
    try:
        instanceLocker.lock(11)
        # The following print should never happen
        print "ERROR locking of invalid instance returned OK"
    except:
        print "Ok: the locking of a invalid instance has been rejected"
    # There should be no lock files in the folder now
    if printListOfLockFiles(prefix,suffix)!=0:
        print "ERROR: the folder should not contain any lock file at this stage!"
    print "----- Test 3 done -----"
    
    # Check locking/unlocking of the actual ACS_INSTANCE
    print "----- Test 4: lock ACS_INSTANCE -----"
    if instance==None:
        expectedInstance=0
    else:
        expectedInstance=int(instance)
    print "Locking ACS_INSTANCE",instance
    lockedInstance=instanceLocker.checkAndLock()
    if lockedInstance==-1:
        print "ERROR locking ACS_INSTANCE"
    else:
        print "Instance locked",lockedInstance
        if expectedInstance!=lockedInstance:
            print "ERROR: instance locked",lockedInstance,"expected",expectedInstance
    if printListOfLockFiles(prefix,suffix)!=1:
        print "ERROR: the folder should contain one and only one lock file at this stage!"
    # Free the instance
    if instanceLocker.freeInstance()!=0:
        print "Error freeing ACS_INSTANCE"
    if printListOfLockFiles(prefix,suffix)!=0:
        print "ERROR: the folder should not contain any lock file at this stage!"
    print "----- Test 4 done -----"
#
# ___oOo___
