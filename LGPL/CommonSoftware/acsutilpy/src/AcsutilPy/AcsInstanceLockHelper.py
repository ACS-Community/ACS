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
# acaproni  2014-02-07  created
#
from os      import environ, makedirs, chmod
from os.path import exists 
import stat
from subprocess import call

class AcsInstanceLockHelper:
    '''
    This class is in charge of creating/deleting the lock file for a running ACS instance
    in ACSDATA/tmp (@see ICT-734 for further details).
    
    The name of the lock files is defined in __buildLockFileName(...).
    
    The lock file is created with the lockfile unix command.
    
    @attention: The lock file must be created using ACSDATA environment variable and not ACS_TMP 
                because the latter could be redirected by the user (and it is normally done while testing).
    
    @since: ACS 12.3
    '''    
    
    def __init__(self):
        '''
        Constructor
        '''
        
        # ACSDATA from the environment variable
        self.__acsdataEnvVar=environ['ACSDATA']
        
        # ACS_INSTANCE from environment variable
        # It is None if ACS_INSTANCE is not defined in the environment
        # because it can be normal in operations
        self.__instanceEnvVar=None
        try:
           self.__instanceEnvVar=environ['ACS_INSTANCE'] 
        except:
            pass
        if self.__instanceEnvVar is not None:
            # Is the instance valid?
            assert int(self.__instanceEnvVar) in range(10)
        
        # The path where to read and write lock files
        self.__lockFilesBaseFolder=self.__acsdataEnvVar+"/tmp/"
        if not exists(self.__lockFilesBaseFolder):
            makedirs(self.__lockFilesBaseFolder)
            mask=stat.S_IRUSR|stat.S_IXUSR|stat.S_IWUSR
            mask=mask|stat.S_IRGRP|stat.S_IWGRP|stat.S_IXGRP
            mask=mask|stat.S_IROTH|stat.S_IWOTH|stat.S_IXOTH
            chmod(self.__lockFilesBaseFolder,mask)
        
        # The prefix to build the name of a lock file
        self.lockFilenamePrefix="acsInstance"
        
        # The suffix to build the name of a lock file
        self.lockFilenameSuffix=".lock"
        
    def __buildLockFileName(self, instance):
        '''
        Build and return the lock file name for the passed instance.
        
        @param instance: The ACS instance 
        @return The full path name of the lock file for the passed instance
        '''
        assert instance in range(10)
        return self.__lockFilesBaseFolder+self.lockFilenamePrefix+str(instance)+self.lockFilenameSuffix
        
    def isAvailableInstance(self, instance):
        '''
        Check if the instance, whose number is passed in the argument, is available
        i.e. a lock file for such instance does not exist.
        
        @param instance: The instance to check for availability
        @return True if the instance is available
                False otherwise 
        '''
        assert instance in range(10)
        lockFileName=self.__buildLockFileName(instance)
        print "Checking the existence of",lockFileName
        
    def lock(self, instance):
        '''
        Create the lock file for the passed instance
        
        @param instance: The ACS instance
        @return: 0 if the instance has been locked;
                the error code if the instance has not been locked
        '''
        assert instance in range(10)
        lockFileName=self.__buildLockFileName(instance)
        print "Locking instance",str(instance),"with lock file",lockFileName
        return call(["lockfile","-r-0",lockFileName])
        
    
    def unlock(self, instance):
        '''
        Remove the lock file for the passed instance
        
        @param instance: The ACS instance
        @return: 0 if the lock has been removed
        ''' 
        assert instance in range(10)
        lockFileName=self.__buildLockFileName(instance)
        print "Freeing instance",str(instance),"removing lock file",lockFileName
        return call(["rm","-rf",lockFileName])
    
    def checkAndLock(self):
        '''
        checkAndLock checks if the instance is available and create the lock file
        by delegating to self.lock(..).
        
        This method behaves differently if ACS_INSTANCE environment variable is defined or not.
        
        If ACS_INSTANCE is defined then the instance to lock is the one defined by ACS_INSTANCE itself.
        In this case it tries to lock the instance by delegating to self.lock(..).
        The method returns an error if the instance is already locked; otherwise it returns the
        number of the instance locked that in this case is the same of ACS_INSTANCE.
        
        If ACS_INSTANCE is not defined then checkAndLock locks the first available instance by looking
        for the first available lock file in ACSDATA/tmp.
        Running ACS without having defined ACS_INSTANCE is a abnormal situation so this script
        writes a clear message in that case. The reason is that by getting the first available
        instance there is the risk to lock the instance assigned to someone else.
        
        While searching for the first available instance, the script looks for the first available lock file 
        in ACSDATA/tmp. Between the moment it looks for a free instance and the instant when the
        script effectively creates the lock file there is a critical race (i.e someone else
        outside of this script could have locked the instance in the meantime): in that case the
        script returns an error.
        
        In operation it is not mandatory to define the ACS_INSTANCE environmental variable even if
        it is normally set to 0. The script works in that case either because the first 
        available INSTANCE would be 0.
        
        @return The number of the locked instance i.e. a integer in [0,9] or
                -1 in case of error
        '''
        
        # Initially deal with the case of ACS_INSTANCE defined in the environment 
        if self.__instanceEnvVar is not None:
            ret = self.lock(int(self.__instanceEnvVar))
            if ret is not 0:
                # Error creating the lock file
                return -1
            else:
                # Lock file created: return the ACS instance
                return int(self.__instanceEnvVar)
        
        # Finally handle the case of a undefined ACS_INSTANCE
        print "ACS_INSTANCE not defined: looking for the first available instance"
        for instance in range(10):
            print "Checking instance",instance
            ret = self.lock(int(instance))
            if ret is 0:
                # Lock file created: return the ACS allocated instance
                return int(instance)
        
        print "No available ACS instance found"
        return -1

    def freeInstance(self):
        '''
        Free the ACS_INSTANCE instance.
        
        if ACS_INSTANCE is undefined this method throws a exception
        
        @return: 0 if the instance has been freed
        ''' 
        if self.__instanceEnvVar is None:
            # Throws a exception
            raise Exception("Trying to free a instance but ACS_INSTANCE is not defined.")
        
        return self.unlock(int(self.__instanceEnvVar))
    
#
# ___oOo___
