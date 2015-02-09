#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2013 
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
# acaproni  2013-11-29  created
#

from threading import Condition 

class ContainerActivationMap(object):
    '''
    ContainerActivationMap remembers which components the container is 
    activating/deactivating.
    
    The purpose of this class is to avoid that concurrent calls at least 
    -activate the same component at the same time.
    -de-activate the same component at the same time.
    -one activate a component that another calling is de-activating
    
    The container registers in this class the name of the component
    it is activating or deactivating.
    If the same component is being activated or deactivated by another
    thread then the container waits until the activation/deactivation
    terminates and try again.
    The container notifies when a component has been activated or
    deactivated.
    
    @param logger: The logger
    '''
    
    def __init__(self,logger):
        
        # The logger
        self.logger=logger
        
        # All the threads of the container wait on this same condition because
        # they are interested in being awakened for each change in the 
        # list of components the container is working on
        #
        # All the threads wait on the same condition so they awake they have
        # to check again if they can go ahead. The green light to proceed
        # is represented by not having the name of the component in the list
        # of components that the container is processing
        self.cond=Condition()
        
        # The list of components that the container is concurrently processing
        # (i.e activating or deactivating)
        #
        # Note that dictionaries and other python types are thread safe but only 
        # to one operation, of course.
        self.processing=[]
        
    def _aboutToProcess(self,name):
        '''
        Internally we do not distinguish if the container is about
        to activate or deactivate a component
        
        @param name: The name of the component that the container is going to activate
        '''
        self.cond.acquire()
        while self.processing.count(name)>0:
            # the name of the process is already in the list so 
            # we wait until something changes in the list
            self.logger.logDelouse("The container is already dealing with "+name+": wait until it terminates")
            self.cond.wait()
        self.processing.append(name)
        self.logger.logDelouse("The container is not dealing with "+name+": the operation can go ahead")
        self.cond.release()
        
    def _processTerminated(self,name):
        '''
        A activation or deactivation just finished:
        remove the entry from the list and awake other threads (if any)
        waiting to start a new operation
        '''
        self.cond.acquire()
        if self.processing.count(name)==0:
            # This should never happen!
            self.logger.logWarning("The internal list of components seems corrupted")
        else:
            self.logger.logDelouse("The container terminated dealing with "+name)
            self.processing.remove(name)
        # Awake all the waiting threads
        self.cond.notify_all()
        self.cond.release()
    
    def aboutToActivate(self, name):
        '''
        The container is about to activate a component with the passed name 
        
        @param name: The name of the component that the container is going to activate
        '''
        self.logger.logDelouse("The container is asking the clearance to activate "+name)
        self._aboutToProcess(name)
        self.logger.logDelouse("The container has been cleared to activate "+name)
        
    
    def aboutToDeactivate(self, name):
        '''
        The container is about to deactivate a component with the passed name
        
        @param name: The name of the component that the container is going to deactivate 
        '''
        self.logger.logDelouse("The container is asking the clearance to deactivate "+name)
        self._aboutToProcess(name)
        self.logger.logDelouse("The container has been cleared to activate "+name)

    def activated(self, name):
        '''
        A component with the passed name has been activated
        
        @param name: The name of the activated component
        '''
        self.logger.logDelouse("The container terminated the activation of "+name+": awakening waiting threads")
        self._processTerminated(name)
    
    def deactivated(self, name):
        '''
        A component with the passed name has been deactivated
        
        @param name: The name of the deactivated component 
        '''
        self.logger.logDelouse("The container terminated the deactivation of "+name+": awakening waiting threads")
        self._processTerminated(name)
        
    def numOfProcessingItems(self):
        '''
        @return: The number of parallel operations that the container is performing
        '''
        return len(self.processing)
    
    def isProcessing(self,name):
        '''
        @param name: The name of the component to check 
        @return: True if the container is processing a component with the pased name
        ''' 
        return self.processing.count(name)>0
    