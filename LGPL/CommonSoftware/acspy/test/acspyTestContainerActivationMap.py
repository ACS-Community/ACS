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
from threading import Thread
from random import randint
from time import sleep
from Acspy.Clients.SimpleClient import PySimpleClient
from Acspy.ContainerActivationMap import ContainerActivationMap

'''
Test the ContainerActivationMap
'''

class ContainerSimulator(Thread):
    '''
    ContainerSimulator activate/deactivate components as the container could do
    '''
    
    def __init__(self, activate,instances,namePrefix,map):
        '''
        Constructor
        
        @param activate: If True activate components
                         otherwise deactivate
        @param instances: The number of components to activate/deactivate
        @param namePrefix: The prefix to the name of each component 
        @param map: The map to get cleared
        '''
        Thread.__init__(self,name="ContainerSimulator for components "+namePrefix)
        self.activate=activate
        self.prefix=namePrefix
        self.instances=instances
        self.map=map
        
    def run(self):
        for i in range(self.instances):
            name=self.prefix+"_"+str(i)
            if self.activate:
                self.map.aboutToActivate(name)
            else:
                self.map.aboutToDeactivate(name)
            assert(self.map.isProcessing(name))
            sleep(randint(10,100)/1000)
        for i in range(self.instances):
            name=self.prefix+"_"+str(i)
            if self.activate:
                self.map.activated(name)
            else:
                self.map.deactivated(name)
            sleep(randint(10,100)/1000)
            
class ActivationBlockTester(Thread):
    '''
    The purpose of thread is to block while trying to activate/deactivate 
    a component because the container is already working on the same component
    '''
    def __init__(self,activate,compName,map,logger,threadName):
        '''
        Constructor
        
        @param activate: If True activate components
                         otherwise deactivate
        @param compName: The name of the component to activate/deactivate
        @param map: The map to get cleared
        @param logger: The logger 
        '''
        Thread.__init__(self,name="BlockTesterThread_"+threadName)
        self.compName=compName
        self.activate=activate
        self.map=map
        self.logger=logger
        
    def run(self):
        # Ensure that the map already contains the component
        # so that the thread will be blocked
        self.logger.logInfo(self.name+" started")
        assert(self.map.isProcessing(self.compName))
        if self.activate:
            self.map.aboutToActivate(self.compName)
        else:
            self.map.aboutToDeactivate(self.compName)
        assert(self.map.isProcessing(self.compName))
        self.logger.logInfo(self.name+" processing "+self.compName)
        if self.activate:
            self.map.activated(self.compName)
        else:
            self.map.deactivated(self.compName)
        self.logger.logInfo(self.name+" processed "+self.compName)
    
            
simpleClient = PySimpleClient("acspyTestContainerActivationMap")

map = ContainerActivationMap(simpleClient.getLogger())

# Simulate the starting of the activation of multiple components
simpleClient.getLogger().logInfo("Testing activation of components from the main thread...")
for i in range(1000):
    name="TestComponent_"+str(i)
    map.aboutToActivate(name)
    if not map.isProcessing(name):
        simpleClient.getLogger().logError("The map does not contain "+name)
        assert(map.numOfProcessingItems()==i+1)
# And then their termination of their activation
for i in range(1000):
    name="TestComponent_"+str(i)
    map.activated(name)
    if map.isProcessing(name):
        simpleClient.getLogger().logError("The map should NOT contain "+name)
        assert(map.numOfProcessingItems()==999-i)
simpleClient.getLogger().logInfo("Test of activation of components OK")
# Activation/deactivation of components by 5 concurrent threads
#
# Each Activated/Deactivated component has a different name so
# the map must not block any of them
simpleClient.getLogger().logInfo("Testing multiple threads against non conflicting component names...")
thread1=ContainerSimulator(True,500,"T1",map)
thread2=ContainerSimulator(False,500,"T2",map)
thread3=ContainerSimulator(True,500,"T3",map)
thread4=ContainerSimulator(False,500,"T4",map)
thread5=ContainerSimulator(True,500,"T5",map)
# Start the threads
simpleClient.getLogger().logInfo("Starting threads")
thread1.start()
thread2.start()
thread3.start()
thread4.start()
thread5.start()
# Wait their termination
simpleClient.getLogger().logInfo("Waiting for thread termination")
thread1.join()
thread2.join()
thread3.join()
thread4.join()
thread5.join()
simpleClient.getLogger().logInfo("Threads terminated")
# The map should be empty now
if not map.numOfProcessingItems()==0:
    simpleClient.getLogger().logError("The map should be empty now")
else:
    simpleClient.getLogger().logInfo("The map is empty")
 
# Same test as before but now we instantiate 5 threads that activate
# components with the same name
simpleClient.getLogger().logInfo("Testing multiple threads against the same set of components...")
thread1=ContainerSimulator(True,500,"T1",map)
thread2=ContainerSimulator(False,500,"T1",map)
thread3=ContainerSimulator(True,500,"T1",map)
thread4=ContainerSimulator(False,500,"T1",map)
thread5=ContainerSimulator(True,500,"T1",map)
# Start the threads
simpleClient.getLogger().logInfo("Starting threads")
thread1.start()
thread2.start()
thread3.start()
thread4.start()
thread5.start()
# Wait their termination
simpleClient.getLogger().logInfo("Waiting for thread termination")
thread1.join()
thread2.join()
thread3.join()
thread4.join()
thread5.join()
simpleClient.getLogger().logInfo("Threads terminated")
# The map should be empty now
if not map.numOfProcessingItems()==0:
    simpleClient.getLogger().logError("The map should be empty now")
else:
    simpleClient.getLogger().logInfo("The map is empty")
    
# Nested activation i.e. one component is activated
# during the activation of the first one
simpleClient.getLogger().logInfo("Testing nested activation...")
name="NestedActTest1"
map.aboutToActivate(name)
if not map.isProcessing(name):
    simpleClient.getLogger().logError("The map does not contain "+name)
nested="NestedActTest2"
map.aboutToActivate(nested)
if not map.isProcessing(name):
    simpleClient.getLogger().logError("The map does not contain "+name)
if not map.isProcessing(nested):
    simpleClient.getLogger().logError("The map does not contain "+nested)
map.activated(nested)
if not map.isProcessing(name):
    simpleClient.getLogger().logError("The map does not contain "+name)
if map.isProcessing(nested):
    simpleClient.getLogger().logError("The map should NOT contain "+nested)
map.activated(name)
if map.isProcessing(name):
    simpleClient.getLogger().logError("The map should NOT contain "+name)
if map.isProcessing(nested):
    simpleClient.getLogger().logError("The map should NOT contain "+nested)
if not map.numOfProcessingItems()==0:
    simpleClient.getLogger().logError("The map should be empty now")
else:
    simpleClient.getLogger().logInfo("The map is empty")
    simpleClient.getLogger().logInfo("Testing nested activation OK")

# The main thread start to operate on a component so all the other threads 
# must be blocked until it terminates 
simpleClient.getLogger().logInfo("Testing the serialization of the same component activation/deactivation")
name="blockedComponent"
numOfThreads=20
threads=[]
for i in range(numOfThreads):
    threads.append(ActivationBlockTester(i%2==0,name,map,simpleClient.getLogger(),str(i)))
map.aboutToActivate(name)
if not map.isProcessing(name):
    simpleClient.getLogger().logError("The map does not contain "+name)
else:
    simpleClient.getLogger().logInfo("The map is processing "+name)
# Start the threads
simpleClient.getLogger().logInfo("Starting threads")
for thread in threads:
    thread.start()
simpleClient.getLogger().logInfo("Threads started")
# Start the thread but all of them must immediately block
for thread in threads:
    assert(thread.isAlive())
simpleClient.getLogger().logInfo("All threads alive")
# Now this thread terminate the activation: all other threads are supposed to terminate
simpleClient.getLogger().logInfo("Main thread is completing the activation of "+name)
map.activated(name)
# Wait for the termination of all the other threads
simpleClient.getLogger().logInfo("Waiting for other threads to terminate")
for thread in threads:
    thread.join()
simpleClient.getLogger().logInfo("Threads terminated")
if not map.numOfProcessingItems()==0:
    simpleClient.getLogger().logError("The map should be empty now")
else:
    simpleClient.getLogger().logInfo("The map is empty")
    simpleClient.getLogger().logInfo("Testing the serialization of the same component OK")

#
# ___oOo___
