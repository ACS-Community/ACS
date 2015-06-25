# @(#) $Id: Scheduler.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA,  2001
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
'''
This module provides the implementation of an event/timeout scheduler which is
quite similar to the ACS Timer component. Its main use is as a utility class
which is particlarly useful with the implementation of BACI. Unfortunately,
the native sched.scheduler class did not include enough functionality to be 
used on its own.
'''
__revision__ = "$Id: Scheduler.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
import time
from threading import RLock
from threading import Thread
from copy      import deepcopy
from traceback import print_exc
from atexit    import register

#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.TimeHelper import getTimeStamp as getEpochTimeStamp
from Acspy.Common.Log         import getLogger

#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
def ACSSleep(hundredNanoUnits):
    '''
    A function designed to sleep for an amount of time defined in CORBA units
    of time (i.e., 100 nanoseconds).

    Parameters: 
        hundredNanoUnits - integer unit of time which is in hundreds
        of nanosecond units as its name implies.

    Returns: Nothing

    Raises: Nothing
    '''
    time.sleep(hundredNanoUnits/10000000.0)
#------------------------------------------------------------------------------
class Scheduler(object):
    '''
    A class designed to schedule/cancel "one shot" and continuous timeouts.
    It was created because the native Python scheduler class (found in the 
    sched package) is unsuitable for ACS purposes.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, minimum_sleep=1000L):
        '''
        Standard Constructor.

        Parameters:
        - minimum_sleep is the amount of time the thread created by this class
        should sleep after looking over the list of timeouts that need to be
        executed. This is in ACS::Time units (i.e., 100ns).
        '''
        
        #mutex used to lock member variables that can be changed from one
        #of the threads spawned by this class
        self.lock = RLock()
        
        #unique identifier for each thread is also the total number of threads
        #that have been created by this object so far
        self.timeout_counter = 0
        
        #list of timeouts created by this class 
        self.timeout_dict = {}

        #elementary amount of time to sleep (in ACS units)
        self.minimum_sleep = minimum_sleep

        self.logger = getLogger()

        #object has not been deleted
        self.alive = 1

        #create the thread which has the responsibility of executing timeouts
        self.executor_thread = Thread(target=self.__timeoutExecutor,
                                      name="Scheduler.__timeoutExecutor")
        self.executor_thread.setDaemon(1)
        #start the thread
        self.executor_thread.start()
        
        register(self.__destroy)
    #--------------------------------------------------------------------------
    def __destroy(self):
        '''
        The scheduler must be destroyed as it spawns a thread. This need not
        be exeucted by developers as the Python atexit module takes care of it.
        '''
        if self.alive:
            #signal the thread to die
            self.alive = 0
            #give it a chance to kill itself
            self.executor_thread.join()
    #--------------------------------------------------------------------------
    def scheduleTimeout(self,
                        timeout_func,
                        time_to_occur = 0,
                        frequency = 0,
                        arg_tuple = None,
                        arg_dict = None):
        '''
        Method used to schedule timeouts.

        Parameters:
        - timeout_func: a function specified by the developer to be invoked
        when each timeout occurs. 
        - time_to_occur: the time (relative to this invocation) in which the
        first invocation of timeout_func will occur. This is in 100ns units and
        must be a postive integer value. A value of 0 denotes that timeout_func
        shoud be invoked immediately.
        - frequency: the frequency at which timeout_func will be invoked. This
        is in 100ns units and must be a postive integer value. A value of 0 
        implies the timeout is a "one shot" deal.
        - arg_tuple: tuple of values supplied to timeout_func. A deepcopy of
        this tuple is made
        - arg_dict: dictionary of values supplied to timeout_func. A deepcopy
        of this dictionary is made

        Returns: ID of the newly created timeout

        Raises: ???
        '''
        
        #increment the timeout ID to be returned by this method to guarantee
        #it's uniqueness
        self.timeout_counter += 1
        timeout_id = self.timeout_counter
        
        #determine the first real execution date by adding the relative time
        #to the real current time.
        time_to_occur += getEpochTimeStamp().value
        
        #setup some parameters
        if arg_tuple == None:
            arg_tuple = ()
        if arg_dict == None:
            arg_dict = {}
        
        #add the ID to the list
        self.lock.acquire()
        self.timeout_dict[timeout_id] = {'time_to_occur':long(time_to_occur),
                                         'frequency': long(frequency),
                                         'timeout_func':timeout_func,
                                         'is_suspended':0,
                                         'arg_tuple':deepcopy(arg_tuple),
                                         'arg_dict':deepcopy(arg_dict)}
        self.lock.release()

        self.logger.logDebug("Timeout scheduled to occur:" + str(self.timeout_dict[timeout_id]))
        
        return timeout_id
    #--------------------------------------------------------------------------
    def changeTimeoutFrequency(self, timeout_id, new_frequency):
        '''
        Changes the frequency at which a specified timeout occurs.

        Parameters:
        - timeout_id is the ID of the timeout
        - new_frequency is the new frequency at which timeout invocations will
        occur. This value of course should be in 100ns units

        Returns: Nothing

        Raises: ???
        '''
        self.lock.acquire()
        self.timeout_dict[timeout_id]['frequency'] = long(new_frequency)
        self.lock.release()
    #--------------------------------------------------------------------------
    def getTimeout(self, timeout_id):
        '''
        Returns a dictionary containing information relating to a particular
        timeout.

        Parameters: timeout_id is the ID for the timeout returned by the
        scheduleTimeout method of this class.

        Returns: timeout dictionary

        Raises: Nothing
        '''
        return self.timeout_dict[timeout_id]
    #--------------------------------------------------------------------------
    def cancelAllTimeouts(self):
        '''
        Cancels all previous timeouts that have been scheduled.
        
        Parameters: None
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        #just empty the dictionary
        self.lock.acquire()
        self.timeout_dict = {}
        self.lock.release()
        return
    #--------------------------------------------------------------------------
    def cancelTimeout(self, timeout_id):
        '''
        Cancels the timeout using the specified ID.
        
        Parameters: timeout_id - ID of the timeout to be cancelled
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        self.lock.acquire()
        try:
            #simply remove it from the dict
            del self.timeout_dict[timeout_id]
        except:
            pass
        self.lock.release()
        return
    #--------------------------------------------------------------------------
    def suspendTimeout(self, timeout_id):
        '''
        Suspends the timeout using the specified ID.

        Parameters: timeout_id - ID of the timeout to be suspended

        Returns: Nothing

        Raises: Nothing
        '''
        self.lock.acquire()
        try:
            self.timeout_dict[timeout_id]['is_suspended'] = 1
        except:
            pass
        self.lock.release()
        return
    #--------------------------------------------------------------------------
    def resumeTimeout(self, timeout_id):
        '''
        Resumes the timeout using the specified ID.

        Parameters: timeout_id - ID of the timeout to be resumed

        Returns: Nothing

        Raises: Nothing
        '''
        self.lock.acquire()
        try:
            self.timeout_dict[timeout_id]['is_suspended'] = 0
        except:
            pass
        self.lock.release()
        return
    #--------------------------------------------------------------------------
    def __timeoutExecutor(self):
        '''
        Utility function executed by a thread and actually calls timeouts.
        User code should never invoke this directly.
        '''
        
        #continuously loop until this scheduler object is deleted
        while self.alive:
            
            #sleep first
            ACSSleep(self.minimum_sleep)
            
            #iterate through the entire list
            for timeout_id in self.timeout_dict.keys():
                
                #use the lock to obtain all the info we can about the
                #timeout
                self.lock.acquire()
                try:
                    time_to_occur = self.timeout_dict[timeout_id]['time_to_occur']
                    frequency = self.timeout_dict[timeout_id]['frequency']
                    timeout_func = self.timeout_dict[timeout_id]['timeout_func']
                    arg_tuple = self.timeout_dict[timeout_id]['arg_tuple']
                    arg_dict = self.timeout_dict[timeout_id]['arg_dict']
                    is_suspended = self.timeout_dict[timeout_id]['is_suspended']
                    self.lock.release()
                    
                except:
                    #it's possible that the user ran Scheduler.cancelAllTimeouts()
                    #and the timeoutID no longer exists. as a result
                    #sometimes it might be necessary to skip it
                    self.lock.release()
                    continue
                
                #check to see if the deadline has not passed and will not pass
                #before the next round
                now = getEpochTimeStamp().value
                if (time_to_occur>now)and(time_to_occur > (now+self.minimum_sleep)):
                    #OK to skip this timeout until the next round
                    continue
                
                #if it's suspended, we just move on to the next timeout
                if is_suspended:
                    continue
                
                #if we've gotten this far, it's OK to execute the timeout
                try:
                    timeout_func(*arg_tuple, **arg_dict)
                except:
                    self.logger.logCritical("Failed to invoke function denoted by ID:" + 
                                             str(timeout_id))
                    print_exc()
                
                #check to see if it was a one-time ordeal
                if frequency == 0L:
                    #need to delete this timeout ID
                    self.cancelTimeout(timeout_id)

                #figure out when the next invocation will be
                else:
                    next_time = getEpochTimeStamp().value + frequency
                    self.lock.acquire()
                    try:
                        self.timeout_dict[timeout_id]['time_to_occur'] = next_time
                    except:
                        #it's possible that the user ran Scheduler.cancelAllTimeouts()
                        #and the timeout_id no longer exists. as a result
                        #sometimes it might be necessary to ignore this error
                        pass
                    
                    self.lock.release()
#--------------------------------------------------------------------------
if __name__ == "__main__":
    def joe(blar1, blar2):
        print "joe method", blar1, blar2

    h = Scheduler()
    h.scheduleTimeout(joe, 1E7, 0, (88, None))
    time.sleep(3)
    temp_id = h.scheduleTimeout(joe, 1E7, 1E7, (1, None))
    time.sleep(3)
    h.changeTimeoutFrequency(temp_id, 1E6)
    
    #h.scheduleTimeout(joe, 3E7, 2E7, (2, None))
    #time.sleep(3)
    #h.scheduleTimeout(joe, 6E7, 3E7, (3, None))
    print "DWF2"
    time.sleep(7)
    h.changeTimeoutFrequency(temp_id, 3E7)
    time.sleep(7)
    h.cancelAllTimeouts()
