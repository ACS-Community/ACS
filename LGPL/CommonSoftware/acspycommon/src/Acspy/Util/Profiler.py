# @(#) $Id: Profiler.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
Provides profiling tools to benchmark functions/methods.

TODO:
- calculate meaningful data like standard deviation
'''
__revision__ = "$Id: Profiler.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
import time

#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.TimeHelper import getTimeStamp
from AcsutilPy.ACSPorts      import getIP

#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class Profiler:
    '''
    Profiler is a utility class providing a very simple profiling mechanism.
    '''
    #--------------------------------------------------------------------------
    def __init__(self):
        '''
        Constructor.
        '''
        #last time start was invoked
        self.last_start_time = None
        
        #total time that has passed between all start/stops
        self.total_time = None
        
        #total number of times start/stop has been invoked
        self.total_num_starts = None

        #the smallest amount of time that has passed between a start/stop
        self.min = None
        
        #the largest amount of time that has passed between a start/stop
        self.max = None

        #user added data
        self.extra_descrip = None
        
        self.reset()
        
    #--------------------------------------------------------------------------
    def reset(self):
        '''
        Resets this objects values.
        
        Params: None
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        self.last_start_time = 0L
        self.total_time = 0L
        self.total_num_starts = 0L
        self.min = long(0x1FFFFFFF)
        self.max = 0L
        self.extra_descrip = ""
        return
    #--------------------------------------------------------------------------
    def start(self):
        '''
        Starts a timing operation.
        
        Params: None
        
        Returns: Nothing
        
        Raises: ???
        '''
        
        if self.last_start_time==0:
            self.total_num_starts=self.total_num_starts+1
        else:
            print "Looks like Profiler::start was called twice in a row without invoking Profiler::stop"

        self.last_start_time=getTimeStamp().value
        return
    #--------------------------------------------------------------------------
    def stop(self):
        '''
        Stops a timing operation. Can only be called after a start invocation.
        
        Params: None
        
        Returns: the duration of time that has passed since start was
        called in ms units
        '''
        if self.last_start_time == 0:
            print "Looks like Profiler::stop was called twice in a row without invoking Profiler::start"
            return 0
        
        #get the time difference in millisecond units
        timeDiff = (getTimeStamp().value - self.last_start_time)/10000.0
        
        #check to see if we have new records for the min or max
        #durations
        if timeDiff > self.max:
            self.max = timeDiff
	    
        if timeDiff < self.min:
            self.min=timeDiff
	    
        self.last_start_time=0
        self.total_time = self.total_time + timeDiff
        
        return timeDiff
    #--------------------------------------------------------------------------
    def fullDescription(self, msg):
        '''
        Prints out a full description of all times that were saved along with
        other relevant statistical data.
        
        Params: message to be printed out
        
        Returns: the full description printed to stdout
        
        Raises: Nothing
        '''
        #sanity check to see if the timer has stopped
        if self.last_start_time != 0:
            self.stop()
            
        if self.total_num_starts == 0:
            print "ACS PROFILER: No start invocations - ", msg
            return

        avg_time = self.total_time / self.total_num_starts
        date = time.strftime("%Y-%m-%dT%H:%M:%S",time.gmtime())+str(".000")

        retVal =  "#ACS PROFILER# msg=%s, avg=%f, runs=%d, mindur=%f, maxdur=%f, cpu=Unknown, mem=Unknown, date=%s, ip=%s, lang=py, units=ms %s" % (msg, avg_time, self.total_num_starts, self.min, self.max, date, str(getIP()), self.extra_descrip)

        print retVal
        
        return retVal
    #--------------------------------------------------------------------------
    def addData(self, key, value):
        '''
        Adds extra data to the description.
        
        Params: 
            key - a keyword descriptor
            value - value 
        
        '''
        formatted_descrip = ", " + str(key) + "=" + str(value)
        self.extra_descrip = self.extra_descrip + formatted_descrip

#------------------------------------------------------------------------------
if __name__=="__main__":
    from time import sleep
    joe = Profiler()
    
    print "*****************************************************" 
    joe.fullDescription("In theory this should bail...")
    print "*****************************************************" 
    joe.start()
    joe.stop()
    joe.fullDescription("Should only be one...")
    print "*****************************************************" 
    joe.reset()
    joe.start()
    joe.stop()
    joe.fullDescription("Should only be one...")
    print "*****************************************************" 
    joe.reset()
    joe.start()
    joe.stop()
    joe.start()
    joe.stop()
    joe.start()
    joe.stop()
    joe.fullDescription("Should be three...")
    print "*****************************************************" 
    joe.reset()
    joe.start()
    print "This should fail!" 
    joe.start()
    joe.fullDescription("Should be none...")
    print "*****************************************************" 
    joe.reset()
    joe.start()
    joe.stop()
    print "This should fail!" 
    joe.stop()
    joe.fullDescription("Should be one...")
    print "*****************************************************" 
    joe.reset()
    joe.start()
    sleep(5)
    joe.stop()
    joe.start()
    sleep(3)
    joe.stop()
    joe.fullDescription("Should be two with an average of 4 seconds...")
    print "*****************************************************" 
    joe.reset()
    joe.start()
    sleep(1)
    joe.stop()
    joe.addData("a key", "a value")
    joe.fullDescription("Should be one extra descrip.")
    joe.addData("somethingElse", "1.2345678")
    joe.fullDescription("Should be two extra descrips.")

