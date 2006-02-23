 
# @(#) $Id: Profiler.py,v 1.9 2005/02/23 00:04:55 dfugate Exp $
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

TODO:
- calculate meaningful data like standard deviation???
'''
__revision__ = "$Id: Profiler.py,v 1.9 2005/02/23 00:04:55 dfugate Exp $"

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
        self.reset()
        return
    #--------------------------------------------------------------------------
    def reset(self):
        '''
        Resets this objects values.
        '''
        
        #last time start was invoked
        self.lastStart=0L
        
        #total time that has passed between all start/stops
        self.totalTime=0L
        #total number of times start/stop has been invoked
        self.totalNumStarts=0L

        #the smallest amount of time that has passed between a start/stop
        self.minDuration=long(0x1FFFFFFF)
        #the largest amount of time that has passed between a start/stop
        self.maxDuration=0L

        #user added data
        self.extraDescrip = ""
        return
    #--------------------------------------------------------------------------
    def start(self):
        '''
        Starts a timing operation.
        '''
        
        if self.lastStart==0:
            self.totalNumStarts=self.totalNumStarts+1
        else:
            print "Looks like Profiler::start was called twice in a row without invoking Profiler::stop"

        self.lastStart=getTimeStamp().value
        return
    #--------------------------------------------------------------------------
    def stop(self):
        '''
        Stops a timing operation. Can only be called after a start invocation.
        '''
        if self.lastStart==0:
            print "Looks like Profiler::stop was called twice in a row without invoking Profiler::start"
            return 0
    
        timeDiff = (getTimeStamp().value - self.lastStart) / 10000.0

        if timeDiff>self.maxDuration:
            self.maxDuration=timeDiff
	
        if timeDiff<self.minDuration:
            self.minDuration=timeDiff
	
        self.lastStart=0
        self.totalTime = self.totalTime + timeDiff
    
        return timeDiff
    #--------------------------------------------------------------------------
    def fullDescription(self, msg):
        '''
        Prints out a full description of all times that were saved along with
        other relevant statistical data.

        Params: message to be printed out
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        if self.lastStart!=0:
            self.stop()
            
        if self.totalNumStarts==0:
            print "ACS PROFILER: No start invocations - ", msg
            return

        averageTime = self.totalTime / self.totalNumStarts
        date=time.strftime("%Y-%m-%dT%H:%M:%S",time.gmtime())+str(".000")

        retVal =  "#ACS PROFILER# msg=%s, avg=%f, runs=%d, mindur=%f, maxdur=%f, cpu=Unknown, mem=Unknown, date=%s, ip=%s, lang=py, units=ms %s" % (msg, averageTime, self.totalNumStarts, self.minDuration, self.maxDuration, date, str(getIP()), self.extraDescrip)

        print retVal
        return retVal
    #--------------------------------------------------------------------------
    def addData(self, key, value):
        '''
        Adds extra data to description
        '''
        self.extraDescrip = self.extraDescrip + ", " + key + "=" + value

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

