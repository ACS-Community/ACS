#--REGULAR IMPORTS-------------------------------------------------------------
import perftest
import sys
import acstime
from time import sleep

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.TimeHelper  import getTimeStamp
from Acspy.Nc.Consumer   import Consumer

#------------------------------------------------------------------------------
class NCStressConsumer (Consumer):

    #--------------------------------------------------------------------------
    def __init__ (self, numToExpect, outputFileName, waitBetweenEvents=0):
        '''
        Constructor.
        
        Params:
        - handler
        
        Returns: Nothing
        
        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        Consumer.__init__(self, perftest.NOTIFICATION_STRESS_CHANNEL_NAME)
        self.addSubscription(perftest.NotificationServiceStress.NotificationServiceStressEvent)
        self.outputFileName = outputFileName
        if(outputFileName is None):
            self.outputfile = sys.stdout
        else:
            self.outputfile = open(outputFileName, "w")
        self.consumerReady()
        if(numToExpect is None):
            self.numToExpect = 1
        else:
            self.numToExpect = numToExpect
        self.numReceived = 0
        self.waitBetweenEvents=waitBetweenEvents

    def disconnect(self):
        Consumer.disconnect(self)
        self.outputfile.flush()
        if(self.outputFileName != None):
            self.outputfile.close()

    def push_structured_event (self, event):
        try:
            print "push_structured_event called!"
            self.numReceived = self.numReceived + 1

            receptionTime = getTimeStamp().value

            # output the reception time (only done for the first and last messages)
            if(self.numReceived == 1 or self.numReceived >= self.numToExpect):
                print >> self.outputfile, "reception time: " + str(receptionTime)
            
            #calculate the time difference in seconds
            sentTime = event.remainder_of_body.value().timestamp
            timeDiff = duration2py(long(receptionTime - sentTime))

            # special check for negative values - still not clear why we get the occasional value below zero, for
            # remote consumers - hypothesis is something to do w/ precision of clock synchronization. See wiki page
            # for more detailed explanation of this hypothesis. For now, make the negative values equal to zero.
            if timeDiff < 0.0:
               timeDiff = 0.0

            print >> self.outputfile, str(timeDiff)
            # flush is needed because monitoring applications are watching output for sign of completion
            # and will not know when things are finished if they're buffered
            if(self.numReceived >= self.numToExpect):
                self.outputfile.flush()

            sleep(self.waitBetweenEvents)

        except Exception, e:
            self.logger.logCritical('Unable to use handler function...' + str(e))
            print_exc()


#------------------------------------------------------------------------------

def debugprint(stringToPrint, debugMode):
   if(debugMode is True):
      print "DEBUG: " + stringToPrint

# this is a modified version of that found in acs time utilities; the one there
# has a bug... remove this when / if we fix the bug. The bug is that it gives
# precision of whole numbers due to not converting to float.
def duration2py(duration):
        '''
        Convert an ACS duration to a Python duration.

        Parameters: duration is in units of 100 nanoseconds

        Return: duration converted to seconds

        Raises: Nothing
        '''
        if isinstance(duration, long):
            duration = acstime.Duration(duration)

        sec = float(duration.value) / float(10000000L)
        return sec

