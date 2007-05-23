#--REGULAR IMPORTS-------------------------------------------------------------
import perftest
import sys

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.TimeHelper  import getTimeStamp
from Acspy.Nc.Consumer   import Consumer

#------------------------------------------------------------------------------
class NCStressConsumer (Consumer):

    #--------------------------------------------------------------------------
    def __init__ (self, outputFileName):
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

    def disconnect(self):
        Consumer.disconnect(self)
        self.outputfile.flush()
        if(self.outputFileName != None):
            self.outputfile.close()

    def push_structured_event (self, event):
        try:
            print "push_structured_event called!"
            receptionTime = getTimeStamp().value
            sentTime = event.remainder_of_body.value().timestamp
            #calculate the time differences in seconds
            timeDiff = (receptionTime - sentTime)/100000000.0
            #debugprint("Reception time: " + str(receptionTime) + " and sent time: " + str(sentTime) + " and diff: " + str(timeDiff), True)
            print >> self.outputfile, str(timeDiff)
            self.outputfile.flush()
        except Exception, e:
            self.logger.logCritical('Unable to use handler function...' + str(e))
            print_exc()


#------------------------------------------------------------------------------

def debugprint(stringToPrint, debugMode):
   if(debugMode is True):
      print "DEBUG: " + stringToPrint

