import perftest
from Acspy.Nc.Supplier import Supplier
from time import sleep

class NCStressSupplier:
   def __init__(self, debug, sizeOfMessage, numMessages, delayBetweenMessages):
      self.debug = debug
      self.sizeOfMessage = sizeOfMessage
      self.numMessages = numMessages
      self.delayBetweenMessages = delayBetweenMessages
      self.charString = ""
      for i in range(0, self.sizeOfMessage):
         self.charString += "a"
      self.event = perftest.NotificationServiceStress.NotificationServiceStressEvent(self.charString)

   def sendMessages(self):
      supplier = Supplier(perftest.NOTIFICATION_STRESS_CHANNEL_NAME)
      for i in range(0, self.numMessages):
         supplier.publishEvent(self.event)
         if(self.delayBetweenMessages > 0):
            sleep(self.delayBetweenMessages / 1000.)
      supplier.disconnect()
      return


def debugprint(stringToPrint, debugMode):
   if(debugMode is True):
      print "DEBUG: " + stringToPrint
