import perftest
from Acspy.Nc.Supplier import Supplier
from time import sleep

class NCStressSupplier:
   def __init__(self, debug, sizeOfMessage, numMessages, delayBetweenMessages):
      self.debug = debug
      self.sizeOfMessage = sizeOfMessage
      self.numMessages = numMessages
      self.delayBetweenMessages = delayBetweenMessages

   def sendMessages(self):
      supplier = Supplier(perftest.NOTIFICATION_STRESS_CHANNEL_NAME)
      charString = ""
      for i in range(0, self.sizeOfMessage):
         charString += "a"
      event = perftest.NotificationServiceStress.NotificationServiceStressEvent(charString)
      for i in range(0, self.numMessages):
         supplier.publishEvent(event)
         if(self.delayBetweenMessages > 0):
            sleep(self.delayBetweenMessages / 1000)
      supplier.disconnect()
      return


def debugprint(stringToPrint, debugMode):
   if(debugMode is True):
      print "DEBUG: " + stringToPrint
