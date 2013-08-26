import perftest__POA
from Acspy.Servants.ContainerServices import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent import ACSComponent
import threading
import time

#------------------------------
# @author: Steve Harrington
# 
# Simple component to send logs to be used for benchmarking logging system.
#------------------------------

class SendingThread(threading.Thread):

   def __init__(self, numLogs, delay, logger):
      threading.Thread.__init__(self)
      self.numLogs = numLogs
      self.delay = delay
      self.done = False
      self.logger = logger
      return

   def run(self):
      for i in range(0, self.numLogs):
         self.logger.logInfo("Python stress msg: " + str(i))
         if(self.delay != 0):
            time.sleep(self.delay/1000.)
      self.done = True
      return

   def getDone(self):
      return self.done
      
class LogStressImpl(perftest__POA.LogStressWithDelay, ACSComponent, ContainerServices, ComponentLifecycle):

   def __init__(self):
      ACSComponent.__init__(self)
      ContainerServices.__init__(self)
      return

   def logNumTimes(self, numTimes, delayBetweenLogs):
      self.sendingThread = SendingThread(numTimes, delayBetweenLogs, self.getLogger())
      self.sendingThread.start()
      return

   def getThreadDone(self):
      return self.sendingThread.getDone()
