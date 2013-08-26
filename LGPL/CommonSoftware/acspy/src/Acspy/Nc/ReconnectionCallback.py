'''
Implementation of the Reconnection object to be regitered in the NC
'''

__revision__ = ""

#--CORBA STUBS--
from acsnc__POA            import OSReconnectionCallback
from NotifyExt             import ReconnectionRegistry
from NotifyMonitoringExt   import EventChannelFactory
import CORBA



class ReconnectionCallback (OSReconnectionCallback):

   def __init__ (self, NCSub): # pragma: NO COVER
      '''
      Constructor.

      Params:
      - NCSub is the consumer or supplier that implements the reconnect method

      Returns: Nothing

      Raises: Nothing
      '''

      self.ecf = None
      self.sub = NCSub
      self.callback_id = None
      self.id_is_valid = False


   def is_alive(self): # pragma: NO COVER
      return true

   def reconnect(self, new_connection): # pragma: NO COVER
      self.ecf = new_connection._narrow(EventChannelFactory)
      if self.ecf != None:
         self.sub.reconnect(self.ecf)


   def init(self, ecf):
      '''
      This method must be called to initialize and register the callback in the
      corresponding Event Channel Factory

      Params: 
      - ecf id the EventChannelFactory to use to register the callback

      Returns: Nothing

      Raises:Nothing
      '''
      if ecf == None:
         return

      self.ecf = ecf
#the try is for to pass the test
      try:
         callback = self._this()
         registry = self.ecf._narrow(ReconnectionRegistry)
         self.callback_id = registry.register_callback(callback)
         self.id_is_valid = True
      except AttributeError, e:
         pass

   def disconnect(self):
      '''
      Release the resources used by the callback

      Params: Nothing

      Returns: Nothing

      Raises: Nothing
      '''
      if self.id_is_valid :
         registry = self.ecf._narrow(ReconnectionRegistry)
         registry.unregister_callback(self.callback_id)
         self.id_is_valid = False

