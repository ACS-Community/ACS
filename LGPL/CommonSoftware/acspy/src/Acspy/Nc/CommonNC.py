# @(#) $Id: CommonNC.py,v 1.2 2006/01/27 19:07:07 dfugate Exp $
#
# Copyright (C) 2001
# Associated Universities, Inc. Washington DC, USA.
#
# Produced for the ALMA project
#
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Library General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option) any
# later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
# details.
#
# You should have received a copy of the GNU Library General Public License
# along with this library; if not, write to the Free Software Foundation, Inc.,
# 675 Massachusetts Ave, Cambridge, MA 02139, USA.  Correspondence concerning
# ALMA should be addressed as follows:
#
# Internet email: alma-sw-admin@nrao.edu
'''
Provides functionality common to both NC suppliers and consumers.
'''

__revision__ = "$Id: CommonNC.py,v 1.2 2006/01/27 19:07:07 dfugate Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
from ACSErrTypeCommonImpl         import CORBAProblemExImpl
from ACSErr                       import NameValue
import CosNotifyChannelAdmin
import acscommon
#--ACS Imports-----------------------------------------------------------------
from Acspy.Util.ACSCorba      import getORB
from Acspy.Util               import NameTree
from Acspy.Nc.CDBProperties   import cdb_channel_config_exists
from Acspy.Nc.CDBProperties   import get_channel_qofs_props
from Acspy.Nc.CDBProperties   import get_channel_admin_props
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class CommonNC:
    '''
    Serves as a baseclass for notification channel objects.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, channelname, component):
        '''
        Constructor.
        
        Params:
        - channelName is the channel name
        - component is the component this object resides within
        
        Returns: Nothing
        
        Raises: Nothing.
        '''
        #true means I need to disconnect later
        self.connected = 0  
        #name of the channel we'll be working with
        self.channelName = str(channelname) 
        #CORBA ref to the channel
        self.evtChan = None
        #Python Naming Service helper class
        self.nt = None
        #component...use this to get the name.
        self.component = component
    #------------------------------------------------------------------------------
    def configQofS(self):
        '''
        Configures the quality of service properties for this channel.
        
        Only useful if the channel has not been created yet and this particular
        method is being overriden.
        
        Parameters: None
        
        Returns: A sequence of Quality of Service properties.
        
        Raises: Nothing
        '''
        if cdb_channel_config_exists(self.channelName):
            self.logger.logDebug("Found Q of S properties in the CDB")
            return get_channel_qofs_props(self.channelName)
        else:
            return []
    #------------------------------------------------------------------------------
    def configAdminProps(self):
        '''
        Configures the administrative properties for this channel.

        Only useful if the channel has not been created yet and this particular
        method is being overriden.

        Parameters: None

        Returns: A sequence of Administrative properties.

        Raises: Nothing
        '''
        if cdb_channel_config_exists(self.channelName):
            self.logger.logDebug("Found admin properties in the CDB")
            return get_channel_admin_props(self.channelName)
        else:
            return []
    #------------------------------------------------------------------------------
    def getChannelKind(self):
        '''
        This method returns a constant character pointer to the "kind" of
        notification channel as registered with the naming service (i.e., the kind
        field of a CosNaming.Name) which is normally equivalent to
        acscommon::NC_KIND. The sole reason this method is provided is to accomodate
        subclasses which subscribe/publish non-ICD style events (ACS archiving
        channel for example).In that case, the developer would override this method.
        
        Parameters: None
        
        Returns:a constant string.
        
        Raises: Nothing
        '''
        return acscommon.NC_KIND
    #------------------------------------------------------------------------------
    def getChannelDomain(self):
        '''
        This method returns a constant character pointer to the notification channel
        domain which is normally equivalent to acscommon::ALMADOMAIN. The sole
        reason this method is provided is to accomodate subclasses which
        subscribe/publish non-ICD style events (ACS archiving channel for example).
        In that case, the developer would override this method.

        Parameters: None
        
        Returns: a constant string.

        Raises: Nothing
        '''
        return acscommon.ALMADOMAIN
    #------------------------------------------------------------------------------
    def getNotificationFactoryName(self):
        '''
        This method returns the name of the notification service as registered
        with the CORBA Naming Service.

        Parameters: None
        
        Returns: pointer to a constant string. Normally
        acscommon::NOTIFICATION_FACTORY_NAME

        Raises: Nothing
        '''
        return acscommon.NOTIFICATION_FACTORY_NAME
    #------------------------------------------------------------------------------
    def initCORBA(self):
        '''
        Handles all the CORBA involved in creating a CommonNC.
        
        Parameters: None
        
        Returns: Nothing
        
        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        #Get orb stuff, and name service tree.
        #If any of this fails, must raise an exception because there's absolutely
        #nothing that can be done.
        try:
            self.nt = NameTree.nameTree(getORB())
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("exception",
                                                      str(e))])
        if self.nt == None:
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("reason",
                                                      "Naming Service")])

        #Get EventChannel.
        #First try to use the naming service to access the channel. If that fails,
        #try to create it.  If this also fails, forget it...there's nothing that
        #can be done.
        try:
            obj = self.nt.getObject(self.channelName, self.getChannelKind())
            self.evtChan = obj._narrow(CosNotifyChannelAdmin.EventChannel)
        except:
            self.createNotificationChannel()
            self.logger.logInfo('Created new channel.')    
    #------------------------------------------------------------------------------
    def destroyNotificationChannel(self):
        '''
        Destroys the channel and unregisters it from the naming service. ONLY 
        USE THIS METHOD IF YOU KNOW FOR CERTAIN THERE IS ONLY ONE SUPPLIER FOR 
        THE CHANNEL!!!  To be used with extreme caution. Likely to be removed with
        future versions of ACS.
        
        Parameters: None
        
        Returns: 1 on success and 0 on failure

        Raises: Nothing
        '''
        try:
            #Unregister our channel with the naming service
            self.nt.delObject(self.channelName, self.getChannelKind())

            #Destroy the remote object
            self.evtChan.destroy()
            
            #Nice return value
            return 1
        except Exception, e:
            self.logger.logWarning(str(e))
            print_exc()
            return 0
    #------------------------------------------------------------------------------
    def createNotificationChannel(self):
        '''
        Creates the notification channel.  Only to be called if it does not already
        exist.
        
        Parameters: None
        
        Returns: Nothing

        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        
        #Get at the Notification Service first.
        try:
            channel_factory = self.nt.getObject(self.getNotificationFactoryName(), "")
            channel_factory = channel_factory._narrow(CosNotifyChannelAdmin.EventChannelFactory)
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("reason",
                                                      "Unable to get Notification Service"),
                                            NameValue("exception",
                                                      str(e))])

        #Create the actual channel.
        try:
            (self.evtChan, chan_id) = channel_factory.create_channel(self.configQofS(),
                                                                     self.configAdminProps())
            #make the NRI happy
            chan_id = None
            
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("reason",
                                                      "Unable to create channel"),
                                            NameValue("exception",
                                                      str(e))])
        
        # Register the new channel w/ the naming service under the names &
        # type. The event channel is now ready for action.
        try:
            self.nt.putObject(self.channelName, self.getChannelKind(), self.evtChan)
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("reason",
                                                      "Cannot register with Naming Service"),
                                            NameValue("exception",
                                                      str(e))])
        return
#------------------------------------------------------------------------------
