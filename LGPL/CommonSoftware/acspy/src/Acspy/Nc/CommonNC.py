# @(#) $Id: CommonNC.py,v 1.10 2012/01/25 16:45:33 acaproni Exp $
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

__revision__ = "$Id: CommonNC.py,v 1.10 2012/01/25 16:45:33 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
from AcsutilPy.WildcharMatcher import wildcharMatch
#--CORBA STUBS-----------------------------------------------------------------
from ACSErrTypeCommonImpl         import CORBAProblemExImpl
from ACSErr                       import NameValue
import CosNotifyChannelAdmin
import NotifyMonitoringExt
import acscommon
#--ACS Imports-----------------------------------------------------------------
from Acspy.Util.ACSCorba      import getORB
from Acspy.Util               import NameTree
from Acspy.Nc.CDBProperties   import cdb_channel_config_exists
from Acspy.Nc.CDBProperties   import get_channel_qofs_props
from Acspy.Nc.CDBProperties   import get_channel_admin_props
from Acspy.Nc.CDBProperties   import get_notification_service_mapping
from Acspy.Nc.ReconnectionCallback import ReconnectionCallback

from acscommon import NAMESERVICE_BINDING_NC_DOMAIN_DEFAULT
from acscommon import NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR

#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class CommonNC(object):
    '''
    Serves as a baseclass for notification channel objects.
    
    The notification channel is registered in the NamingService
    with the name: channelName+ NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR+domainName (see COMP-9338 for 
    further details).
    If the domain name is not provided then the fault is used: NAMESERVICE_BINDING_NC_DOMAIN_DEFAULT
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, channelname, component, domainname=None):
        '''
        Constructor.
        
        Params:
        - channelName is the channel name
        - component is the component this object resides within
        - domainName is the domain the channel belongs to
        
        Returns: Nothing
        
        Raises: Nothing.
        '''
        #true means I need to disconnect later
        self.connected = 0  
        #name of the channel we'll be working with
        self.channelName = str(channelname) 
        #domainname of the channel we'll be working with
        #
        # If the domain name is None then we assign a default here.
        # however for ACS NCs we do not have to register with a domain 
        # so we override this default while building the channel name in
        # combineChannelAndDomainName
        if domainname is None:
            self.domainName = NAMESERVICE_BINDING_NC_DOMAIN_DEFAULT
        else:
            self.domainName = str(domainname) 
        #CORBA ref to the channel
        self.evtChan = None
        #Python Naming Service helper class
        self.nt = None
        #component...use this to get the name.
        self.component = component
        #create the reconnection callback
        self.channel_factory = None
        self.callback = ReconnectionCallback(self)
    #------------------------------------------------------------------------------
    def combineChannelAndDomainName(self):
        return self.channelName+NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR+self.domainName
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
    def getNotificationFactoryNameForChannel(self,channel,domain=None):
        '''
        This method returns the name of the notification service for the channel
        or domain from the configuration information given in the CDB.

        Parameters:
        - channel is the channel name of the desired factory
        - domain is the domain of the desired factory

        Returns: string containing the factory name or None

        Raises: Nothing
        '''
        temp=""
        if channel is not None:
            crec = [ chan for chan in get_notification_service_mapping('Channel') if wildcharMatch(chan['Name'], channel)]
            if crec != []:
                temp=crec[0]['NotificationService']

        if len(temp)==0 and (domain is not None):
            crec = [ chan for chan in get_notification_service_mapping('Domain') if wildcharMatch(chan['Name'], domain)]
            if crec != []:
                temp=crec[0]['NotificationService']

        if len(temp)==0:
            crec = get_notification_service_mapping('Default')
            if crec != []:
                temp=crec[0]['DefaultNotificationService']
            else:
                return None
            
        if not temp.endswith(acscommon.NOTIFICATION_FACTORY_NAME):
            return temp+acscommon.NOTIFICATION_FACTORY_NAME
        else:
            return temp
            
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
        return self.getNotificationFactoryNameForChannel(self.channelName, self.domainName) or acscommon.NOTIFICATION_FACTORY_NAME
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

        #First try to use the naming service to access the channel. If that fails,
        #try to create it.  If this also fails, forget it...there's nothing that
        #can be done.
        try:
            self.channel_factory = self.nt.getObject(self.getNotificationFactoryName(), "")
            self.channel_factory = self.channel_factory._narrow(NotifyMonitoringExt.EventChannelFactory)
            obj = self.nt.getObject(self.combineChannelAndDomainName(), self.getChannelKind())
            self.evtChan = obj._narrow(NotifyMonitoringExt.EventChannel)
        except:
            try:
                self.createNotificationChannel()
                self.logger.logInfo('Created new channel.')
            except CORBAProblemExImpl, e:
                if e.getData('exception') == ['NotifyMonitoringExt.NameAlreadyUsed()']:
                    while True:
                        try:
                            obj = self.nt.getObject(self.combineChannelAndDomainName(), self.getChannelKind())
                            self.evtChan = obj._narrow(NotifyMonitoringExt.EventChannel)
                        except:
                            pass
                        if self.evtChan is not None:
                            break
                else:
                    raise

        #initialize the callback
        self.callback.init(self.channel_factory)

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
            self.nt.delObject(self.combineChannelAndDomainName(), self.getChannelKind())

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
            self.channel_factory = self.nt.getObject(self.getNotificationFactoryName(), "")
            self.channel_factory = self.channel_factory._narrow(NotifyMonitoringExt.EventChannelFactory)
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
            (self.evtChan, chan_id) = self.channel_factory.create_named_channel(self.configQofS(),
                                                                self.configAdminProps(),
                                                                self.channelName)
            #make the NRI happy
            chan_id = None
            
        except AttributeError, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("reason",
                                                      "Invalid channel factory"),
                                            NameValue("exception",
                                                      str(e))])
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
            self.nt.putObject(self.combineChannelAndDomainName(), self.getChannelKind(), self.evtChan)
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

