#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2008 
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: test_Acspy_Nc_CommonNC.py,v 1.4 2010/06/08 01:55:25 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-11-17  created
#

#--REGULAR IMPORTS-------------------------------------------------------------
import sys
import unittest
import mock
import CORBA
import CosNaming
import CosNotification
import NotifyMonitoringExt

#--ACS Imports-----------------------------------------------------------------
import acscommon
from ACSErr import NameValue
import Acspy.Common.Log
import Acspy.Util.ACSCorba

# The ACS Logger requires a connection to the Manager.
# In order to run the tests offline, the Logger has
# to be replace with a mock object
#mockLog = mock.Mock(spec=Acspy.Common.Log.Logger)

#def getMockLogger(name):
#    return mockLog

#Acspy.Common.Log.getLogger = getMockLogger

from ACSErrTypeCommonImpl import CORBAProblemExImpl
from Acspy.Util import NameTree
from Acspy.Nc.CommonNC import CommonNC

#--Global Mock Objects-------------------------------------------------------
# Mock object that replaces cdb_channel_config_exists which returns True
mockExists = mock.Mock()
mockExists.return_value = True

# Mock object that replaces cdb_channel_config_exists which returns True
mockNotExists = mock.Mock()
mockNotExists.return_value = False

# Mock object that returns simulated cdb properties
mockProps = mock.Mock()
mockProps.return_value = [ 'a', 'b', 'c' ]

# Mock object that simulates behaviour of the Naming Service
mocknameTree = mock.Mock(spec=NameTree.nameTree)

# Mock object that simulates the action of the NameTree module
mockNameTree = mock.Mock()
mockNameTree.nameTree.return_value = mocknameTree

# Simulated exception thrown by the Naming Service
def nameTree_side_effect(*args):
    raise RuntimeError("NameTree Fault")

# Simulated EventChannel CORBA object
gco = mock.Mock(spec=CORBA.Object)
gco._narrow.return_value = mock.Mock(spec=NotifyMonitoringExt.EventChannel)

# Simulation of failed Naming Service operation that subsequently succeeds
def nameTree_side_effect_onetime(*args):
    global mocknameTree

    mocknameTree.getObject.return_value = gco
    mocknameTree.getObject.side_effect = None
    raise RuntimeError("NameTree Fault")


class TestCommonNC(unittest.TestCase):
    
    def setUp(self):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)
        self.nc = CommonNC("Channel", mock.Mock())
        self.original = sys.stderr
        sys.stderr = mock.Mock(spec=sys.stderr)

    def tearDown(self):
        sys.stderr = self.original
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = None
        
    @mock.patch_object(Acspy.Nc.CommonNC,'cdb_channel_config_exists', mockNotExists)
    def test_configAdminProps_noconfig(self):
        """CommonNC configAdminProps returns expected value when no CDB configuration defined"""
        self.assertEqual([], self.nc.configAdminProps())

    @mock.patch_object(Acspy.Nc.CommonNC,'cdb_channel_config_exists', mockExists)
    @mock.patch_object(Acspy.Nc.CommonNC,'get_channel_admin_props', mockProps)
    def test_configAdminProps_propsExist(self):
        """CommonNC configAdminProps returns correct values when CDB configuration defined"""
        self.nc.logger = mock.Mock(spec=Acspy.Common.Log.Logger)
        self.assertEqual([ 'a', 'b', 'c' ], self.nc.configAdminProps())
        self.assertEqual([('logDebug', ('Found admin properties in the CDB',), {})], self.nc.logger.method_calls)

    @mock.patch_object(Acspy.Nc.CommonNC,'cdb_channel_config_exists', mockNotExists)
    def test_configQofS_noconfig(self):
        """CommonNC configQofS returns expected value when no CDB configuration defined"""
        self.assertEqual([], self.nc.configQofS())

    @mock.patch_object(Acspy.Nc.CommonNC,'cdb_channel_config_exists', mockExists)
    @mock.patch_object(Acspy.Nc.CommonNC,'get_channel_qofs_props', mockProps)
    def test_configQofS_propsExist(self):
        """CommonNC configQofS returns correct values when CDB configuration defined"""
        self.nc.logger = mock.Mock(spec=Acspy.Common.Log.Logger)
        self.assertEqual([ 'a', 'b', 'c' ], self.nc.configQofS())
        self.assertEqual([('logDebug', ('Found Q of S properties in the CDB',), {})], self.nc.logger.method_calls)

    def test_createNotificationChannel_missingFactory(self):
        """CommonNC createNotificationChannel throws correct exception when no EventChannelFactory defined"""
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        def side_effect(*args):
            raise CosNaming.NamingContext.NotFound(None, None)
        self.nc.nt.getObject.side_effect = side_effect
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        try:
            self.nc.createNotificationChannel()
            self.fail("No Exception thrown")
        except CORBAProblemExImpl, e:
            self.assertEqual(['Unable to get Notification Service'],e.getData('reason'))
            self.assertEqual(['CosNaming.NamingContext.NotFound(why=None, rest_of_name=None)'],e.getData('exception'))

    def test_createNotificationChannel_registerFailedNF(self):
        """CommonNC createNotificationChannel throws correct exception when registering channel with name service fails (Not Found)"""
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        def side_effect(*args):
            raise CosNaming.NamingContext.NotFound(None, None)
        mchan = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannel)
        mchanfac = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannelFactory)
        mchanfac.create_named_channel.return_value = (mchan,0)
        mfact = mock.Mock(spec=CORBA.Object)
        mfact._narrow.return_value = mchanfac
        self.nc.nt.getObject.return_value = mfact
        self.nc.nt.putObject.side_effect = side_effect
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.configQofS = mock.Mock()
        self.nc.configAdminProps = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        try:
            self.nc.createNotificationChannel()
            self.fail("No Exception thrown")
        except CORBAProblemExImpl, e:
            self.assertEqual(['Cannot register with Naming Service'],e.getData('reason'))
            self.assertEqual(['CosNaming.NamingContext.NotFound(why=None, rest_of_name=None)'],e.getData('exception'))

    def test_createNotificationChannel_registerFailedCP(self):
        """CommonNC createNotificationChannel throws correct exception when registering channel with name service fails (Cannot Proceed)"""
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        def side_effect(*args):
            raise CosNaming.NamingContext.CannotProceed(None, None)
        mchan = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannel)
        mchanfac = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannelFactory)
        mchanfac.create_named_channel.return_value = (mchan,0)
        mfact = mock.Mock(spec=CORBA.Object)
        mfact._narrow.return_value = mchanfac
        self.nc.nt.getObject.return_value = mfact
        self.nc.nt.putObject.side_effect = side_effect
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.configQofS = mock.Mock()
        self.nc.configAdminProps = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        try:
            self.nc.createNotificationChannel()
            self.fail("No Exception thrown")
        except CORBAProblemExImpl, e:
            self.assertEqual(['Cannot register with Naming Service'],e.getData('reason'))
            self.assertEqual(['CosNaming.NamingContext.CannotProceed(cxt=None, rest_of_name=None)'],e.getData('exception'))

    def test_createNotificationChannel_registerFailedIN(self):
        """CommonNC createNotificationChannel throws correct exception when registering channel with name service fails (Invalid Name)"""
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        def side_effect(*args):
            raise CosNaming.NamingContext.InvalidName()
        mchan = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannel)
        mchanfac = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannelFactory)
        mchanfac.create_named_channel.return_value = (mchan,0)
        mfact = mock.Mock(spec=CORBA.Object)
        mfact._narrow.return_value = mchanfac
        self.nc.nt.getObject.return_value = mfact
        self.nc.nt.putObject.side_effect = side_effect
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.configQofS = mock.Mock()
        self.nc.configAdminProps = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        try:
            self.nc.createNotificationChannel()
            self.fail("No Exception thrown")
        except CORBAProblemExImpl, e:
            self.assertEqual(['Cannot register with Naming Service'],e.getData('reason'))
            self.assertEqual(['CosNaming.NamingContext.InvalidName()'],e.getData('exception'))

    def test_createNotificationChannel_wrongFactoryType(self):
        """CommonNC createNotificationChannel throws correct exception when wrong type returned for EventChannelFactory"""
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        def side_effect(*args):
            raise NotifyMonitoringExt.NameAlreadyUsed(None)
        mfact = mock.Mock(spec=CORBA.Object)
        mfact._narrow.return_value = None
        self.nc.nt.getObject.return_value = mfact
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        try:
            self.nc.createNotificationChannel()
            self.fail("No Exception thrown")
        except CORBAProblemExImpl, e:
            self.assertEqual(['Invalid channel factory'],e.getData('reason'))
            self.assertEqual(["'NoneType' object has no attribute 'create_named_channel'"],e.getData('exception'))

    def test_createNotificationChannel_createFailedBadQoS(self):
        """CommonNC createNotificationChannel throws correct exception when EventChannelFactory cannot create channel (Bad QoS)"""
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        def side_effect(*args):
            raise CosNotification.UnsupportedQoS(None)
        mchanfac = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannelFactory)
        mchanfac.create_named_channel.side_effect = side_effect
        mfact = mock.Mock(spec=CORBA.Object)
        mfact._narrow.return_value = mchanfac
        self.nc.nt.getObject.return_value = mfact
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.configQofS = mock.Mock()
        self.nc.configAdminProps = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        try:
            self.nc.createNotificationChannel()
            self.fail("No Exception thrown")
        except CORBAProblemExImpl, e:
            self.assertEqual(['Unable to create channel'],e.getData('reason'))
            self.assertEqual(['CosNotification.UnsupportedQoS(qos_err=None)'],e.getData('exception'))

    def test_createNotificationChannel_createFailedBadAdmin(self):
        """CommonNC createNotificationChannel throws correct exception when EventChannelFactory cannot create channel (Bad Admin)"""
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        def side_effect(*args):
            raise CosNotification.UnsupportedAdmin(None)
        mchanfac = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannelFactory)
        mchanfac.create_named_channel.side_effect = side_effect
        mfact = mock.Mock(spec=CORBA.Object)
        mfact._narrow.return_value = mchanfac
        self.nc.nt.getObject.return_value = mfact
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.configQofS = mock.Mock()
        self.nc.configAdminProps = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        try:
            self.nc.createNotificationChannel()
            self.fail("No Exception thrown")
        except CORBAProblemExImpl, e:
            self.assertEqual(['Unable to create channel'],e.getData('reason'))
            self.assertEqual(['CosNotification.UnsupportedAdmin(admin_err=None)'],e.getData('exception'))

    def test_createNotificationChannel_createFailedDuplicate(self):
        """CommonNC createNotificationChannel throws correct exception when EventChannelFactory cannot create duplicate channel"""
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        def side_effect(*args):
            raise NotifyMonitoringExt.NameAlreadyUsed(None)
        mchanfac = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannelFactory)
        mchanfac.create_named_channel.side_effect = side_effect
        mfact = mock.Mock(spec=CORBA.Object)
        mfact._narrow.return_value = mchanfac
        self.nc.nt.getObject.return_value = mfact
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.configQofS = mock.Mock()
        self.nc.configAdminProps = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        try:
            self.nc.createNotificationChannel()
            self.fail("No Exception thrown")
        except CORBAProblemExImpl, e:
            self.assertEqual(['Unable to create channel'],e.getData('reason'))
            self.assertEqual(['NotifyMonitoringExt.NameAlreadyUsed()'],e.getData('exception'))

    def test_createNotificationChannel_createFailedMappingProblem(self):
        """CommonNC createNotificationChannel throws correct exception when EventChannelFactory cannot create mapping for channel"""
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        def side_effect(*args):
            raise NotifyMonitoringExt.NameMapError(None)
        mchanfac = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannelFactory)
        mchanfac.create_named_channel.side_effect = side_effect
        mfact = mock.Mock(spec=CORBA.Object)
        mfact._narrow.return_value = mchanfac
        self.nc.nt.getObject.return_value = mfact
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.configQofS = mock.Mock()
        self.nc.configAdminProps = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        try:
            self.nc.createNotificationChannel()
            self.fail("No Exception thrown")
        except CORBAProblemExImpl, e:
            self.assertEqual(['Unable to create channel'],e.getData('reason'))
            self.assertEqual(['NotifyMonitoringExt.NameMapError()'],e.getData('exception'))

    def test_createNotificationChannel(self):
        """CommonNC createNotificationChannel configures event channel correctly"""
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        mchan = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannel)
        mchanfac = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannelFactory)
        mchanfac.create_named_channel.return_value = (mchan,0)
        mfact = mock.Mock(spec=CORBA.Object)
        mfact._narrow.return_value = mchanfac
        self.nc.nt.getObject.return_value = mfact
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.configQofS = mock.Mock()
        self.nc.configAdminProps = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        self.nc.createNotificationChannel()
        self.assertEqual(mchan, self.nc.evtChan)


    def test_destroyNotificationChannel(self):
        """CommonNC destroyNotificationChannel returns the expected value when successful"""
        self.nc.logger = mock.Mock(spec=Acspy.Common.Log.Logger)
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        self.nc.evtChan = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannel)
        self.assertEqual(1, self.nc.destroyNotificationChannel())
        self.assertEqual([], self.nc.logger.method_calls)

    def test_destroyNotificationChannel_deregisterFailedCP(self):
        """CommonNC destroyNotificationChannel throws exception when deregister from Naming Service fails(CannotProceed)"""
        def side_effect(*args):
            raise CosNaming.NamingContext.CannotProceed(None, None)
        self.nc.logger = mock.Mock(spec=Acspy.Common.Log.Logger)
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        self.nc.nt.delObject.side_effect = side_effect
        self.nc.evtChan = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannel)
        self.assertEqual(0, self.nc.destroyNotificationChannel())
        self.assertEqual([('logWarning', ('CosNaming.NamingContext.CannotProceed(cxt=None, rest_of_name=None)',), {})], self.nc.logger.method_calls)

    def test_destroyNotificationChannel_deregisterFailedNF(self):
        """CommonNC destroyNotificationChannel throws exception when deregister from Naming Service fails(NotFound)"""
        def side_effect(*args):
            raise CosNaming.NamingContext.NotFound(None, None)
        self.nc.logger = mock.Mock(spec=Acspy.Common.Log.Logger)
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        self.nc.nt.delObject.side_effect = side_effect
        self.nc.evtChan = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannel)
        self.assertEqual(0, self.nc.destroyNotificationChannel())
        self.assertEqual([('logWarning', ('CosNaming.NamingContext.NotFound(why=None, rest_of_name=None)',), {})], self.nc.logger.method_calls)

    def test_destroyNotificationChannel_deregisterFailedIN(self):
        """CommonNC destroyNotificationChannel throws exception when deregister from Naming Service fails(InvalidName)"""
        def side_effect(*args):
            raise CosNaming.NamingContext.InvalidName()
        self.nc.logger = mock.Mock(spec=Acspy.Common.Log.Logger)
        self.nc.nt = mock.Mock(spec=NameTree.nameTree)
        self.nc.nt.delObject.side_effect = side_effect
        self.nc.evtChan = mock.Mock(spec=NotifyMonitoringExt._objref_EventChannel)
        self.assertEqual(0, self.nc.destroyNotificationChannel())
        self.assertEqual([('logWarning', ('CosNaming.NamingContext.InvalidName()',), {})], self.nc.logger.method_calls)

    def test_getChannelDomain(self):
        """CommonNC channel domain is correct"""
        self.assertEqual(acscommon.ALMADOMAIN, self.nc.getChannelDomain())

    def test_getChannelKind(self):
        """CommonNC channel kind is correct"""
        self.assertEqual(acscommon.NC_KIND, self.nc.getChannelKind())

    def test_getNotificationFactoryName_default(self):
        """CommonNC getNotificationFactoryName returns default"""
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = None
        self.assertEqual(acscommon.NOTIFICATION_FACTORY_NAME, self.nc.getNotificationFactoryName())

    def test_getNotificationFactoryName_given(self):
        """CommonNC getNotificationFactoryName returns non-default"""
        self.nc.getNotificationFactoryNameForChannel = mock.Mock()
        self.nc.getNotificationFactoryNameForChannel.return_value = "NotifyFactory"
        self.assertEqual('NotifyFactory', self.nc.getNotificationFactoryName())


    @mock.patch_object(Acspy.Nc.CommonNC,'get_notification_service_mapping', mock.Mock())
    def test_getNotificationFactoryNameForChannel_noconfig(self):
        """CommonNC getNotificationFactoryNameForChannel returns expected value when not configured"""
        Acspy.Nc.CommonNC.get_notification_service_mapping.return_value = []
        self.assertEqual(True, self.nc.getNotificationFactoryNameForChannel('Channel') is None)
        
    @mock.patch_object(Acspy.Nc.CommonNC,'get_notification_service_mapping', mock.Mock())
    def test_getNotificationFactoryNameForChannel_default(self):
        """CommonNC getNotificationFactoryNameForChannel returns expected default value"""
        Acspy.Nc.CommonNC.get_notification_service_mapping.return_value = [{'DefaultNotificationService': 'NotifyEventChannelFactory'}]
        self.assertEqual('NotifyEventChannelFactory', self.nc.getNotificationFactoryNameForChannel(None))

    @mock.patch_object(Acspy.Nc.CommonNC,'get_notification_service_mapping', mock.Mock())
    def test_getNotificationFactoryNameForChannel_domainFound(self):
        """CommonNC getNotificationFactoryNameForChannel returns expected domain value"""
        Acspy.Nc.CommonNC.get_notification_service_mapping.return_value = [{'Name': 'ALARMSYSTEM', 'NotificationService': 'AlarmNotifyService'}]
        self.assertEqual('AlarmNotifyService', self.nc.getNotificationFactoryNameForChannel(channel=None, domain='ALARMSYSTEM'))

    @mock.patch_object(Acspy.Nc.CommonNC,'get_notification_service_mapping', mock.Mock())
    def test_getNotificationFactoryNameForChannel_domainNotFound(self):
        """CommonNC getNotificationFactoryNameForChannel returns expected value when domain search fails"""
        Acspy.Nc.CommonNC.get_notification_service_mapping.return_value = []
        self.assertEqual(None, self.nc.getNotificationFactoryNameForChannel(channel=None, domain='Fred'))

    @mock.patch_object(Acspy.Nc.CommonNC,'get_notification_service_mapping', mock.Mock())
    def test_getNotificationFactoryNameForChannel_channelFound(self):
        """CommonNC getNotificationFactoryNameForChannel returns expected channel value"""
        Acspy.Nc.CommonNC.get_notification_service_mapping.return_value = [{'Name': 'CONTROL_*', 'NotificationService': 'ControlNotifyService'}, {'Name': 'HLA_*', 'NotificationService': 'HLANotifyService'}, {'Name': 'PARTICULAR', 'NotificationService': 'ParticularNotifyService'}]
        self.assertEqual('HLANotifyService', self.nc.getNotificationFactoryNameForChannel('HLA_FOO'))

    @mock.patch_object(Acspy.Nc.CommonNC,'get_notification_service_mapping', mock.Mock())
    def test_getNotificationFactoryNameForChannel_channelNotFound(self):
        """CommonNC getNotificationFactoryNameForChannel returns expected value when domain search fails"""
        Acspy.Nc.CommonNC.get_notification_service_mapping.return_value = []
        self.assertEqual(None, self.nc.getNotificationFactoryNameForChannel('HLA_FOO'))

    @mock.patch_object(Acspy.Nc.CommonNC, 'NameTree', mockNameTree)
    def test_initCORBA(self):
        """CommonNc initCORBA initializes object correctly"""
        global mocknameTree
        co = mock.Mock(spec=CORBA.Object)
        co._narrow.return_value = mock.Mock(spec=NotifyMonitoringExt.EventChannel)
        mocknameTree.getObject.return_value = co
        self.nc.initCORBA()
        self.assertEqual(False, self.nc.nt is None)
        self.assertEqual(mocknameTree, self.nc.nt)
        self.assertEqual(False, self.nc.evtChan is None)
        self.assertEqual(co._narrow(), self.nc.evtChan) 

    @mock.patch_object(mockNameTree, 'nameTree', nameTree_side_effect)
    @mock.patch_object(Acspy.Nc.CommonNC, 'NameTree', mockNameTree)
    def test_initCORBA_nameTreeEx(self):
        """CommonNc initCORBA handles Naming Service access exception correctly"""
        try:
            self.nc.initCORBA()
            self.fail('No Exception Raised')
        except CORBAProblemExImpl, e:
            self.assertEqual(['NameTree Fault'],e.getData('exception'))
            

    @mock.patch_object(Acspy.Nc.CommonNC, 'NameTree', mockNameTree)
    def test_initCORBA_noNamingService(self):
        """CommonNc initCORBA handles missing/invalid Naming Service correctly"""
        global mockNameTree
        mockNameTree.nameTree.return_value = None
        try:
            self.nc.initCORBA()
            self.fail('No Exception Raised')
        except CORBAProblemExImpl, e:
            self.assertEqual(['Naming Service'],e.getData('reason'))
        mockNameTree.nameTree.return_value = mocknameTree

    @mock.patch_object(Acspy.Nc.CommonNC, 'NameTree', mockNameTree)
    def test_initCORBA_noChannel(self):
        """CommonNc initCORBA creates EventChannel when appropriate"""
        def mockCreate():
            self.nc.evtChan = mock.Mock(spec=NotifyMonitoringExt.EventChannel)
        global mocknameTree
        mocknameTree.getObject.side_effect = nameTree_side_effect
        self.nc.createNotificationChannel = mockCreate
        self.nc.logger = mock.Mock(spec=Acspy.Common.Log.Logger)
        self.nc.initCORBA()
        self.assertEqual(True, self.nc.evtChan is not None)
        self.assertEqual([('logInfo', ('Created new channel.',), {})], self.nc.logger.method_calls)

    @mock.patch_object(Acspy.Nc.CommonNC, 'NameTree', mockNameTree)
    def test_initCORBA_noChannelDuplicate(self):
        """CommonNc initCORBA retries Naming Service lookup if EventChannel create fails with duplicate name exception"""
        def mockCreate():
            e = NotifyMonitoringExt.NameAlreadyUsed(None)
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.nc.channelName),
                                            NameValue("reason",
                                                      "Unable to create channel"),
                                            NameValue("exception",
                                                      str(e))])

        global mocknameTree
        mocknameTree.getObject.return_value = None
        mocknameTree.getObject.side_effect = nameTree_side_effect_onetime
        self.nc.createNotificationChannel = mockCreate
        self.nc.logger = mock.Mock(spec=Acspy.Common.Log.Logger)
        self.nc.initCORBA()
        self.assertEqual(True, self.nc.evtChan is not None)
        self.assertEqual(False, self.nc.logger.called)

    def test_object_initialization_default(self):
        """CommonNC default initialization"""
        nc = CommonNC(None, None)
        self.assertEqual(0, nc.connected)
        self.assertEqual('None', nc.channelName)
        self.assertEqual(True, nc.component is None)
        self.assertEqual('None', nc.domainName)
        self.assertEqual(True, nc.evtChan is None)
        self.assertEqual(True, nc.nt is None)

    def test_object_initialization_no_domain(self):
        """CommonNC initialization with default domain"""
        nc = CommonNC("Channel", mock.Mock())
        self.assertEqual(0, nc.connected)
        self.assertEqual('Channel', nc.channelName)
        self.assertEqual(True, isinstance(nc.component,mock.Mock))
        self.assertEqual('None', nc.domainName)
        self.assertEqual(True, nc.evtChan is None)
        self.assertEqual(True, nc.nt is None)

    def test_object_initialization_domain(self):
        """CommonNC initialization with domain"""
        nc = CommonNC("Channel", mock.Mock(), "Domain")
        self.assertEqual(0, nc.connected)
        self.assertEqual('Channel', nc.channelName)
        self.assertEqual(True, isinstance(nc.component,mock.Mock))
        self.assertEqual('Domain', nc.domainName)
        self.assertEqual(True, nc.evtChan is None)
        self.assertEqual(True, nc.nt is None)


if __name__ == '__main__':
    unittest.main()
