#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2009 
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
# "@(#) $Id: test_Acspy_Container.py,v 1.1 2009/01/15 23:20:56 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2009-01-09  created
#

#--REGULAR IMPORTS-------------------------------------------------------------
import unittest
import mock
from os import environ
import CDB

#--ACS Imports-----------------------------------------------------------------
from Acspy.Util import LoggingConfig_xsd
import Acspy.Util.ACSCorba

# In order to remove the dependency on ACS being up, the client and
# manager have to be replaced.  We use mock objects so that the Container
# will function normally.
Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)

def mockGetManager():
    mockMan = mock.Mock(spec=Acspy.Util.ACSCorba.maci._objref_Manager)
    mockMan.login.return_value = 12345
    return mockMan

Acspy.Util.ACSCorba.getManager = mockGetManager

import Acspy.Common.CDBAccess
import Acspy.Container


#--Global Objects
# Test Container
C = Acspy.Container.Container("UnitTestContainer")
#print "Initial Batchsize"
#print Acspy.Container.Log.CENTRALHANDLER.batchsize
#print C.cdbAccess
#print C.name
#print "MACI/Containers/"  + C.name
#C.cdbAccess.cache = {}
#print C.cdbAccess.cache
#print C.cdbAccess.getElement("MACI/Containers/"  + C.name, "Container/LoggingConfig")

#--Global Mock Objects-------------------------------------------------------
# Mock object that replaces the CDB
mockCDB = mock.Mock(spec=CDB._objref_DAL)

# Mock object that replaces the CDBaccess object
mockCDBaccess = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)

class TestContainer(unittest.TestCase):
    def setUp(self):
#        print
#        print Acspy.Container.Log.CENTRALHANDLER.batchsize
        pass

    def tearDown(self):
        C.cdbAccess.cache = {}
        C.refresh_logging_config()
        C.cdbAccess.cache = {}
        
##     def test_activateOffShoot(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_activate_component(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_configCORBA(self):
##         self.fail("Not implemented") # TODO: implement your test here

    @mock.patch_object(C, 'cdbAccess', mockCDBaccess)
    def test_configureComponentLogger(self):
        """Defined logger is configured to the CDB values."""
        global mockCDBaccess
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9' }]
        C.configureComponentLogger('DefinedTestComponent')
        ll = C.get_logLevels('DefinedTestComponent')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(8, ll.minLogLevel)
        self.assertEqual(9, ll.minLogLevelLocal)
        mockCDBaccess.getElement.return_value = mockCDBaccess

    @mock.patch_object(C,'cdbAccess', mockCDBaccess)
    def test_configureComponentLogger_unknown(self):
        """Unknown logger is configured to the CDB values."""
        global mockCDBaccess
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9' }]
        C.configureComponentLogger('UnknownTestComponent')
        ll = C.get_logLevels('UnknownTestComponent')
        self.assertEqual(True, ll.useDefault)
        self.assertEqual(2, ll.minLogLevel)
        self.assertEqual(2, ll.minLogLevelLocal)
        mockCDBaccess.getElement.return_value = mockCDBaccess

    @mock.patch_object(C,'cdbAccess', mockCDBaccess)
    def test_configureComponentLogger_partialLocal(self):
        """Partially-defined (local) logger is configured to the CDB values."""
        global mockCDBaccess
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        C.configureComponentLogger('PartiallyDefinedTestComponentA')
        ll = C.get_logLevels('PartiallyDefinedTestComponentA')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(2, ll.minLogLevel)
        self.assertEqual(9, ll.minLogLevelLocal)
        mockCDBaccess.getElement.return_value = mockCDBaccess

    @mock.patch_object(C,'cdbAccess', mockCDBaccess)
    def test_configureComponentLogger_partialCentral(self):
        """Partially-defined (local) logger is configured to the CDB values."""
        global mockCDBaccess
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        C.configureComponentLogger('PartiallyDefinedTestComponentB')
        ll = C.get_logLevels('PartiallyDefinedTestComponentB')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(8, ll.minLogLevel)
        self.assertEqual(2, ll.minLogLevelLocal)
        mockCDBaccess.getElement.return_value = mockCDBaccess

    @mock.patch_object(C,'cdbAccess', mockCDBaccess)
    def test_configureComponentLogger_localEnv(self):
        """Partially-defined (local) logger is configured to the CDB values and not STDOUT environment value."""
        global mockCDBaccess
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        environ['ACS_LOG_STDOUT'] = '5'
        C.configureComponentLogger('PartiallyDefinedTestComponentA')
        ll = C.get_logLevels('PartiallyDefinedTestComponentA')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(2, ll.minLogLevel)
        self.assertEqual(9, ll.minLogLevelLocal)
        del environ['ACS_LOG_STDOUT']
        mockCDBaccess.getElement.return_value = mockCDBaccess

    @mock.patch_object(C,'cdbAccess', mockCDBaccess)
    def test_configureComponentLogger_centralEnv(self):
        """Partially-defined (local) logger is configured to the CDB values and not CENTRAL environment value."""
        global mockCDBaccess
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        environ['ACS_LOG_CENTRAL'] = '5'
        C.configureComponentLogger('PartiallyDefinedTestComponentB')
        ll = C.get_logLevels('PartiallyDefinedTestComponentB')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(8, ll.minLogLevel)
        if 'ACS_LOG_STDOUT' in environ:
            self.assertEqual(int(environ['ACS_LOG_STDOUT']), ll.minLogLevelLocal)
        else:
            self.assertEqual(2, ll.minLogLevelLocal)
        del environ['ACS_LOG_CENTRAL']
        mockCDBaccess.getElement.return_value = mockCDBaccess

##     def test_createPOAForComponent(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_deactivate_components(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_destroyCORBA(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_disconnect(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_failedActivation(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_getCDBInfo(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_getCode(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_getExistingComponent(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_getManagerHost(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_getMyCorbaRef(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_get_component_info(self):
##         self.fail("Not implemented") # TODO: implement your test here

    def test_get_default_logLevels(self):
        """Container's default log levels are correct"""
        ll =  C.get_default_logLevels()
        lcfg = LoggingConfig_xsd.LoggingConfig()
        self.assertEqual(True, ll.useDefault)
        self.assertEqual(lcfg.minLogLevel, ll.minLogLevel)
        self.assertEqual(lcfg.minLogLevelLocal, ll.minLogLevelLocal)

    @mock.patch_object(C.cdbAccess, 'm_dal', mockCDB)
    def test_get_logLevels(self):
        global mockCDB
        mockCDB.get_DAO.return_value = '<Container xmlns="urn:schemas-cosylab-com:Container:1.0" xmlns:cdb="urn:schemas-cosylab-com:CDB:1.0" xmlns:baci="urn:schemas-cosylab-com:BACI:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:log="urn:schemas-cosylab-com:LoggingConfig:1.0" Timeout="20.0" UseIFR="1" ManagerRetry="10" ImplLang="py"> <Autoload> <cdb:_ string="acspyTestAutoload" /> </Autoload> <LoggingConfig centralizedLogger="Log" minLogLevel="5" minLogLevelLocal="6" maxLogQueueSize="0" dispatchPacketSize="0" immediateDispatchLevel="99" flushPeriodSeconds="100"> <log:_ Name="UnitTestContainer" minLogLevel="4" minLogLevelLocal="5" /> </LoggingConfig> </Container>'
        C.refresh_logging_config()
        cll = C.get_logLevels('UnitTestContainer')
        self.assertEqual(False, cll.useDefault)
        self.assertEqual(4, cll.minLogLevel)
        self.assertEqual(5, cll.minLogLevelLocal)
        cll.useDefault = True
        C.set_logLevels('UnitTestContainer', cll)
        mockCDB.get_DAO.return_value = mockCDB

    def test_get_logLevels_undefined(self):
        """Exception is thrown when log levels are requested for undefined logger"""
        self.assertRaises(Acspy.Container.LoggerDoesNotExistExImpl,
                          C.get_logLevels,"Phantom")

    def test_get_logLevels_nocdb(self):
        """Container log levels are correct when no CDB info present"""
        dll = C.get_default_logLevels()
        cll = C.get_logLevels('UnitTestContainer')
        self.assertEqual(dll.useDefault, cll.useDefault)
        self.assertEqual(dll.minLogLevel, cll.minLogLevel)
        self.assertEqual(dll.minLogLevelLocal, cll.minLogLevelLocal)

    @mock.patch_object(C.cdbAccess, 'm_dal', mockCDB)
    def test_get_logger_names(self):
        global mockCDB
        mockCDB.get_DAO.return_value = '<Container xmlns="urn:schemas-cosylab-com:Container:1.0" xmlns:cdb="urn:schemas-cosylab-com:CDB:1.0" xmlns:baci="urn:schemas-cosylab-com:BACI:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:log="urn:schemas-cosylab-com:LoggingConfig:1.0" Timeout="20.0" UseIFR="1" ManagerRetry="10" ImplLang="py"> <Autoload> <cdb:_ string="acspyTestAutoload" /> </Autoload> <LoggingConfig centralizedLogger="Log" minLogLevel="5" minLogLevelLocal="6" maxLogQueueSize="0" dispatchPacketSize="0" immediateDispatchLevel="99" flushPeriodSeconds="100"> <log:_ Name="TestComponent" minLogLevel="4" minLogLevelLocal="5" /> </LoggingConfig> </Container>'
        l = Acspy.Container.Log.getLogger('TestComponent')
        self.assertEqual(True, 'UnitTestContainer' in C.get_logger_names())
        self.assertEqual(True, 'TestComponent' in C.get_logger_names())
        mockCDB.get_DAO.return_value = mockCDB

    def test_get_logger_names_nocdb(self):
        """Container list of named loggers is correct when no CDB info present"""
        self.assertEqual(True, 'UnitTestContainer' in C.get_logger_names())

##     def test_handler(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_object_initialization(self):
##         self.fail("Not implemented") # TODO: implement your test here

    @mock.patch_object(C,'cdbAccess', mockCDBaccess)
    def test_refresh_logging_config(self):
        """Container log levels are set correctly to CDB values."""
        global mockCDBaccess
        mockCDBaccess.getElement.return_value = [{'centralizedLogger':"Log", 'minLogLevel':"5", 'minLogLevelLocal':"6",
                                                  'maxLogQueueSize':"0", 'dispatchPacketSize':"0", 'immediateDispatchLevel':"99", 'flushPeriodSeconds':"100"}]
        lcfg = LoggingConfig_xsd.LoggingConfig()
        C.refresh_logging_config()
        self.assertNotEqual(Acspy.Container.Log.LEVELS[lcfg.minLogLevel],
                         Acspy.Container.Log.DEFAULTCENTRALHANDLER.level)
        self.assertNotEqual(Acspy.Container.Log.LEVELS[lcfg.minLogLevelLocal],
                         Acspy.Container.Log.DEFAULTLOCALHANDLER.level)
        self.assertNotEqual(lcfg.maxLogQueueSize, Acspy.Container.Log.CENTRALHANDLER.capacity)
        self.assertNotEqual(lcfg.dispatchPacketSize, Acspy.Container.Log.CENTRALHANDLER.batchsize)
        self.assertNotEqual(Acspy.Container.Log.LEVELS[lcfg.immediateDispatchLevel],
                         Acspy.Container.Log.CENTRALHANDLER.dispatchlevel)
        self.assertNotEqual(lcfg.flushPeriodSeconds, Acspy.Container.Log.INTERVAL)
        mockCDBaccess.getElement.return_value = mockCDBaccess

    def test_refresh_logging_config_nocdb_withcentral(self):
        """Container log levels are reset correctly when no CDB info and ACS_LOG_CENTRAL is present"""
        environ['ACS_LOG_CENTRAL'] = '5'
        lcfg = LoggingConfig_xsd.LoggingConfig()
        C.refresh_logging_config()
        self.assertEqual(Acspy.Container.Log.LEVELS[5],
                         Acspy.Container.Log.DEFAULTCENTRALHANDLER.level)
        del environ['ACS_LOG_CENTRAL']

    def test_refresh_logging_config_nocdb_withlocal(self):
        """Container log levels are reset correctly when no CDB info and ACS_LOG_STDOUT is present"""
        environ['ACS_LOG_STDOUT'] = '5'
        lcfg = LoggingConfig_xsd.LoggingConfig()
        C.refresh_logging_config()
        self.assertEqual(Acspy.Container.Log.LEVELS[5],
                         Acspy.Container.Log.DEFAULTLOCALHANDLER.level)
        del environ['ACS_LOG_STDOUT']

    def test_refresh_logging_config_nocdb_or_env(self):
        """Container log levels are reset correctly when no CDB info or environment vars are present"""
        lcfg = LoggingConfig_xsd.LoggingConfig()
        C.refresh_logging_config()
        self.assertEqual(Acspy.Container.Log.LEVELS[lcfg.minLogLevel],
                         Acspy.Container.Log.DEFAULTCENTRALHANDLER.level)
        self.assertEqual(Acspy.Container.Log.LEVELS[lcfg.minLogLevelLocal],
                         Acspy.Container.Log.DEFAULTLOCALHANDLER.level)
        self.assertEqual(lcfg.maxLogQueueSize, Acspy.Container.Log.CENTRALHANDLER.capacity)
        self.assertEqual(lcfg.dispatchPacketSize, Acspy.Container.Log.CENTRALHANDLER.batchsize)
        self.assertEqual(Acspy.Container.Log.LEVELS[lcfg.immediateDispatchLevel],
                         Acspy.Container.Log.CENTRALHANDLER.dispatchlevel)
        self.assertEqual(lcfg.flushPeriodSeconds, Acspy.Container.Log.INTERVAL)
        self.assertEqual(True, Acspy.Container.Log.isFlushRunning())

##     def test_run(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_set_component_shutdown_order(self):
##         self.fail("Not implemented") # TODO: implement your test here

    def test_set_default_logLevels(self):
        """Container's default log levels are set correctly"""
        sll =  C.get_default_logLevels()
        sll.minLogLevel = 5
        sll.minLogLevelLocal = 6
        C.set_default_logLevels(sll)
        ll = C.get_default_logLevels()
        self.assertEqual(sll.useDefault, ll.useDefault)
        self.assertEqual(sll.minLogLevel, ll.minLogLevel)
        self.assertEqual(sll.minLogLevelLocal, ll.minLogLevelLocal)

    def test_set_logLevels(self):
        """Log levels for a named logger are set correctly"""
        sll =  C.get_default_logLevels()
        sll.useDefault = False
        sll.minLogLevel = 5
        sll.minLogLevelLocal = 6
        C.set_logLevels('UnitTestContainer',sll)
        ll = C.get_logLevels('UnitTestContainer')
        self.assertEqual(sll.useDefault, ll.useDefault)
        self.assertEqual(sll.minLogLevel, ll.minLogLevel)
        self.assertEqual(sll.minLogLevelLocal, ll.minLogLevelLocal)

    def test_set_logLevels_unknown(self):
        """Exception is thrown when attempt to set log levels on an undefined logger"""
        self.assertRaises(Acspy.Container.LoggerDoesNotExistExImpl,
                          C.set_logLevels, 'Phantom', None)

##     def test_shutdown(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_taggedmessage(self):
##         self.fail("Not implemented") # TODO: implement your test here

if __name__ == '__main__':
    unittest.main()
