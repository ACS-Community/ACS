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
# "@(#) $Id: __init__.py,v 1.6 2012/03/09 14:36:04 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-09-29  created
#

import Acspy.Common.Log as Log
import Acspy.Common.CDBAccess as CDBAccess
import AcsAlarmSystem_xsd
import FaultState
import acsErrTypeAlarmSourceFactoryImpl as ErrFactory

class AlarmSystemInterfaceFactory(object):
    """
    Base class for creating sources and fault states.
    """
    logger = Log.getLogger("AlarmSystemInterfaceFactory")
    manager = None
    systemtype = None
    initialized = False
    registry ={}

    @classmethod
    def init(cls, man=None):
        """
        Initializes the factory.  This method must be called before using
        the other methods of this class.

        Returns: None
        """
        cls.logger.logTrace("AlarmSystemInterfaceFactory::init() entering.")
        cls.manager = man
        cls.initialized = True

        try:
            import ACSAlarmSystemInterfaceProxy as ACSProxy
            cls.registry['ACS'] = ACSProxy.ACSAlarmSystemInterfaceProxy
        except:
            pass

        try:
            import CERNAlarmSystemInterfaceProxy as CERNProxy
            cls.registry['CERN'] = CERNProxy.CERNAlarmSystemInterfaceProxy
        except:
            pass


        # Parse the CDB to get the system type
        cdb = CDBAccess.CDBaccess()
        try:
            ln = cdb.getField('Alarms/Administrative/AlarmSystemConfiguration','AlarmSystemConfiguration/configuration-property')
            doc = AcsAlarmSystem_xsd.minidom.parseString(ln)
            rootNode = doc.documentElement
            rootObj = AcsAlarmSystem_xsd.alarm_system_configurationListType.factory()
            rootObj.build(rootNode)
            cls.systemtype = rootObj.get_configuration_property()[0].valueOf_
        except:
            cls.systemtype='ACS'
        
        cls.logger.logDebug("Using %s alarm system" % cls.systemtype)
        cls.logger.logTrace("AlarmSystemInterfaceFactory::init() exiting.")

    @classmethod
    def done(cls):
        """
        Release the resources held by the factory.  This method must be called
        when the client is finished using the factory.

        Returns:  None
        """
        cls.logger.logTrace("AlarmSystemInterfaceFactory::done() entering.")
        cls.manager = None
        cls.initialized = False
        cls.systemtype = None
        cls.registry = {}
        cls.logger.logTrace("AlarmSystemInterfaceFactory::done() exiting.")

    @classmethod
    def createSource(cls, sourceName = None):
        """
        Create a new instance of an alarm system interface.

        Parameters: sourceName is the name of the alarm source

        Returns: an instance of the appropriate interface

        Raises:  exception if factory has not been initialized
        """
        cls.logger.logTrace("AlarmSystemInterfaceFactory::createSource() entering.")
        if not cls.initialized:
            raise ErrFactory.ACSASFactoryNotInitedExImpl()
        cls.logger.logTrace("AlarmSystemInterfaceFactory::createSource() exiting.")
        return cls.registry[cls.systemtype](sourceName)

    @classmethod
    def createFaultState(cls, family = None, member = None, code = None):
        """
        Create a new instance of a fault state.

        Parameters: family is the name of the alarm family for this fault
                    member is the name of the member of the alarm family for this fault
                    code is the error code for this fault

        Returns: an instance of FaultState

        Raises:  exception if factory has not been initialized
        """
        cls.logger.logTrace("AlarmSystemInterfaceFactory::createFaultState() entering.")
        if not cls.initialized:
            raise ErrFactory.ACSASFactoryNotInitedExImpl()
        cls.logger.logTrace("AlarmSystemInterfaceFactory::createFaultState() exiting.")
        return FaultState.FaultState(family, member, code)


#
# ___oOo___
