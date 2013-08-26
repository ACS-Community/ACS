#!/usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002 
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307  USA
#
# @(#) $Id: acspyTestAcsCORBA.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
###############################################################################
'''
Tests CORBA access.
'''
from Acspy.Util import ACSCorba
###############################################################################
if __name__ == '__main__':
    print 'Manager corbaloc: %s' % ACSCorba.getManagerCorbaloc()
    print 'ORB: %s' % ACSCorba.getORB()
    print 'POA ROOT: %s' % ACSCorba.getPOARoot()
    print 'POA Manager: %s' % ACSCorba.getPOAManager()
    print 'Manager: %s' % ACSCorba.getManager()
    print 'Client: %s' % ACSCorba.getClient()
    print 'Log: %s' % ACSCorba.log()
    print 'LogFactory: %s' % ACSCorba.logFactory()
    print 'NotifyEventChannelFactory: %s' % ACSCorba.notifyEventChannelFactory()
    print 'ArchivingChannel: %s' % ACSCorba.archivingChannel()
    print 'LoggingChannel: %s' % ACSCorba.loggingChannel()
    print 'InterfaceRepository: %s' % ACSCorba.interfaceRepository()
    print 'CDB: %s' % ACSCorba.cdb()
    print 'ACSLogSvc: %s' % ACSCorba.acsLogSvc()
    print 'NameService: %s' % ACSCorba.nameService()
