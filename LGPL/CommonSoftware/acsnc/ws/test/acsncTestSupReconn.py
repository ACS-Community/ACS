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
# @(#) $Id: acsncTestConSup.py,v 1.6 2006/03/08 17:50:44 dfugate Exp $

"""
"""

from Acspy.Common.Log import getLogger
from Acspy.Clients.SimpleClient import PySimpleClient
from sys                        import argv
from time                       import sleep
import threading

num_test = int(argv[1])

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

# Get logger
logger = getLogger()

# Autoreconnect ON, Notify Service restarted
def test1():
        name = "NamedCh_SUP1"
        name_con = "NamedCh_CON1"
        consumer = simpleClient.getComponent(name_con)
        supplier = simpleClient.getComponent(name)
        logger.logAlert("Calling supplier.testReconn1(True,True)")
        supplier.testReconn1(True,True)
        logger.logAlert("Waiting 20 seconds ...")
        sleep(20)
        logger.logAlert("Releasing component %s"%(name))
        simpleClient.releaseComponent(name)
        logger.logAlert("Releasing component %s"%(name_con))
        simpleClient.releaseComponent(name_con)
        sleep(10)
        logger.logAlert('test1 finished')

# Autoreconnect OFF, Notify Service restarted
def test2():
        name = "NamedCh_SUP1"
        name_con = "NamedCh_CON1"
        consumer = simpleClient.getComponent(name_con)
        supplier = simpleClient.getComponent(name)
        logger.logAlert("Calling supplier.testReconn1(False,True)")
        supplier.testReconn1(False,True)
        logger.logAlert("Waiting 10 seconds ...")
        sleep(10)
        logger.logAlert("Releasing component %s"%(name))
        simpleClient.releaseComponent(name)
        logger.logAlert("Releasing component %s"%(name_con))
        simpleClient.releaseComponent(name_con)
        sleep(10)
        logger.logAlert('test2 finished')

# Autoreconnect ON, Notify Service stopped 
def test3():
        name = "NamedCh_SUP1"
        name_con = "NamedCh_CON1"
        consumer = simpleClient.getComponent(name_con)
        supplier = simpleClient.getComponent(name)
        logger.logAlert("Calling supplier.testReconn1(True,False)")
        supplier.testReconn1(True,False)
        logger.logAlert("Waiting 20 seconds ...")
        sleep(20)
        logger.logAlert("Releasing component %s"%(name))
        simpleClient.releaseComponent(name)
        logger.logAlert("Releasing component %s"%(name_con))
        simpleClient.releaseComponent(name_con)
        sleep(10)
        logger.logAlert('test3 finished')

# Autoreconnect OFF, Notify Service stopped 
def test4():
        name = "NamedCh_SUP1"
        name_con = "NamedCh_CON1"
        consumer = simpleClient.getComponent(name_con)
        supplier = simpleClient.getComponent(name)
        logger.logAlert("Calling supplier.testReconn1(False,False)")
        supplier.testReconn1(False,False)
        logger.logAlert("Waiting 10 seconds ...")
        sleep(10)
        logger.logAlert("Releasing component %s"%(name))
        simpleClient.releaseComponent(name)
        logger.logAlert("Releasing component %s"%(name_con))
        simpleClient.releaseComponent(name_con)
        sleep(10)
        logger.logAlert('test4 finished')


if num_test == 1:
	test1()
elif num_test == 2:
    test2()
elif num_test == 3:
    test3()
elif num_test == 4:
    test4()


simpleClient.disconnect()
