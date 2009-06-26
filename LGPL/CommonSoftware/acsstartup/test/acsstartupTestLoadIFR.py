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
# "@(#) $Id: acsstartupTestLoadIFR.py,v 1.3 2009/06/26 05:55:22 hyatagai Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2008-02-19  created
#

from __future__ import with_statement
import os
import omniORB
import CORBA

omniORB.importIRStubs()

fname = "%s/ACS_INSTANCE.%s/iors/InterfaceRepositoryIOR" % (os.environ['ACS_TMP'], os.environ['ACS_INSTANCE'])

with open(fname,'r') as ior :
    
    print "Retrieving IR object..."
    orb = CORBA.ORB_init()
    obj = orb.string_to_object(ior.read())
    IR = obj._narrow(CORBA.Repository)

    print "Looking up Container Interface..."
    interf = IR.lookup_id('IDL:ijs.si/maci/Container:1.0')
    print "Interface is: ", interf

#
# ___oOo___
