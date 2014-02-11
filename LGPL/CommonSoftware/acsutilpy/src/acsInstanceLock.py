#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2014 
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
#
# who       when      what
# --------  --------  ----------------------------------------------
# acaproni  2014-02-07  created
#

'''
A python script to lock/unlock a ACS instance.

The locking/unlocking is done by creating a lock file into ACSDATA/tmp
with the help of AcsutilPy.AcsInstanceLockHelper class.
'''
from optparse import OptionParser, OptionGroup
from AcsutilPy.AcsInstanceLockHelper import AcsInstanceLockHelper

if __name__ == "__main__":
    
    parser = OptionParser(usage="USAGE: %prog [Options] Action")
    parser.add_option("-b", "--baseport", 
                      dest="baseport",
                      action="store",
                      type="int",
                      help="The acs instance (0-9) you want to lock. If not set, $ACS_INSTANCE is used.", 
                      metavar="INSTANCE",
                      default=None)
    group=OptionGroup(parser, "Action: one and only one of these options must be present")
    group.add_option("-l", "--lock",
                      action="store_true",
                      default=False,
                      help="Lock the instance")
    group.add_option("-u", "--unlock",
                      action="store_true",
                      default=False,
                      help="Unlock the instance")
    parser.add_option_group(group)
    (options, args) = parser.parse_args()
    
    if options.lock==options.unlock:
        if options.lock is False:
            print "\nERROR: at least one action must be present\n"
        else:
            print "\nERROR: too many actions in command line\n"
        parser.print_help()
        os.exit(-1)
        
    
    lockHelper = AcsInstanceLockHelper()
    if options.baseport != None:
        # Use ACS_INSTANCE
        if options.lock:
            ret=lockHelper.lock(options.baseport)
        else:
            ret=lockHelper.unlock(options.baseport)
    else:
        if options.lock:
            ret=lockHelper.checkAndLock()
        else:
            ret=lockHelper.freeInstance()
    
    exit(ret)
# ___oOo___
