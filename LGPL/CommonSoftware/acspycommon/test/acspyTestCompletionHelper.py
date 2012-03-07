#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2006 
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
# "@(#) $Id: acspyTestCompletionHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# dfugate  2006-01-06  created
#

#************************************************************************
#   NAME
# 
#   SYNOPSIS
# 
#   DESCRIPTION
#
#   FILES
#
#   ENVIRONMENT
#
#   RETURN VALUES
#
#   CAUTIONS
#
#   EXAMPLES
#
#   SEE ALSO
#
#   BUGS     
#
#------------------------------------------------------------------------
#

from Acspy.Common.Log import getLogger
from Acspy.Common.Err import addComplHelperMethods
from Acspy.Common.Err import createErrorTrace
from ACSErr import Completion

et = createErrorTrace(51L,
                      4L,
                      level=-1)

#go through the usual spiel to get a completion
completion = Completion(132922080005000000L,
                        51L,
                        4L,
                        [et])

#add the helper methods
addComplHelperMethods(completion)

#test the helper methods
print "The timestamp is:", completion.getTimeStamp()
print "The type is:", completion.getType()
print "The code is:", completion.getCode()
print "The completion is error free:", completion.isErrorFree()
print "Logging the completion:", completion.log()
print "Adding data to the completion:", completion.addData("some_name", 42)
print "Relogging the completion:", completion.log()



#
# ___oOo___
