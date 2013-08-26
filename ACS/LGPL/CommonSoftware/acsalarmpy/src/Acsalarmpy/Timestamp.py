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
# "@(#) $Id: Timestamp.py,v 1.1 2008/10/09 16:11:10 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-10-01  created
#

import time

class Timestamp(object):
    """
    Utility class to hold a timestamp.  
    """
    def __init__(self, seconds=None, microSeconds=None):
        """
        Create a Timestamp.

        Parameters: seconds and microseconds are the time values to be recorded.
        """
        if seconds is None and microSeconds is None:
            t = time.time()
            self.seconds = int(t)
            self.microseconds = int((t - self.seconds) * 10 ** 6)
        else:
            self.seconds = seconds or 0
            self.microseconds = microSeconds or 0

    def toXML(self, elementName="user-timestamp", amountToIndent=6):
        """
        Generate an XML fragment representing the timestamp for the message
        to be sent to the LASER alarm server.

        Parameter: elementName is the tag for the timestamp
                   amountToIndent is the level of indentation

        Returns:  an indented XML string
        """
        pad = '\t'.expandtabs(amountToIndent)
        ln = '<%s seconds="%d" microseconds="%d"/>\n' % (elementName, self.seconds, self.microseconds)
        return pad + ln
    
#
# ___oOo___
