#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2012 
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
# "@(#) $Id: acsutilpyTestWildcharMatcher.py,v 1.1 2012/01/25 13:46:30 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# acaproni  2012-01-25  created
#

'''
Test WildcharMatchr
'''

from AcsutilPy.WildcharMatcher import simpleWildcardToRegex
from AcsutilPy.WildcharMatcher import wildcharMatch

if __name__ == "__main__":
    # Test simpleWildcardToRegex
    print "Translation to regex",simpleWildcardToRegex("No control chars in wildcard")
    print "Translation to regex",simpleWildcardToRegex("Dot.and asterisk*")
    print "Translation to regex",simpleWildcardToRegex("Asterisk* and 2 ?questions?marks")
    print "Translation to regex",simpleWildcardToRegex("Regexp control chars {} . () $^ [] \\ |")
    
    # Test wildcharMatch
    print "Match with '*' (expect True)",wildcharMatch("*", "Match")
    print "Match with '?' (expect True)", wildcharMatch("?est",'Test')
    print "Match with regex control char' (expect True)", wildcharMatch("?rray[4]",'Array[4]')
    print "Does not match with '?' (expect False)", wildcharMatch("?est",'finest')
    print "Does not match with '*' (expect False)", wildcharMatch("*pute",'Computer')
    print "Does not match with '*' and ?' (expect False)", wildcharMatch("?est* day",'Yesterday')
#
# ___oOo___
