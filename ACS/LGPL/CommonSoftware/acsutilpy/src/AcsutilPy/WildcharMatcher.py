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
# "@(#) $Id: WildcharMatcher.py,v 1.1 2012/01/25 13:45:12 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# acaproni  2012-01-25  created
#

from re import match

'''
    This module offers helper methods to handle wildcard strings.
    
    At the present if handles "simple" wildcard string i.e. wildacrds
    whose control chars are only '*' and '?'.
'''

def simpleWildcardToRegex(wildcard):
    '''
        Return the passed wildcard string translated into a regular expression
        
        Chars that are control chars for regular expressions are escaped in the returned string.
    '''
    wildcarStr=str(wildcard)
    
    collector=[]
    collector.append("^")
    for c in wildcard:
        if c== '*':
            collector.append(".*")
            continue
        if c=='?':
            collector.append(".")
            continue
        if c=='(' or c==')' or c=='[' or c==']' or \
            c=='$' or c=='^' or c=='.' or c=='{' or \
            c=='}' or c=='|' or c=='\\':
            collector.append("\\")
            collector.append(str(c))
            continue
        collector.append(str(c))
    collector.append('$')
    return "".join(collector) 

def wildcharMatch(wildcard, string):
    '''
        Return True if the wildcard and the string matches.
        
        wilcard The wildcard string
        string The string to match with the wildcard
    '''
    wildcardRegExp = simpleWildcardToRegex(wildcard)
    return match(wildcardRegExp,string)!=None

#
# ___oOo___
