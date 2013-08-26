# @(#) $Id: FindFile.py,v 1.6 2009/10/27 11:04:21 agrimstrup Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA,  2001
#    (c) European Southern Observatory, 2002
#    Copyright by ESO (in the framework of the ALMA collaboration)
#    and Cosylab 2002, All rights reserved
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
# ---------------------------------------------------------------------------
'''
This module provides for Python the file finding algorithm used by
ACS. It is intended to work like its namesake "acsFindFile.cpp".
'''
# ---------------------------------------------------------------------------
__revision__ = "$Id: FindFile.py,v 1.6 2009/10/27 11:04:21 agrimstrup Exp $"
# ---------------------------------------------------------------------------
import os
import os.path
import stat
# ---------------------------------------------------------------------------
DEBUG = 0
# -------------------------------------------------------------------
def __getSEDirs():
    '''
    Returns a list of all SE prescribed INTROOT-type directories
    found using ACS environment variables (i.e., ACSROOT, INTLIST, etc)
    '''
    ret_val = []
    
    #check introot first
    if os.environ.has_key('INTROOT'):
        ret_val.append(os.environ['INTROOT'])
        
    # Check whether file exists in INTLIST
    if os.environ.has_key('INTLIST'):
        intlist = os.environ['INTLIST']
        intlist_list = intlist.split(":")
        
        #iterate through the entire list
        for intlist_dir in intlist_list:
            ret_val.append(intlist_dir)

    #check ACSROOT as well
    if os.environ.has_key('ACSROOT'):
        ret_val.append(os.environ['ACSROOT'])
    
    if DEBUG==1:
        print "ret_val:", ret_val
    
    return ret_val
# -------------------------------------------------------------------
def __searchSEDirs(filename):
    '''
    Helper function used to search through the SE-prescribed directory
    structure.
    '''
    #build up our list of directories
    se_dirs = __getSEDirs()
    head,tail = os.path.split(filename)
    
    for se_dir in se_dirs:
        filepath = os.path.join(se_dir, filename)

        if head:
            if os.path.exists(filepath):
                return(filepath)
        else:
            for w in os.walk(se_dir):
                if filename in w[1] or filename in w[2]:
                    return os.path.join(w[0], filename)
                
    #worst-case scenario we just return None
    return None
    
# ---------------------------------------------------------------------------
def findFile(filename):
    '''
    Searches for "filename" in current directory, INTROOT, or ACSROOT,
    searching in that order. This is intended to function like the C++
    version in "acsFindFile.cpp".
    
    Parameters:
    - fileName is the stringified file name you are looking for. An example
    could be "idl/maci.idl"
    
    Return: tuple composed of
    * file path (empty string, if file is not found)
    * file mode
    * file size
    * directory flag (non-zero iff file path is a directory; zero, otherwise)

    Use with caution.
    '''
    global DEBUG
    
    result = ('', '', 0, 0)
    
    if filename == None:
        return result
        
    # Absolute paths will not work with the following, so make sure that
    # we have a relative path.
    if os.path.isabs(filename):
        filename = filename[1:]

    if DEBUG:
        print filename

    # Check whether file exists in current directory
    head, tail = os.path.split(filename)
    filepath = os.path.abspath(tail)
    found = os.path.exists(filepath)

    if found == 0:
        # Check whether file exists in sub-directory of current directory
        cwd = os.getcwd()
        filepath = os.path.join(cwd, filename)
        found = os.path.exists(filepath)

        if found == 0:
            # Check whether file exists as sub-directory of current module
            head, tail = os.path.split(cwd)
            filepath = os.path.join(head, filename)
            found = os.path.exists(filepath)

            if found == 0:
                filepath = __searchSEDirs(filename)
                
    if (filepath!=None) and (os.path.exists(filepath)):
        filestat = os.stat(filepath)
        result = (filepath, 
                  filestat[stat.ST_MODE], 
                  filestat[stat.ST_SIZE], 
                  stat.S_ISDIR(filestat[stat.ST_MODE]))
    return result
# -----------------------------------------------------------

