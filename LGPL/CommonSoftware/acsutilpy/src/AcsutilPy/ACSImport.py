#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2007 
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
# The 'knee' module, from which this module is derived, is
# Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006, 2007
# Python Software Foundation; All Rights Reserved.  It is used
# under the terms of the Python Software Foundation License Version 2.
# 
# No changes were made to Python during the preparation of this
# module nor are any required to use it.
#
# "@(#) $Id: ACSImport.py,v 1.4 2007/06/11 21:49:42 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstr  2007-02-20  created
#
"""A customized hierarchical module import for ACS.

In ACS, Python modules from the same package can reside in several
places in the PYTHONPATH, such as INTROOT, INTLIST and ACSROOT.
The default Python import will stop searching for modules when it
finds the first instance of the package directory.  As a result,
modules stored in the other parts of the tree are never found.

The replacement function preserves the hierarchical structure
of Python modules and supports both 'import foo' and
'from foo import bar' syntax.

It is based on the 'knee' module that appears in the Python demo source
tree.  

"""
#------------------------------------------------------------------------------
__revision__ = "$Id: ACSImport.py,v 1.4 2007/06/11 21:49:42 agrimstrup Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import sys
import os
import imp
import inspect
import __builtin__
#------------------------------------------------------------------------------

def searchingImport(name, globals=None, locals=None, fromlist=None):
    """Replacement for Python default import

    Arguments:
    name -- Name of the module to be imported (No default)

    Keyword arguments:
    globals -- Dictionary containing the globally defined names
    (Default: None)
    locals -- Dictionary containing the locally defined names
    (Default: None)
    fromlist -- List of classes to be imported (Default: None)

    Returns:
    m -- The module object imported

    Exceptions:
    ImportError is thrown if the module is not found.

    """
    try:
        m = _original_import(name, globals, locals, fromlist)
        if inspect.ismodule(m) and not imp.is_builtin(m.__name__) and not hasattr(m,'__file__'):
            reload(m)
    except ImportError:
        parent = _determineParent(globals)
        q, tail = _findHeadPackage(parent, name)
        m = _loadTail(q, tail)
        if not fromlist:
            return q
        if hasattr(m, "__path__"):
            _ensureFromList(m, fromlist)
    return m

def _determineParent(globals):
    """Determine the parent of this module

    Arguments:
    globals -- Dictionary containing the globally defined names

    Returns:
    parent -- The module object of the parent

    """
    if not globals or  not globals.has_key("__name__"):
        return None
    pname = globals['__name__']
    if globals.has_key("__path__"):
        parent = sys.modules[pname]
        assert globals is parent.__dict__
        return parent
    if '.' in pname:
        i = pname.rfind('.')
        pname = pname[:i]
        parent = sys.modules[pname]
        assert parent.__name__ == pname
        return parent
    return None

def _findHeadPackage(parent, name):
    """Find and load the package root

    Arguments:
    parent -- Parent module of this package
    name -- Name of the module to be loaded

    Returns:
    q -- The module object for the package root
    tail -- The remainder of the module tree to be loaded

    Exceptions:
    ImportError is raised if the module was not found.

    """
    if '.' in name:
        i = name.find('.')
        head = name[:i]
        tail = name[i+1:]
    else:
        head = name
        tail = ""
    if parent:
        qname = "%s.%s" % (parent.__name__, head)
    else:
        qname = head
    q = _importModule(head, qname, parent)
    if q: return q, tail
    if parent:
        qname = head
        parent = None
        q = _importModule(head, qname, parent)
        if q: return q, tail
    raise ImportError, "No module named " + qname

def _loadTail(q, tail):
    """Load the remainder of the module hierarchy

    Arguments:
    q -- Root module of the hierarchy
    tail -- The remainder of the module tree

    Returns:
    m -- The module object for the leaf module

    Exceptions:
    ImportError is raised if any module in the hierarchy
    cannot be found."""
    m = q
    while tail:
        i = tail.find('.')
        if i < 0: i = len(tail)
        head, tail = tail[:i], tail[i+1:]
        mname = "%s.%s" % (m.__name__, head)
        m = _importModule(head, mname, m)
        if not m:
            raise ImportError, "No module named " + mname
    return m

def _ensureFromList(m, fromlist, recursive=0):
    for sub in fromlist:
        if sub == "*":
            if not recursive:
                try:
                    all = m.__all__
                except AttributeError:
                    pass
                else:
                    _ensureFromList(m, all, 1)
            continue
        if sub != "*" and not hasattr(m, sub):
            subname = "%s.%s" % (m.__name__, sub)
            submod = _importModule(sub, subname, m)
            if not submod:
                raise ImportError, "No module named " + subname

def _importModule(partname, fqname, parent):
    try:
        return sys.modules[fqname]
    except KeyError:
        pass

    m = None
    try:
        fp, pathname, stuff = imp.find_module(partname,
                                              parent and parent.__path__)
    except ImportError:
        try:
            dirpath = parent.__name__.replace('.','/')
        except AttributeError:
            dirpath = ""
        for dir in sys.path:
            pkgpath = os.path.join(dir, dirpath)
            if os.access(pkgpath, os.F_OK):
                filepath = pkgpath + '/' + partname + '.py'
                if os.access(filepath, os.R_OK):
                    m = imp.load_source(fqname, filepath)
            if m: break
        if m is None:
            return None
        else:
            if parent:
                setattr(parent, partname, m)
            return m
            
    try:
        m = imp.load_module(fqname, fp, pathname, stuff)
    finally:
        if fp: fp.close()
        
    if parent:
        setattr(parent, partname, m)
    return m


# Save the original hooks
_original_import = __builtin__.__import__

# Now install our hooks
__builtin__.__import__ = searchingImport


#
# ___oOo___
