#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2005 
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
# "@(#) $Id: Visualizer.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when      what
# --------  --------  -------------------------------------------------------
# dfugate  2005-11-01  created
#
'''
Provides functions used to visually represent CORBA-based objects.
'''
#----------------------------------------------------------------------------
import omniORB
import types
#----------------------------------------------------------------------------
#list of object members that should not be printed out
IGNORE_LIST = ['_NP_RepositoryId', '__doc__', '__init__', '__module__', 
               '_members', '_values']
#standard amount of whitespace used by getTextRepresentation function
STD_WHITESPACE  = "  "
#----------------------------------------------------------------------------
def getTextRepresentation(some_obj):
    '''
    Returns a textual representation of the some_obj. Preferably some_obj 
    is an instance of some CORBA stub class, but this does not necessarily
    have to be the case. 
    
    Params: some_obj - an instance of a Python object.
    
    Returns: a list of strings which represent some_obj
    
    Raises: ???
    '''
    #initialize the return value
    ret_val = []

    #beginning is always the IFR ID of the object
    try:
        ret_val.append(some_obj._NP_RepositoryId)
    except:
        #if it's a pure Python object just throw in the Python type
        ret_val.append(str(type(some_obj)))

    #get a list of all the simple attributes
    attrib_list = []
    
    try:
        #if it's CORBA and hasn't been imported yet
        #the following will work
        attrib_list = some_obj._members
    except:
        #whoops, must be some sort of known ACS CORBA
        #struct. fine, do this the hard way
        
        for attrib in dir(some_obj):
            #skip over things that should be ignored
            if IGNORE_LIST.count(attrib) != 0:
                continue
            #get this far and it's OK to include it
            attrib_list.append(attrib)
    
    #get the individual values for each attribute
    for entry_name in attrib_list:

        #get the real value
        entry = some_obj.__dict__[entry_name]

        #interesting...a complex struct is defined within it...
        class_name = entry.__class__.__name__
        if (type(entry)==types.InstanceType) and (class_name=="UnknownStruct"):
            #use recursion
            ret_val.append(STD_WHITESPACE + entry_name + ":")
            ret_val = ret_val + map(lambda x: STD_WHITESPACE + x, 
                                     getTextRepresentation(entry))
            continue
        
        #must be some simple type...good.
        ret_val.append(STD_WHITESPACE +
                       entry_name +
                       "=" +
                       str(entry))
            
    return ret_val