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
# "@(#) $Id: Visualizer.py,v 1.2 2005/11/02 21:52:22 dfugate Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# dfugate  2005-11-01  created
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
import omniORB
import types
#--------------------------------------------------------------------------------------------
ignore_list = ['_NP_RepositoryId', '__doc__', '__init__', '__module__', '_members', '_values']
std_spaces  = "  "
#--------------------------------------------------------------------------------------------
def getTextRepresentation(some_obj):
    '''
    Returns a textual representation of the some_obj.
    '''
    ret_val = []

    #beginning is always the IFR ID of the object
    try:
        ret_val.append(some_obj._NP_RepositoryId)
    except:
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
            if ignore_list.count(attrib)!=0:
                continue
            #get this far and it's OK to include it
            attrib_list.append(attrib)

    for entry_name in attrib_list:

        #get the real value
        entry = some_obj.__dict__[entry_name]

        #interesting...a complex struct is defined within it...
        if (type(entry)==types.InstanceType) and (entry.__class__.__name__=="UnknownStruct"):
            #use recursion
            ret_val.append(std_spaces + entry_name + ":")
            ret_val = ret_val + map(lambda x: std_spaces + x, getTextRepresentation(entry))
            continue
        

        #must be some simple type...good.
        ret_val.append(std_spaces +
                       entry_name +
                       "=" +
                       str(entry))
            
    return ret_val
    


#
# ___oOo___
#----------------------------------------------------
#from Acspy.Nc.Consumer          import Consumer

#last = None
#def fridgeDataHandler(someParam):
#    global last
#    last = someParam
#    print getTextRepresentation(someParam)
#
#g = Consumer("fridge")
#g.addSubscription("temperatureDataBlockEvent", fridgeDataHandler)
#g.addSubscription("garbage", fridgeDataHandler)
#g.addSubscription("complexStruct", fridgeDataHandler)
#g.consumerReady()
