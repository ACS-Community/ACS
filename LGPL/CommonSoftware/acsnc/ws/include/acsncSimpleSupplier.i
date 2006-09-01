#ifndef SIMPLE_SUPPLIER_I
#define SIMPLE_SUPPLIER_I
/*    @(#) $Id: acsncSimpleSupplier.i,v 1.23 2006/09/01 02:20:54 cparedes Exp $
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Associated Universities Inc., 2002 
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

/** @file acsncSimpleSupplier.i
 *  Header file for the Supplier-derived class that should be used from within
 *  components to publish events.
 */

namespace nc {
//----------------------------------------------------------------------
template <class T> void 
SimpleSupplier::publishData(T data)
    throw (CORBAProblemEx)
{
    try
	{
	any_m <<= data;
	Supplier::publishEvent(any_m);
	}
    catch(CORBAProblemEx)
	{
	//an exception from subclasses...OK to rethrow
	throw;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,
		       "SimpleSupplier::publishData(...) %s channel - unknown error!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,
						    __LINE__,
						    "nc::SimpleSupplier::publishData");
	throw err;
	}
}
//----------------------------------------------------------------------
 }; 
#endif
