#ifndef test_supplier_H
#define test_supplier_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: testSupplier.h,v 1.1 2005/11/18 00:17:58 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-11-15  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <basencSupplier.h>
#include <acscommonC.h>

class TestSupplier : public BaseSupplier
{
  public:
    
    TestSupplier(const char* channelName) : BaseSupplier(channelName)
	{}

    void
    publishEvent(const char* someData)
	{
	    CosNotification::StructuredEvent event;
	    populateHeader(event);

	    acsnc::EventDescription descrip;
	    descrip.timestamp = 130183200000000000ULL;
	    descrip.count     = 7ULL;
	    descrip.name = "Unknown";
	    event.remainder_of_body <<= descrip;
	    
	    event.filterable_data.length(1);
	    event.filterable_data[0].value <<= CORBA::string_dup(someData);
	    BaseSupplier::publishEvent(event);
	}

  protected:
    /**
     * Overridden.
     */
    virtual const char* 
    getChannelKind() 
	{ return acscommon::NC_KIND; }

    virtual const char*
    getEventType()
	{ return "string"; }

    virtual ~TestSupplier(){}

};


#endif /*!test_supplier_H*/
