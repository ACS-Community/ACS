/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005 
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
* "@(#) $Id: testCDBProps.cpp,v 1.2 2010/02/26 18:13:38 utfsm Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-25  created
*/

#include <baci.h>
#include <acscomponentImpl.h>
#include "testCDBProps.h"
#include "acsddsncCDBProperties.h"

using namespace baci;

CDBPropsCompImpl::CDBPropsCompImpl(const ACE_CString &name, 
			maci::ContainerServices *containerServices) : 
		ACSComponentImpl(name, containerServices)
{
	ACS_TRACE("::CDBPropsCompImpl::CDBPropsCompImpl");
}
CORBA::Long
CDBPropsCompImpl::runTest()
{
	ACS_STATIC_SHORT_LOG((LM_INFO,
 				  "CDBPropsCompImpl::runTest",
                                  "Starting the tests"));

	CORBA::Long result = 0;
	CORBA::String_var channel = "blar";
	CORBA::String_var notChannel = "NotInCDB";
	
	//getCDB	
	CDB::DAL_var cdb = ddsnc::CDBProperties::getCDB();
	
	if (CORBA::is_nil(cdb.in()))
            return -1;
	//-----------------------------------------------
	//cdbChannelConfigExists
	bool value;
	value = ddsnc::CDBProperties::cdbChannelConfigExists(channel);
	if(value)
	    result += 1;
	
	value = ddsnc::CDBProperties::cdbChannelConfigExists(notChannel);
	if(!value)
	    result += 1;
	//-----------------------------------------------
	
	// QoS Props
	//first try on one that exists in the CDB
	DDS::QosPolicyCountSeq tmp;
	tmp = ddsnc::CDBProperties::getCDBQoSProps(channel);
	ACS_STATIC_SHORT_LOG((LM_INFO,
 				  "CDBPropsCompImpl::runTest",
                                  "getCDBQoSProps with an existent channel"));
	unsigned int j=0;
	for (unsigned int i=0; i <tmp.length(); i++)
	    {
	    if(tmp[i].policy_id > 0)
		j += 1;
	    }
	if(j == tmp.length())
	{
		result += 1;
	}
	//next try on one that is not in the CDB
	tmp = ddsnc::CDBProperties::getCDBQoSProps(notChannel);
	ACS_STATIC_SHORT_LOG((LM_INFO,
 				  "CDBPropsCompImpl::runTest",
                                  "getCDBQoSProps with an nonexistent channel"));
	j=0;
	for (unsigned int i=0; i <tmp.length(); i++)
	    {
	    if(tmp[i].policy_id > 0)
		j += 1;
	    }
	if(j == 0)
	{
		result += 1;
	}                   	
	//-----------------------------------------------
	// fix
	return result;	    
}

CDBPropsCompImpl::~CDBPropsCompImpl()
{

}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(CDBPropsCompImpl)
/* ----------------------------------------------------------------*/
