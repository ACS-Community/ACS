/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsQoStimeout.cpp,v 1.12 2008/07/14 11:43:40 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-08-24  created
*/

static char *rcsId="@(#) $Id: acsQoStimeout.cpp,v 1.12 2008/07/14 11:43:40 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsQoStimeout.h"

using namespace acsQoS;

Timeout::Timeout(unsigned long timeout) :
    timeout_m(timeout)
{
	try
	{
		CORBA::Object_var object = orb_m->resolve_initial_references ("PolicyCurrent");
		policyCurrent_m = CORBA::PolicyCurrent::_narrow (object.in());

		// check if there is already a timeout set
		CORBA::PolicyTypeSeq policySeq;
		policySeq.length(1);
		policySeq[0] = Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE;
		CORBA::PolicyList_var policyList = policyCurrent_m->get_policy_overrides(policySeq);

		if (policyList->length() > 0)
		{
			Messaging::RelativeRoundtripTimeoutPolicy_var m;
			m = Messaging::RelativeRoundtripTimeoutPolicy::_narrow((*policyList)[0]);

			TimeBase::TimeT oldTimeout = m->relative_expiry();

			ACS_DEBUG_PARAM ("Timeout::Timeout", "Constructing timeout; old timeout was: %lu", static_cast<unsigned long>(oldTimeout/10000));
			ACS_DEBUG_PARAM ("Timeout::Timeout", "Constructing timeout; new timeout is: %lu", timeout_m);

			previousPolicy_m.length(1);
			previousPolicy_m[0] = (*policyList)[0]->copy();
		}
		else {
			ACS_DEBUG_PARAM ("Timeout::Timeout", "Constructing timeout with value of: %lu and no previous timeout value.", timeout_m);
		}

		set();
	}
	catch (CORBA::Exception &cex)
	{
		acsQoSErrType::CanNotSetTimeoutExImpl ex(__FILE__, __LINE__, "Timeout::Timeout");
		ex.addData("Caused by CORBA exception", cex._name());
		throw ex;
	}
}//TimeOut

Timeout::~Timeout()
{
	try
	{
		CORBA::PolicyTypeSeq empty_policy_seq;
		CORBA::PolicyList_var overriden_policy_list = policyCurrent_m->get_policy_overrides(empty_policy_seq);

		// overriden_policy_list.length() has to be at least 1 since we override one in constructor
		unsigned int j = previousPolicy_m.length();

		if (j>0)
		{
			Messaging::RelativeRoundtripTimeoutPolicy_var m;
			m = Messaging::RelativeRoundtripTimeoutPolicy::_narrow(previousPolicy_m[0]);
			TimeBase::TimeT timeout = m->relative_expiry();
			ACS_DEBUG_PARAM ("Timeout::~Timeout", "Deleting timeout and resetting timeout to previous value of: %lu", (unsigned long)(timeout/10000));
		}
		else {
			ACS_DEBUG("Timeout::~Timeout", "Deleting timeout: no previous value exists, so not resetting.");
		}

		previousPolicy_m.length (overriden_policy_list->length() - 1 + j );

		for (unsigned int i=0u; i < overriden_policy_list->length(); i++)
		{
			if (overriden_policy_list[i]->policy_type() != Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE)
			{
				previousPolicy_m[j++] = overriden_policy_list[i]->copy();
			}
		}

		policyCurrent_m->set_policy_overrides(previousPolicy_m, CORBA::SET_OVERRIDE);

		for (j=0; j< previousPolicy_m.length(); j++)
		{
			previousPolicy_m[j]->destroy();
		}
	}
	catch (CORBA::Exception &cex)
	{
		acsQoSErrType::CanNotResetTimeoutExImpl ex(__FILE__, __LINE__, "Timeout::~Timeout");
		ex.addData("Caused by CORBA exception", cex._name());
		throw ex;
	}
}//~TimeOut

void Timeout::set()
{
	try
	{
		// convert timeout value into any
		// first, convert milliseconds to 100s of nsecs; see comments in header file (in setObjectTimeout method)
		// for more detailed explanation of this calculation.
		TimeBase::TimeT to = timeout_m * 10000;
		CORBA::Any anyTimeOut;
		anyTimeOut <<= to;

		policyList_m.length (1);
		policyList_m[0] = orb_m->create_policy (Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE, anyTimeOut);

		// we will set timeout at thread level
		policyCurrent_m->set_policy_overrides (policyList_m, CORBA::SET_OVERRIDE);

		policyList_m[0]->destroy();
	}
	catch(CORBA::Exception &cex)
	{
		acsQoSErrType::CanNotSetTimeoutExImpl ex(__FILE__, __LINE__, "Timeout::set");
		ex.addData("Caused by CORBA exception", cex._name());
		throw ex;
	}
}//set


void Timeout::setORBTimeout(unsigned long timeout, CORBA::ORB_ptr _orb)
{
	CORBA::ORB_ptr orb_p = ( CORBA::is_nil(_orb) ) ? Timeout::orb_m.in() : _orb;
	try
	{
		// convert timeout value into any
		TimeBase::TimeT to = timeout * 10000;    //convert to 100th of nsecs
		CORBA::Any anyTimeOut;
		anyTimeOut <<= to;

		CORBA::PolicyList policyList;
		policyList.length (1);
		policyList[0] = orb_p->create_policy (Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE, anyTimeOut);

		CORBA::Object_var obj = orb_p->resolve_initial_references("ORBPolicyManager");
		CORBA::PolicyManager_var policyManager =
		CORBA::PolicyManager::_narrow(obj.in());


		// we will set timeout at orb level
		policyManager->set_policy_overrides (policyList, CORBA::SET_OVERRIDE);

		policyList[0]->destroy();
	}
	catch(CORBA::Exception &cex)
	{
		acsQoSErrType::CanNotSetTimeoutExImpl ex(__FILE__, __LINE__, "Timeout::setORBTimeout");
		ex.addData("Caused by CORBA exception", cex._name());
		throw ex;
	}
}//setORBTimeout

// static methods
void Timeout::init(CORBA::ORB_ptr _orb)
{
	if ( Timeout::initialized_m != true )
	{
		Timeout::orb_m = CORBA::ORB::_duplicate(_orb);
		Timeout::initialized_m = true;
	}
}

bool Timeout::isInitialized()
{
    return Timeout::initialized_m;
}

void Timeout::done()
{
	if ( Timeout::initialized_m == true )
	{
		if (CORBA::is_nil(Timeout::orb_m.in()) != true )
		{
			CORBA::release(Timeout::orb_m._retn());
		}
		Timeout::initialized_m = false;
	}
}

CORBA::ORB_var Timeout::orb_m = CORBA::ORB::_nil();
bool Timeout::initialized_m = false;
/*___oOo___*/
