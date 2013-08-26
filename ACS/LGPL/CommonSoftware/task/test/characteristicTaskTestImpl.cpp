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
* "@(#) $Id: characteristicTaskTestImpl.cpp,v 1.10 2008/10/01 03:07:07 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-09-17  created 
*/

#include <characteristicTaskTestImpl.h>
#include <iostream>
#include <ACSErrTypeCommon.h>

/* ----------------------------------------------------------------*/
CharacteristicTaskTestImpl::CharacteristicTaskTestImpl(const ACE_CString &name,  maci::ContainerServices *containerServices) :
    baci::CharacteristicComponentImpl(name, containerServices)
{
    ACS_TRACE("::CharacteristicTaskTestImpl::CharacteristicTaskTestImpl");
}
/* ----------------------------------------------------------------*/
CharacteristicTaskTestImpl::~CharacteristicTaskTestImpl()
{
    ACS_TRACE("::CharacteristicTaskTestImpl::~CharacteristicTaskTestImpl");
    ACS_DEBUG_PARAM("::CharacteristicTaskTestImpl::~CharacteristicTaskTestImpl", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/
void
CharacteristicTaskTestImpl::run (const ACS::StringSequence & params, const char* fileName)
{
    /*
    if (strcmp(params, "throw") == 0)
	throw taskErrType::TaskRunFailureExImpl(__FILE__, __LINE__, "CharacteristicTaskTestImpl::run").getTaskRunFailureEx();
    */
	for(CORBA::ULong i = 0; i < params.length(); i++) 
	{
		if(NULL != params[i]) {
			std::string currStr(params[i]);
			std::cout << currStr << std::endl;
		}
	}
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
//#define ACS_DLL_UNMANGLED_EXPORT extern;
MACI_DLL_SUPPORT_FUNCTIONS(CharacteristicTaskTestImpl)
/* ----------------------------------------------------------------*/


/*___oOo___*/

