/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
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
* 
*/

#include <MountImpl.h>

using namespace acscomponent;
using namespace testManager;

using std::string;
using std::auto_ptr;

MountImpl::MountImpl(const ACE_CString &name,maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices)
{
	ACS_TRACE("::MountImpl::MountImpl");

}

MountImpl::~MountImpl()
{
	ACS_TRACE("::MountImpl::~MountImpl");
}

void MountImpl::objfix(CORBA::Double az, CORBA::Double el)
{
	AUTO_TRACE("MountImpl::objfix");
	ACS_SHORT_LOG((LM_INFO,"Pointing to [%f,%f]",az,el));
}

/**
 *  Life cycle method
 */
void MountImpl::initialize() {
	AUTO_TRACE("MountImpl::initialize");
	sleep(1);
}

/**
 *  Life cycle method
 */
void MountImpl::execute() {
	AUTO_TRACE("MountImpl::execute");
}

/**
 *  Life cycle method
 */
void MountImpl::cleanUp() {
	AUTO_TRACE("MountImpl::cleanUp");
}

 /**
  *  Life cycle method
  */
void MountImpl::aboutToAbort() {
	AUTO_TRACE("MountImpl::aboutToAbort");
}

/* --------------- [ MACI DLL support functions ] -----------------*/

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(MountImpl)
/* ----------------------------------------------------------------*/
