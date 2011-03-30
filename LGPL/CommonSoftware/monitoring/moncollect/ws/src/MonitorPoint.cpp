/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2009
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
* "@(#) $Id: MonitorPoint.cpp,v 1.2 2011/03/30 18:11:18 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: MonitorPoint.cpp,v 1.2 2011/03/30 18:11:18 tstaig Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "MonitorPoint.h"

using namespace TMCDB;


MonitorPointBase::MonitorPointBase(const char *propertyName, const ACS::TimeInterval &archivingInterval, TMCDB::DataValueType typeOfData, MonitorBlob& mb) :
	propertyName_m(propertyName),
	archivingInterval_m(archivingInterval),
	monitorBlob_m(mb)
{
	AUTO_TRACE("MonitorPointBase::MonitorPointBase");

	monitor_m = ACS::Monitor::_nil();
	curSeqPos_m = 0;
	//monitorBlob_m = new MonitorBlob;
	monitorBlob_m.propertyName = CORBA::string_dup(propertyName);
	monitorBlob_m.typeOfValue = typeOfData;
	suppressed_m = false;
	valuePercentTrigger_m = 0;
}

void MonitorPointBase::setPropertySerialNumber(serialNumberTypeSeq& sn)
{
		monitorBlob_m.propertySerialNumber = sn;
}//setSerialNumber

void MonitorPointBase::activate(maci::ContainerServices *cs)
{
	AUTO_TRACE("MonitorPointBase::activate");
	try
	{
		callback_m = cs->activateOffShoot(this);
		this->_remove_ref(); //Decrease ref count to 1
	}
	catch(CORBA::Exception &ex)
	{
		ACE_PRINT_EXCEPTION(ex, "MonitorPointBase::activate");
	}
}//activate

MonitorPointBase::~MonitorPointBase()
{
	AUTO_TRACE("MonitorPointBase::~MonitorPointBase");
}

/*___oOo___*/
