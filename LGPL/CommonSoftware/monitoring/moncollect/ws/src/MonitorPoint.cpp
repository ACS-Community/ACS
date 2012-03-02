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
* "@(#) $Id: MonitorPoint.cpp,v 1.4 2012/03/02 14:03:10 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: MonitorPoint.cpp,v 1.4 2012/03/02 14:03:10 tstaig Exp $";
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
	subscription_m = ACS::Subscription::_nil();
	curSeqPos_m = 0;
	//monitorBlob_m = new MonitorBlob;
	monitorBlob_m.propertyName = CORBA::string_dup(propertyName);
	monitorBlob_m.typeOfValue = typeOfData;
	monitorSuppressed_m = false;
	alarmSuppressed_m = false;
	valuePercentTrigger_m = 0;
	backLogSize_m = 32;
}

void MonitorPointBase::setPropertySerialNumber(serialNumberTypeSeq& sn)
{
		monitorBlob_m.propertySerialNumber = sn;
}//setSerialNumber

MonitorPointBase::~MonitorPointBase()
{
	AUTO_TRACE("MonitorPointBase::~MonitorPointBase");
}

template<>
char* initValue<char*>(unsigned int len)
{
   return new char[len+1];
}

/*___oOo___*/
