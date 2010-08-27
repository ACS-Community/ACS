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
 *
 * "@(#) "
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * javarias  May 7, 2010  	 created
 */

#include "loggingStdoutlayout.h"
#include "loggingACSCategory.h"
#include "loggingXmlLayout.h"
#include <log4cpp/OstreamAppender.hh>
#include <log4cpp/Appender.hh>
#include <log4cpp/BasicLayout.hh>

int main (int argc, char * argv[])
{
	::log4cpp::Appender *app1 = new ::log4cpp::OstreamAppender("STDOUT Appender", &::std::cout);
	::log4cpp::Appender *app2 = new ::log4cpp::OstreamAppender("XML Appender", &::std::cout);
	app1->setLayout(new ::logging::ACSstdoutLayout());
	app2->setLayout(new ::logging::ACSXmlLayout());
	logging::ACSCategory& cat = ::logging::ACSCategory::getInstance("Logger Instance");
	cat.addAppender(app1);
	cat.addAppender(app2);
	cat.setPriority(::log4cpp::Priority::TRACE);
	for(int i=0; i < 100; i++)
		cat.log("Message",::log4cpp::Priority::CRITICAL, __PRETTY_FUNCTION__, __FILE__, __LINE__, "localhost", "", "Me");
	//cat.alert("lala");

	return 0;
}
