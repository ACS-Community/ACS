/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2005 
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
* "@(#) $Id: loggingLoggable.cpp,v 1.1 2006/10/03 21:44:38 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-28  created 
*/

#include "loggingLoggable.h"
#include "loggingGetLogger.h"
#include "loggingACEMACROS.h"

static char *rcsId="@(#) $Id: loggingLoggable.cpp,v 1.1 2006/10/03 21:44:38 gchiozzi Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

// -------------------------------------------------------
namespace Logging {
    Loggable::Loggable() {
	ACS_CHECK_LOGGER;
	logger_m = Logger::getGlobalLogger();
    }

    Loggable::Loggable(const std::string &loggerName) {
	ACS_CHECK_LOGGER;
	logger_m = getNamedLogger(loggerName);
    }

    Loggable::Loggable(Logger::LoggerSmartPtr logger) {
	ACS_CHECK_LOGGER;
	logger_m = logger;
    }

    Loggable::~Loggable() {
    }

    Logger::LoggerSmartPtr Loggable::getLogger() const {
	return logger_m;
    }
	
};

/*___oOo___*/
