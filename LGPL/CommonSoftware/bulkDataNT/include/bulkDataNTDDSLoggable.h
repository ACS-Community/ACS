#ifndef BULKDATA_NT_DDS_LOGGABLE
#define BULKDATA_NT_DDS_LOGGABLE

/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
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
* "@(#) $Id: bulkDataNTDDSLoggable.h,v 1.1 2011/11/10 11:16:53 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/


#include <loggingLoggable.h>
#include <logging.h>

/**
 * The purpose of this class is to make possible to log from classes that are used by threads created by DDS,
 * where we do not have possibility to do the (per-thread) initialization of the logging system.
 * The class just override getLogger and inside detects if the logging has been already initialized for the thread
 *  if not it does the initialization.
 *  TBD: at the moment the logs do not go to centralized logger. It has to be added functionality to set centralized logger.
 */
class BulkDataNTDDSLoggable
  : public Logging::Loggable
{
public:
  //Constructor
	  BulkDataNTDDSLoggable (const std::string &loggerName) :	Logging::Loggable(loggerName),
	  logger_mp(0), loggerInitCount_m(0){}

  //destructor
  virtual ~BulkDataNTDDSLoggable ();

  /// we override getLogger, so that we can initialize logging system if needed
 virtual Logging::Logger::LoggerSmartPtr getLogger ();
private:
  LoggingProxy *logger_mp; //we need separate logger, because we are in separate thread  ...
  // ... other is one logger for all DDS reader thread
  unsigned int loggerInitCount_m; // we need to count how many time we call LoggerProxy::init
};

#endif //BULKDATA_NT_DDS_LOGGABLE
