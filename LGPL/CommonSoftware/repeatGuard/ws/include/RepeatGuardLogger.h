#ifndef REPEATGUARDLOGGER_H
#define REPEATGUARDLOGGER_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2007 
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
* "@(#) $Id: RepeatGuardLogger.h,v 1.2 2007/02/27 09:14:35 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-02-26  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutilTimeStamp.h>
#include <loggingACSLogger.h>

#include "RepeatGuard.h"

namespace Logging {
        /** @class RepeatGuardLogger
         *  @brief Guard class against log repetitions.
         *
         *   This class implements a logger with protection against logging too often
         *
         *   
         */
        class RepeatGuardLogger:public ACSLogger{
                private:

                protected:
                        RepeatGuard *mGuard;
                public:
                        /** Constructor
                         *  @param interval minimum interval between allowing an action(i.e. check returns true)
                         *  @param maxRepetitions override minimum interval if maxRepetitions is reached.(0 disables this feature)
                         */
                        RepeatGuardLogger(const std::string &loggerName,unsigned int interval, unsigned int maxRepetitions=0);

                        ~RepeatGuardLogger();

                        //void log(const char *format, ...);
                        /** This method logs the given message only if, the conditions given in the
                         *  constructor are met. It accepts printf-like parameters.
                         *  @param format standard format of printf-like functions.
                         */
                        void logAndIncrement(const char *format, ...);

                        /** Appends the number of tries for this log
                         *   @param msg message to which append number of tries
                         */
                        virtual void appendCount(std::string &msg);
        };
};
#endif /*!REPEATGUARDLOGGER_H*/
