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
* "@(#) $Id: RepeatGuardLogger.cpp,v 1.1 2007/02/26 16:28:11 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-02-26  created 
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: RepeatGuardLogger.cpp,v 1.1 2007/02/26 16:28:11 nbarriga Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


#include "RepeatGuardLogger.h"

namespace Logging{

        RepeatGuardLogger::RepeatGuardLogger(const std::string &loggerName,unsigned int interval, unsigned int maxRepetitions) : Logger(loggerName),
                ACSLogger(loggerName){
                        mGuard=new RepeatGuard(interval,maxRepetitions);
                }

        RepeatGuardLogger::~RepeatGuardLogger(){
                delete mGuard;
        }

        void RepeatGuardLogger::log(const char *format, ...){
#define MAX_MSG_SIZE 512
                char msg[MAX_MSG_SIZE];
                va_list arglist;
                va_start (arglist, format);
                vsnprintf(msg,MAX_MSG_SIZE,format,arglist);
                Logger::log(LM_INFO,msg);
        }

        void RepeatGuardLogger::logAndIncrement(){

        }

}
/*___oOo___*/
