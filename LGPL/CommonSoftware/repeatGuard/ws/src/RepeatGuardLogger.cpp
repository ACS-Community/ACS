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
* "@(#) $Id: RepeatGuardLogger.cpp,v 1.2 2007/02/27 09:14:35 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-02-26  created 
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: RepeatGuardLogger.cpp,v 1.2 2007/02/27 09:14:35 nbarriga Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <sstream>

#include "RepeatGuardLogger.h"

namespace Logging{

        RepeatGuardLogger::RepeatGuardLogger(const std::string &loggerName,unsigned int interval, unsigned int maxRepetitions) : Logger(loggerName),
                ACSLogger(loggerName){
                        mGuard=new RepeatGuard(interval,maxRepetitions);
                }

        RepeatGuardLogger::~RepeatGuardLogger(){
                delete mGuard;
        }

        //void RepeatGuardLogger::log(const char *format, ...){}

        void RepeatGuardLogger::logAndIncrement(const char *format, ...){
#define MAX_MSG_SIZE 512
                if(mGuard->checkAndIncrement()){
                        char buffer[MAX_MSG_SIZE];
                        va_list arglist;
                        va_start (arglist, format);
                        vsnprintf(buffer,MAX_MSG_SIZE,format,arglist);
                        std::string msg(buffer);
                        appendCount(msg);
                        Logger::log(LM_INFO,msg);
                }
        }

        void RepeatGuardLogger::appendCount(std::string &msg){
                msg+="; message repeated ";
                std::stringstream strstr;
                strstr<<mGuard->count();
                msg+=strstr.str();
                msg+=" times.";
        }

}
/*___oOo___*/
