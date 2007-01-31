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
* "@(#) $Id: ACSLogTS.cpp,v 1.1 2007/01/31 14:03:39 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-01-30  created 
*/


#include "vltPort.h"

static char *rcsId="@(#) $Id: ACSLogTS.cpp,v 1.1 2007/01/31 14:03:39 nbarriga Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <ACSLogTS.h>
#include <acstimeTimeUtil.h>

ACSLogTS::ACSLogTS(Logging::BaseLog::Priority priority,
			string file,
			unsigned long line,
                        string routine,
                        string name,
                        string shortDescription){
	
	this->priority=priority;
	this->file=file;
	this->line=line;
	this->routine=routine;
	this->name=name;
	this->shortDescription=shortDescription;
}

ACSLogTS::~ACSLogTS(){

}

void ACSLogTS::log(){
	Logging::BaseLog::LogRecord lr;
	lr.priority=this->priority;
	lr.message=this->shortDescription;
	lr.file=this->file;
	lr.line=this->line;
	lr.method=this->routine;
	lr.timeStamp=baci::getTimeStamp();
	LoggingProxy::AddData("logName",this->name.c_str());
	for(unsigned int i=0;i<members.length();i++){
		LoggingProxy::AddData(members[i].name.in(),members[i].value.in());
	}
	LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT);
	getLogger()->log(lr);
}


/*___oOo___*/
