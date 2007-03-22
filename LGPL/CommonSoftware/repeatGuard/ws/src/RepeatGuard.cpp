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
* "@(#) $Id: RepeatGuard.cpp,v 1.3 2007/03/22 10:55:29 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-02-20  created 
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: RepeatGuard.cpp,v 1.3 2007/03/22 10:55:29 gchiozzi Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


#include "RepeatGuard.h"

RepeatGuard::RepeatGuard(unsigned int interval, unsigned int maxRepetitions, bool or_or_and){
        method=or_or_and;
        if(interval==0)method=COUNTER;
        if(maxRepetitions==0)method=TIMER;

	this->maxRepetitions=maxRepetitions;
	this->interval=interval*10000000;
	counter=0;
	counterAtLastCheck=0;
	lastTime=0;
}

RepeatGuard::~RepeatGuard(){

}

bool RepeatGuard::check(){
        switch(method){
                case AND:
                        if(lastTime+interval<=getTimeStamp()&&counter>=maxRepetitions){
                                counterAtLastCheck=counter;
                                counter=0;
                                lastTime=getTimeStamp();
                                return true;
                        }
                        return false;
                        break;
                case OR:
                        if(lastTime+interval<=getTimeStamp()||counter>=maxRepetitions){
                                counterAtLastCheck=counter;
                                counter=0;
                                lastTime=getTimeStamp();
                                return true;
                        }
                        return false;
                        break;
                case TIMER:
                        if(lastTime+interval<=getTimeStamp()){
                                counterAtLastCheck=counter;
                                counter=0;
                                lastTime=getTimeStamp();
                                return true;
                        }
                        return false;
                        break;
                case COUNTER:
                        if(counter>=maxRepetitions){
                                counterAtLastCheck=counter;
                                counter=0;
                                lastTime=getTimeStamp();
                                return true;
                        }
                        return false;
                        break;
        }
	return false;
}

bool RepeatGuard::checkAndIncrement(){

        counter++;
        return check();
}

void RepeatGuard::increment(){
	counter++;
}

unsigned int RepeatGuard::count(){
	return counterAtLastCheck;
}

void RepeatGuard::reset(){
	counter=0;
	counterAtLastCheck=0;
	lastTime=0;
}

void RepeatGuard::reset(unsigned int interval, unsigned int maxRepetitions, bool or_or_and){
        method=OR;
        if(interval==0)method=COUNTER;
        if(maxRepetitions==0)method=TIMER;

	this->maxRepetitions=maxRepetitions;
	this->interval=interval*10000000;
	counter=0;
	counterAtLastCheck=0;
	lastTime=0;
}
/*___oOo___*/
