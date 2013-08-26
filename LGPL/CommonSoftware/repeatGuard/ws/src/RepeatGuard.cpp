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
* "@(#) $Id: RepeatGuard.cpp,v 1.9 2008/05/21 20:25:08 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-02-20  created 
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: RepeatGuard.cpp,v 1.9 2008/05/21 20:25:08 nbarriga Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


#include "RepeatGuard.h"

RepeatGuard::RepeatGuard(ACS::TimeInterval _interval, unsigned int _maxRepetitions, bool or_or_and):
        counter(0),
        counterAtLastCheck(0),
        maxRepetitions(_maxRepetitions),
        method(or_or_and),
        firstTime(true),
        interval(_interval),
        lastTime(0)
{
        if(_interval<=0){
            method=repeatguard::COUNTER;
        }
        if(_maxRepetitions<=0){
            method=repeatguard::TIMER;
        }
}

RepeatGuard::~RepeatGuard(){

}

bool RepeatGuard::check(){
        ACS::ThreadSyncGuard guard(&mutex);
        ACS::Time currentTime=getTimeStamp();
        if(firstTime){
                firstTime=false;
                counterAtLastCheck=counter;
                counter=0;
                lastTime=currentTime;
                return true;

        }
        switch(method){
                case repeatguard::AND:
                        if(lastTime+interval<=currentTime&&counter>=maxRepetitions){
                                counterAtLastCheck=counter;
                                counter=0;
                                lastTime=currentTime;
                                return true;
                        }
                        return false;
                        break;
                case repeatguard::OR:
                        if(lastTime+interval<=currentTime||counter>=maxRepetitions){
                                counterAtLastCheck=counter;
                                counter=0;
                                lastTime=currentTime;
                                return true;
                        }
                        return false;
                        break;
                case repeatguard::TIMER:
                        if(lastTime+interval<=currentTime){
                                counterAtLastCheck=counter;
                                counter=0;
                                lastTime=currentTime;
                                return true;
                        }
                        return false;
                        break;
                case repeatguard::COUNTER:
                        if(counter>=maxRepetitions){
                                counterAtLastCheck=counter;
                                counter=0;
                                lastTime=currentTime;
                                return true;
                        }
                        return false;
                        break;
        }
	return false;
}

bool RepeatGuard::checkAndIncrement(){
        ACS::ThreadSyncGuard guard(&mutex);
        counter++;
	return check();
}

void RepeatGuard::increment(){
        ACS::ThreadSyncGuard guard(&mutex);
	counter++;
}

unsigned int RepeatGuard::count(){
	return counterAtLastCheck;
}

void RepeatGuard::reset(){
        ACS::ThreadSyncGuard guard(&mutex);
	counter=0;
	counterAtLastCheck=0;
	lastTime=0;
        firstTime=true;
}

void RepeatGuard::reset(ACS::TimeInterval interval, unsigned int maxRepetitions, bool or_or_and){
        ACS::ThreadSyncGuard guard(&mutex);
        //method=OR;
        method = or_or_and;
        if(interval<=0)method=repeatguard::COUNTER;
        if(maxRepetitions<=0)method=repeatguard::TIMER;

	this->maxRepetitions=maxRepetitions;
	this->interval=interval;
	counter=0;
	counterAtLastCheck=0;
	lastTime=0;
        firstTime=true;
}
/*___oOo___*/
