// @(#) $Id: testEpoch.cpp,v 1.16 2009/03/13 10:05:37 bjeram Exp $
//
// Copyright (C) 2001
// Associated Universities, Inc. Washington DC, USA.
//
// Produced for the ALMA project
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// Correspondence concerning ALMA Software should be addressed as follows:
//   Internet email: alma-sw-admin@nrao.edu

//
// DESCRIPTION
// An ACS client to test the Epoch interface.
//

#include <maciSimpleClient.h>
#include <baci.h>
#include "acstimeClockImpl.h"
#include "acstimeDurationHelper.h"
#include "acstimeEpochHelper.h"

 using namespace maci;
 using namespace ACS;
//------------------------------------------------------------------------------
#include "helperFuncs.c"
//------------------------------------------------------------------------------

int main(int argc,char *argv[])
{    
    
    try
	{
	// create instance of SimpleClient and init() it.
	SimpleClient ci;
	if(ci.init(argc,argv) == 0)
	    {
	    ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	    return -1;
	    }
	
	// ???
	ci.login();
	
	// get reference to Clock device
	CORBA::Object_var obj = ci.get_object("CLOCK1", 0, true);
	if(CORBA::is_nil(obj.in())) 
	    {
	    ACS_SHORT_LOG((LM_DEBUG,"Cannot get CLOCK1 Object"));
	    return -1;
	    }
	
	// narrow object to obtain Clock reference
	acstime::Clock_var c_var = acstime::Clock::_narrow(obj.in());
	
	if (CORBA::is_nil(c_var.in())) 
	    {
	    std::cerr << "Nil Clock reference" << std::endl;
	    return -1;
	    }
	
	ACSErr::Completion_var completion;
	ACS::RWlong_var m_array2TAI = c_var->array2TAI();
	ACS::RWlong_var m_TAI2UTC = c_var->TAI2UTC();
	ACS::ROuLongLong_var m_now = c_var->now();

	EpochHelper *e1 = new EpochHelper();
	
	EpochHelper *e2 = new EpochHelper();
	
	
	// create Duration object
	DurationHelper *d1 = new DurationHelper();
	
	
        // create an Epoch structure
	acstime::Epoch eout;

        // format string using all possible
	char allOut[] = "%G %g %x\n%Y %y %m %h %j %d %e %w %a %H:%M:%S.%q %1q %2q %3q %4q %5q %6q";

        // current time
	eout.value = m_now->get_sync(completion.out());
	
	e1->value(eout);
	
	std::string pStr = e1->toString(acstime::TSArray,
				  "%x",
				  m_array2TAI->get_sync(completion.out()), 
				  m_TAI2UTC->get_sync(completion.out()));
	
        cout << "Current time is " << pStr.c_str() << endl;

	// test Epoch range & toUTUdate(), toJulianYear()
	eout.value = 0xFFFFFFFFFFFFFFFALL;
	e1->value(eout);
	
        pStr = e1->toString(acstime::TSArray,"", m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	double utc = e1->toUTCdate(m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
	double julian = e1->toJulianYear(m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));

	long double mjdSeconds = e1->toMJDseconds();
	
	cout << utc << " " << julian << " " << mjdSeconds << endl;

	eout.value = 0LL;
	e1->value(eout);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	utc = e1->toUTCdate(m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
	julian = e1->toJulianYear(m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
	mjdSeconds = e1->toMJDseconds();
	cout << utc << " " << julian << " " << mjdSeconds << endl;
	
	e1->fromString(acstime::TSArray,
                          "60038-3-11T5:36:10.955161");
	
	eout = e1->value();
	
	printLongLong(eout.value);
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	e1->fromString(acstime::TSArray,
                          "1582-10-15T00:00:00.000000");
	
	eout = e1->value();
	
	printLongLong(eout.value);
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	
	e1->fromString(acstime::TSArray,
                          "1995-4-28T17:23:15.654321");
	
	eout = e1->value();
	
	printLongLong(eout.value);
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	utc = e1->toUTCdate(m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
	julian = e1->toJulianYear(m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));

	mjdSeconds = e1->toMJDseconds();
	
	cout << utc << " " << julian << " " << mjdSeconds << endl;

	// test Epoch usec implicit trailing zeroes
	e1->fromString(acstime::TSArray,
                          "1776-7-4T17:23:15.5");
	
        pStr = e1->toString(acstime::TSArray,"%x",m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->fromString(acstime::TSArray,
                          "2345-6-7T08:09:00.103");
	
        pStr = e1->toString(acstime::TSArray,"%x",m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->fromString(acstime::TSArray,
                          "2001-9-11T06:54:32.0506");
	
        pStr = e1->toString(acstime::TSArray,"%x",m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

        // test Epoch.add()
	e1->fromString(acstime::TSArray,
                          "1943-04-05 05:36:10.955161");
	
        eout = e1->value();
	
        e2->value(eout);
	
	d1->fromString("+1 02:03:04.567890");
	
	e1->add(d1->value());
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

        // test Epoch.difference()
	acstime::Duration d2_val = e1->difference(e2->value());
	
	DurationHelper *d2 = new DurationHelper(d2_val);
        pStr = d2->toString("");
	
        cout << pStr.c_str() << endl;
	delete d2;
	

        // test Epoch.subtract()
	e1->subtract(d1->value());
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

        // test Epoch.compare() using Epoch.now()
	eout.value = m_now->get_sync(completion.out());
	
	e1->value(eout);
	
	usleep(8000);		// 8 ms = LCU clock resolution
	eout.value = m_now->get_sync(completion.out());
	
	e2->value(eout);
	
        acstime::TimeComparison tcom = e1->compare(e2->value());
	
        printTimeCompare(tcom,"e2","e1");
        tcom = e2->compare(e1->value());
	
        printTimeCompare(tcom,"e1","e2");
        tcom = e1->compare(e1->value());
	
        printTimeCompare(tcom,"e1","e1");

	// test Epoch setting by parts
	e1->reset();
	
	e1->year(1995);
	
	e1->month(4);
	
	e1->day(28);
	
	e1->hour(17);
	
	e1->minute(23);
	
	e1->second(15);
	
	e1->microSecond(654321);
	
	eout = e1->value();
	
	printLongLong(eout.value);
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	e1->reset();
	
	e1->year(1995);
	
	e1->dayOfYear(118);
	
	eout = e1->value();
	
	printLongLong(eout.value);
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test Epoch getting by parts
	long lngtmp = e1->year();
	
	cout << lngtmp << " ";
	lngtmp = e1->month();
	
	cout << lngtmp << " ";
	lngtmp = e1->day();
	
	cout << lngtmp << " ";
	lngtmp = e1->dayOfYear();
	
	cout << lngtmp << " ";
	lngtmp = e1->dayOfWeek();
	
	cout << lngtmp << " ";
	lngtmp = e1->hour();
	
	cout << lngtmp << " ";
	lngtmp = e1->minute();
	
	cout << lngtmp << " ";
	lngtmp = e1->second();
	
	cout << lngtmp << " ";
	lngtmp = e1->microSecond();
	
	cout << lngtmp << endl;

	// test Epoch.normalize() switch
	e1->normalize(true);
	
	e1->fromString(acstime::TSArray,
                          "1900-13-32T25:67:71.955161");
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test get/set microSecond value w/ normalize true
	lngtmp = e1->microSecond();
	
	e1->microSecond(lngtmp - 1111111);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->microSecond(lngtmp + 1111111);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test get/set second value w/ normalize true
	lngtmp = e1->second();
	
	e1->second(lngtmp - 61);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->second(lngtmp + 61);
	

	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test get/set minute value w/ normalize true
	lngtmp = e1->minute();
	
	e1->minute(lngtmp - 61);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->minute(lngtmp + 61);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test get/set hour value w/ normalize true
	lngtmp = e1->hour();
	
	e1->hour(lngtmp - 25);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->hour(lngtmp + 25);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test get/set day value w/ normalize true (non-leap year)
	e1->fromString(acstime::TSArray,
                          "1901-02-26T21:18:37.955161");
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	lngtmp = e1->day();
	
	e1->day(lngtmp - 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->day(lngtmp + 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	e1->fromString(acstime::TSArray,
                          "1901-03-02T12:16:43.955161");
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	lngtmp = e1->day();
	
	e1->day(lngtmp - 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->day(lngtmp + 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test get/set day value w/ normalize true (leap year)
	e1->fromString(acstime::TSArray,
                          "1904-02-26T08:53:12.955161");
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	lngtmp = e1->day();
	
	e1->day(lngtmp - 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->day(lngtmp + 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	e1->fromString(acstime::TSArray,
                          "1904-03-02T18:37:21.955161");
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	lngtmp = e1->day();
	
	e1->day(lngtmp - 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->day(lngtmp + 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test get/set day-of-year value w/ normalize true (non-leap year)
	e1->fromString(acstime::TSArray,
                          "1901-02-26T21:18:37.955161");
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	lngtmp = e1->dayOfYear();
	
	e1->dayOfYear(lngtmp - 58);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->dayOfYear(lngtmp + 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	e1->fromString(acstime::TSArray,
                          "1901-03-02T12:16:43.955161");
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	lngtmp = e1->dayOfYear();
	
	e1->dayOfYear(lngtmp - 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->dayOfYear(lngtmp + 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test get/set day-of-year value w/ normalize true (leap year)
	e1->fromString(acstime::TSArray,
                          "1904-02-26T08:53:12.955161");
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	lngtmp = e1->dayOfYear();
	
	e1->dayOfYear(lngtmp - 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->dayOfYear(lngtmp + 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	e1->fromString(acstime::TSArray,
                          "1904-03-02T18:37:21.955161");
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	lngtmp = e1->dayOfYear();
	
	e1->dayOfYear(lngtmp - 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->dayOfYear(lngtmp + 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test get/set month value w/ normalize true
	e1->fromString(acstime::TSArray,
			   "1904-02-14T18:37:21.955161");
	
	lngtmp = e1->month();
	
	e1->month(lngtmp - 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->month(lngtmp + 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// test get/set year value w/ normalize true
	lngtmp = e1->year();
	
	e1->year(lngtmp - 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	e1->year(lngtmp + 12);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// delete Epoch and Duration objects
	delete e1;
	delete e2;
	delete d1;

	//New test added just to make sure an MJD can be converted to an ACS Epoch
	cout << "New tests for MJD constructor here:" << endl << endl;

	//January 1, 2004 @ 12:00 and 1/2 second
	long double myDouble = 4579632000.500000;

	for (int i = 0; i < 10; i++)
	    {
	    e1 = new EpochHelper(myDouble);
	    cout.precision(16);
	    cout << myDouble << " ";
	    cout.precision(16);
	    cout << e1->toMJDseconds() << " ";
	    cout.precision(32);
	    cout << e1->value().value << " " <<  (e1->toString(acstime::TSArray,"", 0, 0)).c_str() << endl;
	    delete e1;
	    //add one microsecond
	    myDouble = myDouble + 0.000001;
	    }



	///////////
	cout << "New tests for ACS::Time constructor and value method here:" << endl << endl;
	{
	// current time
	ACS::Time time = m_now->get_sync(completion.out());
	
	e1 = new EpochHelper(time);
	
	std::string pStr = e1->toString(acstime::TSArray,
				  "%x",
				  m_array2TAI->get_sync(completion.out()), 
				  m_TAI2UTC->get_sync(completion.out()));
	
        cout << "Current time is " << pStr.c_str() << endl;

	// test Epoch range & toUTUdate(), toJulianYear()
	time = 0xFFFFFFFFFFFFFFFALL;
	e1->value(time);
	
        pStr = e1->toString(acstime::TSArray,"", m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	double utc = e1->toUTCdate(m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
	double julian = e1->toJulianYear(m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));

	long double mjdSeconds = e1->toMJDseconds();
	
	cout << utc << " " << julian << " " << mjdSeconds << endl;

	time = 0LL;
	e1->value(time);
	
	pStr = e1->toString(acstime::TSArray,allOut,m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	utc = e1->toUTCdate(m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
	julian = e1->toJulianYear(m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
	mjdSeconds = e1->toMJDseconds();
	cout << utc << " " << julian << " " << mjdSeconds << endl;
	}


	///////////
	cout << "New tests for operators here:" << endl << endl;
	{
	delete e1;
	e1 = new EpochHelper();
	long long dividend = 1LL;
	ACS::Time seconds = 1000ULL;

	e1->value(seconds);
	eout.value = 1ULL;
	cout << "Epoch value:" <<e1->value().value << ", Modulo operand:" << eout.value;
	e1->modulo(eout);
	cout << ", New value:" << e1->value().value << endl;

	e1->value(seconds);
	eout.value = 11ULL;
	cout << "Epoch value:" <<e1->value().value << ", Modulo operand:" << eout.value;
	e1->modulo(eout);
	cout << ", New value:" << e1->value().value << endl;

	e1->value(seconds);
	dividend = 1LL;
	cout << "Epoch value:" <<e1->value().value << ", Modulo operand:" << dividend;
	d1 = *e1 % dividend;
	cout << ", DurationHelper value:" << d1->value().value << endl;
	delete d1;
	
	e1->value(seconds);
	dividend = 11LL;
	cout << "Epoch value:" <<e1->value().value << ", Modulo operand:" << dividend;
	d1 = *e1 % dividend;
	cout << ", DurationHelper value:" << d1->value().value << endl;
	delete d1;

	seconds = 1ULL;
	e1->value(seconds);
	dividend = 1000LL;
	cout << "Epoch value:" <<e1->value().value << ", Modulo operand:" << dividend;
	d1 = dividend % *e1;
	cout << ", DurationHelper value:" << d1->value().value << endl;
	delete d1;
   	seconds = 11ULL;	
	e1->value(seconds);
	dividend = 1000LL;
	cout << "Epoch value:" <<e1->value().value << ", Modulo operand:" << dividend;
	d1 = dividend % *e1;
	cout << ", DurationHelper value:" << d1->value().value << endl;
	delete d1;
	}



	// ???
	ci.logout();
	}

    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION(ex,"Error!");
	return -1;
	} 

    return 0;
}
