// @(#) $Id: testDuration.cpp,v 1.8 2009/03/13 10:05:37 bjeram Exp $
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
// An ACS client to test the Duration interface.
//
#include <baci.h>
#include "acstimeC.h"
#include "helperFuncs.c"
#include "acstimeDurationHelper.h"


//------------------------------------------------------------------------------
int main(int argc,char *argv[])
{ 
    // create two Duration objects
    DurationHelper *d1 = new DurationHelper();
    DurationHelper *d2 = new DurationHelper();
    
    // create a Duration structure
    acstime::Duration dout;
    
    // test Duration range
    dout.value = 0x7FFFFFFFFFFFFFFALL;
    d1->value(dout);
    std::string pStr = d1->toString("");
    cout << pStr.c_str() << " " << d1->toSeconds() << endl;
    
    dout.value = 0LL;
    d1->value(dout); 
    pStr = d1->toString("");
    cout << pStr.c_str() << " " << d1->toSeconds() << endl;
    
    dout.value = 0x8000000000000000LL + 8;
    d1->value(dout);
    pStr = d1->toString("");
    cout << pStr.c_str() << " " << d1->toSeconds() << endl;
    
    d1->fromString("10675199 02:48:05.47758");
    dout = d1->value();
    printLongLong(dout.value);
    
    d1->fromString("0:0:0");
    dout = d1->value();
    printLongLong(dout.value);
    
    d1->fromString("-10675199 2:48:5.47758");
    dout = d1->value();
    printLongLong( ~ dout.value + 1);  // two's compl to print +
    
    // test Duration usec implicit trailing zeroes
    d1->fromString("98 23:32:10.05");
    pStr = d1->toString("");
    cout << pStr.c_str() << " " << d1->toSeconds() << endl;
    d1->fromString("10:23:54.12305");
    pStr = d1->toString("");
    cout << pStr.c_str() << " " << d1->toSeconds() << endl;
    
    // test Duration setting by parts
    d1->positive(true);
    d1->day(28);
    d1->hour(17);
    d1->minute(23);
    d1->second(15);
    d1->microSecond(654321);
    dout = d1->value();
    printLongLong(dout.value);
    pStr = d1->toString("");
    cout << pStr.c_str() << " " << d1->toSeconds() << endl;

    // test Duration getting by parts
    bool booltmp = d1->positive();
    cout << ((booltmp == true) ? "true" : "false") << " ";
    long tmp = d1->day();
    cout << tmp << " ";
    tmp = d1->hour();
    cout << tmp << " ";
    tmp = d1->minute();
    cout << tmp << " ";
    tmp = d1->second();
    cout << tmp << " ";
    tmp = d1->microSecond();
    cout << tmp << endl;
    
    // test Duration.compare()
    // d1 == "+1 02:03:04.567890" from above
    d2->fromString("-1 02:03:04.56789");
    acstime::TimeComparison tcom = d1->compare(d2->value());
    printTimeCompare(tcom,"d2","d1");
    tcom = d2->compare(d1->value());
    printTimeCompare(tcom,"d1","d2");
    
    // test Duration.add() and Duration.subtract()
    // d1 == "+1 02:03:04.567890" from above
    d2->fromString("0:0:0");
    d1->add(d2->value());
    pStr = d1->toString("");
    cout << pStr.c_str() << endl;
    tcom = d1->compare(d2->value());
    printTimeCompare(tcom,"d2","d1");
    d1->subtract(d2->value());
    pStr = d1->toString("");
    cout << pStr.c_str() << endl;
    tcom = d1->compare(d2->value());
    printTimeCompare(tcom,"d2","d1");
    
    d2->fromString("1:2:3");
    d1->add(d2->value());
    pStr = d1->toString("");
    cout << pStr.c_str() << endl;
    tcom = d1->compare(d2->value());
    printTimeCompare(tcom,"d2","d1");
    d1->subtract(d2->value());
    pStr = d1->toString("");
    cout << pStr.c_str() << endl;
    tcom = d1->compare(d2->value());
    printTimeCompare(tcom,"d2","d1");
    
    d2->fromString("-4 3:2:1.078");
    d1->add(d2->value());
    pStr = d1->toString("");
    cout << pStr.c_str() << endl;
    tcom = d1->compare(d2->value());
    printTimeCompare(tcom,"d2","d1");
    d1->subtract(d2->value());
    pStr = d1->toString("");
    cout << pStr.c_str() << endl;
    tcom = d1->compare(d2->value());
    printTimeCompare(tcom,"d2","d1");
    
    // test Duration.multiply() and Duration.divide()
    // d1 == "+1 02:03:04.567890" from above
    d1->divide(3);
    pStr = d1->toString("");
    cout << pStr.c_str() << endl;
    d1->multiply(3);
    pStr = d1->toString("");
    cout << pStr.c_str() << endl;
    
    // test Duration.normalize() switch
    d1->normalize(true);
    d1->fromString("-4 47:61:71.078");
    pStr = d1->toString("");
    cout << pStr.c_str() << endl;
       

    cout << "New tests for seconds constructor here:" << endl << endl;
    delete d1;
    //
    long double myDouble = 123456789.500000;
    
    for (int i = 0; i < 10; i++)
	{
	d1 = new DurationHelper(myDouble);
	cout.precision(16);
	cout << myDouble << " ";
	cout.precision(16);
	cout << d1->toSeconds() << " ";
	cout.precision(32);
	cout << d1->value().value << " " <<  (d1->toString("")).c_str() << endl;
	delete d1;
	//add one microsecond
	myDouble = myDouble + 0.000001;
	}

    cout << "New tests for ACS::TimeInterval constructor value methods here:" << endl << endl;
    {
    ACS::TimeInterval timeInterval = 0x7FFFFFFFFFFFFFFALL;
    d1 = new DurationHelper(timeInterval);
    std::string pStr = d1->toString("");
    cout << pStr.c_str() << " " << d1->toSeconds() << endl;
    
    timeInterval = 0LL;
    d1->value(timeInterval); 
    pStr = d1->toString("");
    cout << pStr.c_str() << " " << d1->toSeconds() << endl;
    
    timeInterval = 0x8000000000000000LL + 8;
    d1->value(timeInterval);
    pStr = d1->toString("");
    cout << pStr.c_str() << " " << d1->toSeconds() << endl;
    delete d1;
    }

    ///////////
    cout << "New tests for operators here:" << endl << endl;
    {
    d1 = new DurationHelper();
    ACS::TimeInterval seconds =  1000LL; 
    d1->value(seconds);
    dout.value = 1LL;
    cout << "Duration value:" <<d1->value().value << ", Modulo operand:" << dout.value;
    d1->modulo(dout);
    cout << ", New value:" << d1->value().value << endl;
    
    d1->value(seconds);
    dout.value = 11LL;
    cout << "Duration value:" <<d1->value().value << ", Modulo operand:" << dout.value;
    d1->modulo(dout);
    cout << ", New value:" << d1->value().value << endl;
    delete d1;
    }
    
    
    return 0;
}
