// $Id: testProfiler.cpp,v 1.3 2006/09/01 02:20:54 cparedes Exp $
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
// An client to test the C++ TimeUtil class.
//

#include <iostream>

#include "acstimeProfiler.h"

 using namespace std;

//------------------------------------------------------------------------------

int main(int argc, char *argv[])
{
    Profiler *joe = new Profiler();
    
    cout << "*****************************************************" << endl;
    joe->fullDescription("In theory this should bail...");
    cout << "*****************************************************" << endl;
    joe->start();
    joe->stop();
    joe->fullDescription("Should only be one...");
    cout << "*****************************************************" << endl;
    joe->reset();
    joe->start();
    joe->stop();
    joe->fullDescription("Should only be one...");
    cout << "*****************************************************" << endl;
    joe->reset();
    joe->start();
    joe->stop();
    joe->start();
    joe->stop();
    joe->start();
    joe->stop();
    joe->fullDescription("Should be three...");
    cout << "*****************************************************" << endl;
    joe->reset();
    joe->start();
    cout << "This should fail!" << endl;
    joe->start();
    joe->fullDescription("Should be none...");
    cout << "*****************************************************" << endl;
    joe->reset();
    joe->start();
    joe->stop();
    cout << "This should fail!" << endl;
    joe->stop();
    joe->fullDescription("Should be one...");
    cout << "*****************************************************" << endl;
    joe->reset();
    joe->start();
    ACE_OS::sleep(5);
    joe->stop();
    joe->start();
    ACE_OS::sleep(3);
    joe->stop();
    joe->fullDescription("Should be two with an average of 4 seconds...");
    cout << "*****************************************************" << endl;
    joe->reset();
    joe->start();
    ACE_OS::sleep(1);
    joe->stop();
    joe->addData("a key", "a value");
    joe->fullDescription("Should be one extra descrip.");
    joe->addData("somethingElse", "1.2345678");
    joe->fullDescription("Should be two extra descrips.");
    return 0;
}






