// @(#) $Id: helperFuncs.c,v 1.2 2004/03/17 07:43:58 bjeram Exp $
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
#include <baci.h>
#include "acstimeC.h"
#include <iostream>

using namespace std;

void printLongLong(unsigned long long);
void printTimeCompare(acstime::TimeComparison,char*,char*);

//------------------------------------------------------------------------------
// print contents of given unsigned 64-bit int

void printLongLong(unsigned long long numToPrint)
{
    const int size = 20;  // max value 1.84 x 10^19
    char printBuf[size+1];
    
    for (int i = 0; i < size; i++)
	{
        printBuf[i] = ' ';
	}
    printBuf[size] = '\0';
    
    for (int i = size - 1; i >= 0; i--) 
	{
	printBuf[i] = numToPrint % 10 + '0';
	numToPrint /= 10;
	if (numToPrint == 0)
	    {
 	    break;
	    }
	}
    
    std::cout << printBuf << std::endl;
}

//------------------------------------------------------------------------------
// print contents of given enum TimeComparison

void printTimeCompare(acstime::TimeComparison tcom,
                      char* label1,
		      char* label2)
{
    if (tcom == acstime::TCEqualTo)
	{
        cout << label1 << " and " << label2 << " are equal" << endl;
	}
    else if (tcom == acstime::TCLessThan)
	{
        cout << label1 << " is less than " << label2 << endl;
	}    
    else if (tcom == acstime::TCGreaterThan)
	{
        cout << label1 << " is greater than " << label2 << endl;
	}
}
