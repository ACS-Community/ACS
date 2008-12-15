#ifndef ACSCOMPONENTSMARTPTRTEST_H
#define ACSCOMPONENTSMARTPTRTEST_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2008 
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
* "@(#) $Id: acsComponentSmartPtrTestImpl.h,v 1.2 2008/12/15 17:51:05 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* agrimstrup  2008-12-09  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>
#define private public
#include <acsComponentSmartPtr.h>
#undef private
#include "mockContainerServicesImpl.h"
#include "mockComponentImpl.h"

using namespace std;

class smartptrtest : public CPPUNIT_NS :: TestFixture
{
    CPPUNIT_TEST_SUITE (smartptrtest);
    CPPUNIT_TEST (defaultConstructorTest);
    CPPUNIT_TEST (parmConstructorTest);
    CPPUNIT_TEST (copyConstructorTest);
    CPPUNIT_TEST (assignmentOperatorTest);
    CPPUNIT_TEST_SUITE_END ();

    public:
        void setUp (void);
        void tearDown (void);

    protected:
        void defaultConstructorTest (void);
        void parmConstructorTest (void);
        void copyConstructorTest (void);
        void assignmentOperatorTest (void);

    private:
	maci::MockContainerServices *mcs;
};
#endif /*!ACSCOMPONENTSMARTPTRTEST_H*/
