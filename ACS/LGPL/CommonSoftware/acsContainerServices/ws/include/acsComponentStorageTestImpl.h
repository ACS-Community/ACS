#ifndef ACSCOMPONENTSTORAGETEST_H
#define ACSCOMPONENTSTORAGETEST_H
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
* "@(#) $Id: acsComponentStorageTestImpl.h,v 1.1 2009/11/04 20:44:29 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* agrimstrup  2009-11-03  created
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
#define protected public
#include <acsComponentSmartPtr.h>
#undef protected
#include "mockContainerServicesImpl.h"
#undef private
#include "mockComponentImpl.h"

using namespace std;

class storagepolicytest : public CPPUNIT_NS :: TestFixture
{
    CPPUNIT_TEST_SUITE (storagepolicytest);
    CPPUNIT_TEST (isValid_nilpointeeTest);
    CPPUNIT_TEST (isValid_nilcontsrvTest);
    CPPUNIT_TEST (isValid_nonnilTest);
    CPPUNIT_TEST (Destroy_nilhandleTest);
    CPPUNIT_TEST (Destroy_nilpointeeTest);
    CPPUNIT_TEST (Destroy_notstickyTest);
    CPPUNIT_TEST (Destroy_cannotreleaseexTest);
    CPPUNIT_TEST (Destroy_otherexTest);
    CPPUNIT_TEST_SUITE_END ();

    public:
        void setUp (void);
        void tearDown (void);

    protected:
        void isValid_nilpointeeTest (void);
        void isValid_nilcontsrvTest (void);
        void isValid_nonnilTest (void);
        void Destroy_nilhandleTest (void);
        void Destroy_nilpointeeTest (void);
        void Destroy_notstickyTest (void);
        void Destroy_cannotreleaseexTest (void);
        void Destroy_otherexTest (void);

    private:
	maci::MockContainerServices *mcs;
        MockComponent *comp;
};
#endif /*!ACSCOMPONENTSTORAGETEST_H*/
