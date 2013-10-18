#ifndef TEST_ACS_DAEMON_UTILS_H
#define TEST_ACS_DAEMON_UTILS_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
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
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2013-05-23  created
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
#include "acsDaemonUtils.h"

using namespace std;

class TestAcsDaemonUtils : public CPPUNIT_NS :: TestFixture
{
	CPPUNIT_TEST_SUITE (TestAcsDaemonUtils);
	CPPUNIT_TEST (testLogDirectory);
	CPPUNIT_TEST (testLogDirectoryForPlainContainer);
	CPPUNIT_TEST (testLogDirectoryForHierarchicalContainer);
	CPPUNIT_TEST (testTimestamp);
	CPPUNIT_TEST (testSimpleContainerName);
	CPPUNIT_TEST_SUITE_END ();

public:
	void setUp (void);
	void tearDown (void);

protected:
	void testLogDirectory(void);
	void testLogDirectoryForPlainContainer(void);
	void testLogDirectoryForHierarchicalContainer(void);
	void testTimestamp(void);
	void testSimpleContainerName(void);

private:
	AcsDaemonUtils* daemonUtil_mp;

}

#endif /*!TEST_ACS_DAEMON_UTILS_H*/
;
