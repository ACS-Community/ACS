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
* "@(#) $Id: acsComponentStorageTestImpl.cpp,v 1.1 2009/11/04 20:44:29 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* agrimstrup  2009-11-03  created 
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*
*   
*   PARENT CLASS
*
* 
*   DESCRIPTION
*
*
*   PUBLIC METHODS
*
*
*   PUBLIC DATA MEMBERS
*
*
*   PROTECTED METHODS
*
*
*   PROTECTED DATA MEMBERS
*
*
*   PRIVATE METHODS
*
*
*   PRIVATE DATA MEMBERS
*
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsComponentStorageTestImpl.h"

CPPUNIT_TEST_SUITE_REGISTRATION (storagepolicytest);

void storagepolicytest :: setUp (void)
{
    ACE_CString name("Test");
    ACE_CString compname("TestComponent");
    mcs = new maci::MockContainerServices(name); 
    comp = mcs->getComponent<MockComponent>(compname.c_str());
}

void storagepolicytest :: tearDown (void)
{
    mcs->releaseComponent(comp->name());
    delete mcs;
}


void storagepolicytest :: isValid_nilpointeeTest (void)
{
    maci::ComponentStorage<MockComponent, maci::MockContainerServices> foo;
    CPPUNIT_ASSERT_EQUAL (foo.isValid(foo), true);
}

void storagepolicytest :: isValid_nilcontsrvTest (void)
{
    maci::ComponentStorage<MockComponent, maci::MockContainerServices> foo(comp);
    CPPUNIT_ASSERT_EQUAL (foo.isValid(foo), false);
}

void storagepolicytest :: isValid_nonnilTest (void)
{
    maci::ComponentStorage<MockComponent, maci::MockContainerServices> foo;
    foo.setValues(mcs, true, comp);
    CPPUNIT_ASSERT_EQUAL (foo.isValid(foo), true);
}

void storagepolicytest :: Destroy_nilhandleTest (void)
{
    maci::ComponentStorage<MockComponent, maci::MockContainerServices> foo;
    MockComponent *mc;

    foo.setValues(0, true, comp);
    foo.Destroy();
    CPPUNIT_ASSERT_EQUAL (mcs->map->find(comp->name(),mc), 0);
}

void storagepolicytest :: Destroy_nilpointeeTest (void)
{
    maci::ComponentStorage<MockComponent, maci::MockContainerServices> foo;
    MockComponent *mc;

    foo.setValues(mcs, true, foo.Default());
    foo.Destroy();
    CPPUNIT_ASSERT_EQUAL (mcs->map->find(comp->name(),mc), 0);
}

void storagepolicytest :: Destroy_notstickyTest (void)
{
    maci::ComponentStorage<MockComponent, maci::MockContainerServices> foo;
    MockComponent *mc;

    foo.setValues(mcs, false, comp);
    foo.Destroy();
    CPPUNIT_ASSERT_EQUAL (mcs->map->find(comp->name(),mc), 0);
}

void storagepolicytest :: Destroy_cannotreleaseexTest (void)
{
    ACE_CString tname("Boom");
    ACE_CString tcsname("Temp");
    maci::ComponentStorage<MockComponent, maci::MockContainerServices> foo;
    maci::MockContainerServices *tmcs = new maci::MockContainerServices(tcsname);
    MockComponent *badcomp = new MockComponent(tname, tmcs);

    foo.setValues(tmcs, true, badcomp);
    foo.Destroy();
    delete tmcs;
}

void storagepolicytest :: Destroy_otherexTest (void)
{
    ACE_CString tname("ShaBoom");
    ACE_CString tcsname("Temp");
    maci::ComponentStorage<MockComponent, maci::MockContainerServices> foo;
    maci::MockContainerServices *tmcs = new maci::MockContainerServices(tcsname);
    MockComponent *badcomp = new MockComponent(tname, tmcs);

    foo.setValues(tmcs, true, badcomp);
    foo.Destroy();
    delete tmcs;
}

/*___oOo___*/
