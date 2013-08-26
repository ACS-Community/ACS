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
* "@(#) $Id: acsComponentSmartPtrTestImpl.cpp,v 1.6 2009/11/05 03:20:25 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* agrimstrup  2008-12-09  created 
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

static char *rcsId="@(#) $Id: acsComponentSmartPtrTestImpl.cpp,v 1.6 2009/11/05 03:20:25 agrimstrup Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsComponentSmartPtrTestImpl.h"

CPPUNIT_TEST_SUITE_REGISTRATION (smartptrtest);

void smartptrtest :: setUp (void)
{
    ACE_CString name("Test");
    mcs = new maci::MockContainerServices(name); 
}

void smartptrtest :: tearDown (void)
{
    delete mcs;
}

void smartptrtest :: defaultConstructorTest (void)
{
    maci::SmartPtr<MockComponent> foo;
    CPPUNIT_ASSERT_EQUAL ((maci::MockContainerServices *)foo.handle, (maci::MockContainerServices *)0);
    CPPUNIT_ASSERT_EQUAL (foo.sticky, false);
    CPPUNIT_ASSERT_EQUAL ((MockComponent *)foo.pointee_, MockComponent::_nil());
}

void smartptrtest :: parmConstructorTest (void)
{
    ACE_CString cname("Foo");
    maci::SmartPtr<MockComponent> foo(mcs, true, mcs->getComponent<MockComponent>(cname.c_str()));
    CPPUNIT_ASSERT_EQUAL ((maci::MockContainerServices *)foo.handle, mcs);
    CPPUNIT_ASSERT_EQUAL (foo.sticky, true);
    CPPUNIT_ASSERT_EQUAL (foo.pointee_, mcs->getComponent<MockComponent>(cname.c_str()));
}

void smartptrtest :: copyConstructorTest (void)
{
    ACE_CString cname("Foo");
    maci::SmartPtr<MockComponent> foo(mcs, true, mcs->getComponent<MockComponent>(cname.c_str()));
    maci::SmartPtr<MockComponent> bar(foo);
    CPPUNIT_ASSERT_EQUAL ((maci::MockContainerServices *)bar.handle, mcs);
    CPPUNIT_ASSERT_EQUAL (bar.sticky, true);
    CPPUNIT_ASSERT_EQUAL (bar.pointee_, foo.pointee_);
}

void smartptrtest :: assignmentOperatorTest (void)
{
    ACE_CString cname("Foo");
    maci::SmartPtr<MockComponent> foo(mcs, true, mcs->getComponent<MockComponent>(cname.c_str()));
    maci::SmartPtr<MockComponent> bar;
    bar = foo;
    CPPUNIT_ASSERT_EQUAL ((maci::MockContainerServices *)bar.handle, mcs);
    CPPUNIT_ASSERT_EQUAL (bar.sticky, true);
    CPPUNIT_ASSERT_EQUAL (bar.pointee_, foo.pointee_);
}

void smartptrtest :: releaseTest(void)
{
    ACE_CString cname("Foo");
    maci::SmartPtr<MockComponent> foo(mcs, true, mcs->getComponent<MockComponent>(cname.c_str()));
    foo.release();
    CPPUNIT_ASSERT_EQUAL ((maci::MockContainerServices *)foo.handle, (maci::MockContainerServices *)0);
    CPPUNIT_ASSERT_EQUAL (foo.pointee_, (MockComponent *)0);
    CPPUNIT_ASSERT_EQUAL (foo.sticky, false);
}

void smartptrtest :: isNilNotSetTest(void)
{
    maci::SmartPtr<MockComponent> foo;
    CPPUNIT_ASSERT_EQUAL (foo.pointee_, (MockComponent *)0);
}

void smartptrtest :: isNilSetTest(void)
{
    ACE_CString cname("Foo");
    maci::SmartPtr<MockComponent> foo(mcs, true, mcs->getComponent<MockComponent>(cname.c_str()));
    cout << "Set is running" << endl;
    cout.flush();
    CPPUNIT_ASSERT (foo.pointee_ != (MockComponent *)0);
}

void smartptrtest :: assnopr_cpyarginvalidTest(void)
{
    ACE_CString cname("Foo");
    maci::SmartPtr<MockComponent> foo;
    maci::SmartPtr<MockComponent> bar((maci::MockContainerServices *) 0, false, mcs->getComponent<MockComponent>(cname.c_str()));
    foo = bar;
}

void smartptrtest :: assnopr_constinvalidTest (void)
{
    ACE_CString cname("Foo");
    maci::SmartPtr<MockComponent> foo;
    const maci::SmartPtr<MockComponent> bar((maci::MockContainerServices *) 0, false, mcs->getComponent<MockComponent>(cname.c_str()));
    foo = bar;
}

void smartptrtest :: assnopr_invalidTest (void)
{
}

/*___oOo___*/
