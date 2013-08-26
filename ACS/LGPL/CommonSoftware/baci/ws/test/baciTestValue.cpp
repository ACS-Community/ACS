/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005 
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
* "@(#) $Id: baciTestValue.cpp,v 1.7 2008/11/02 14:23:58 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-01-18  created 
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
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

#include "baciValue.h"
#include "baciTestC.h"
#include <cppunit/extensions/HelperMacros.h>

static char *rcsId="@(#) $Id: baciTestValue.cpp,v 1.7 2008/11/02 14:23:58 agrimstrup Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


using namespace baci;

class ValueTestCase : public CPPUNIT_NS::TestFixture
{
    CPPUNIT_TEST_SUITE( ValueTestCase );
    CPPUNIT_TEST( testNull  );
    CPPUNIT_TEST( testPointer );
    CPPUNIT_TEST( testString );
    CPPUNIT_TEST( testDouble );
    CPPUNIT_TEST( testFloat );
    CPPUNIT_TEST( testLong );
    CPPUNIT_TEST( testPattern );
    CPPUNIT_TEST( testDoubleSeq );
    CPPUNIT_TEST( testFloatSeq );
    CPPUNIT_TEST( testLongSeq );
    CPPUNIT_TEST( testLongLong );
    CPPUNIT_TEST( testuLongLong );
    CPPUNIT_TEST( testStringSeq );
    CPPUNIT_TEST( testEnum );
    CPPUNIT_TEST_SUITE_END();
    
  public:
    void setUp()
	{

	}


  protected:
    void testNull() 
	{}

    void testPointer()
	{}

    void testString()
	{ 
	    testBaciValueType<ACE_CString>("ab", "xyz");
	    //testBaciValueDelta<ACE_CString>("ab", "xyz", "a", "cdefg");
	}

    void testDouble()
	{ 
	    testBaciValueType<BACIdouble>(1.0, 5.0);
	    testBaciValueDelta<BACIdouble>(1.0, 5.0, 2.0, 100.0);
	}
    
    void testFloat()
	{ 
	    testBaciValueType<BACIfloat>(1.0, 5.0);
	    testBaciValueDelta<BACIfloat>(1.0, 5.0, 2.0, 100.0);
	}
    
    void testLong()
	{ 
	    testBaciValueType<BACIlong>(1L, 5L);
	    testBaciValueDelta<BACIlong>(1L, 5L, 2L, 100L);
	}

    void testPattern()
	{ 
	    testBaciValueType<BACIpattern>(1L, 5L);
	    //testBaciValueDelta<BACIpattern>(1L, 5L, 2L, 100L);
	}

    void testEnum();


    void testDoubleSeq()
	{
	    //The implementation is broken as these do not pass!
	    /*
	    CORBA::Double sbd[] = {1.0};
	    BACIdoubleSeq sbdSeq(3, 3, sbd);
	    
	    CORBA::Double lbd[] = {1.1, 2.1, 3.1};
	    BACIdoubleSeq lbdSeq(3, 3, lbd);

	    CORBA::Double sDelta[] = {0.5};
	    BACIdoubleSeq sbDelta(3, 3, sDelta);
	    
	    CORBA::Double lDelta[] = {100.1, 200.1, 300.1};
	    BACIdoubleSeq lbDelta(3, 3, lDelta);

	    testBaciValueType<BACIdoubleSeq>(sbdSeq, lbdSeq, sbDelta, lbDelta);
	    */
	}

    void testFloatSeq()
	{
	    //The implementation is broken as these do not pass!
	    /*
	    CORBA::Float sbd[] = {1.0};
	    BACIfloatSeq sbdSeq(3, 3, sbd);
	    
	    CORBA::Float lbd[] = {1.1, 2.1, 3.1};
	    BACIfloatSeq lbdSeq(3, 3, lbd);

	    CORBA::Float sDelta[] = {0.5};
	    BACIfloatSeq sbDelta(3, 3, sDelta);
	    
	    CORBA::Float lDelta[] = {100.1, 200.1, 300.1};
	    BACIfloatSeq lbDelta(3, 3, lDelta);

	    testBaciValueType<BACIfloatSeq>(sbdSeq, lbdSeq, sbDelta, lbDelta);
	    */
	}

    void testLongSeq()
	{
	    //The implementation is broken as these do not pass!
	    /*
	    CORBA::Long sbd[] = {1L};
	    BACIlongSeq sbdSeq(3, 3, sbd);
	    
	    CORBA::Long lbd[] = {3L};
	    BACIlongSeq lbdSeq(3, 3, lbd);

	    CORBA::Double sDelta[] = {1L};
	    BACIdoubleSeq sbDelta(3, 3, sDelta);
	    
	    CORBA::Double lDelta[] = {100L, 200L, 300L};
	    BACIdoubleSeq lbDelta(3, 3, lDelta);

	    testBaciValueType<BACIlongSeq>(sbdSeq, lbdSeq, sbDelta, lbDelta);
	    */
	}

    void testLongLong()
	{
	    testBaciValueType<BACIlongLong>(1LL, 5LL);
	    testBaciValueDelta<BACIlongLong>(1LL, 5LL, 2LL, 100LL);
	}

    void testuLongLong()
	{
	    testBaciValueType<BACIuLongLong>(1ULL, 5ULL);
	    testBaciValueDelta<BACIuLongLong>(1ULL, 5ULL, 2ULL, 100ULL);
	}

    void testStringSeq()
	{
	    //The implementation is broken as these do not pass!
	    /*
	    char* sb[] = {"abc"};
	    BACIstringSeq sbSeq(3, 3, sb);
	    
	    char* lb[] = {"xyz"};
	    BACIstringSeq lbSeq(3, 3, lb);

	    char* lbClone[] = {"xyz"};
	    BACIstringSeq lbSeqClone(3, 3, lbClone);
	    
	    BACIValue bSmall(sbSeq);
	    BACIValue bLarge(lbSeq);
	    BACIValue bLargeClone(lbSeqClone);

	    CPPUNIT_ASSERT_MESSAGE("Less than operator is broken1.", bSmall<bLarge );
	    CPPUNIT_ASSERT_MESSAGE("Less than operator is broken2.", !(bLargeClone<bLarge) );
	    CPPUNIT_ASSERT_MESSAGE("Less than operator is broken3.", !(bLarge<bSmall));
	    CPPUNIT_ASSERT_MESSAGE("Less than equal to operator is broken1.", bSmall<=bLarge);
	    CPPUNIT_ASSERT_MESSAGE("Less than equal to operator is broken2.", bLargeClone<=bLarge );
	    CPPUNIT_ASSERT_MESSAGE("Less than equal to operator is broken3.", !(bLarge<=bSmall));
	    CPPUNIT_ASSERT_MESSAGE("Equal to operator is broken1.", bLarge==bLarge);
	    CPPUNIT_ASSERT_MESSAGE("Equal to operator is broken2.", !(bLarge==bSmall));
	    CPPUNIT_ASSERT_MESSAGE("Equal to operator is broken3.", bLarge==bLargeClone);
	    */
	}

  private:
    template<class T> void
    testBaciValueType(const T smallValue, 
		      const T largeValue);


    template<class T> void
    testBaciValueTypeOperators(const T smallValue, 
			       const T largeValue);
    
    template<class T> void
    testBaciValueDelta(const T smallValue, 
		      const T largeValue, 
		      const T deltaSmall, 
		      const T deltaLarge);

};

CPPUNIT_TEST_SUITE_REGISTRATION(ValueTestCase);

template<class T> void
ValueTestCase::testBaciValueType(const T smallValue, const T largeValue)
{
    testBaciValueTypeOperators<T>(smallValue, largeValue);
    return;
}

template<class T> void
ValueTestCase::testBaciValueDelta(const T smallValue, const T largeValue, const T deltaSmall, const T deltaLarge)
{
    BACIValue bSmall(smallValue);
    BACIValue bLarge(largeValue);
    BACIValue bLargeClone(largeValue);
	
    BACIValue bDSmall(deltaSmall);
    BACIValue bDLarge(deltaLarge);

    CPPUNIT_ASSERT_MESSAGE("Delta1Broken.", !(bLarge.lessThanDelta(bSmall, bDSmall)) ); 
    CPPUNIT_ASSERT_MESSAGE("Delta2Broken.", bLarge.lessThanDelta(bSmall, bDLarge) ); 

    CPPUNIT_ASSERT_MESSAGE("Delta3Broken.", !(bSmall.lessThanDelta(bLarge, bDSmall)) ); 
    CPPUNIT_ASSERT_MESSAGE("Delta4Broken.", bSmall.lessThanDelta(bLarge, bDLarge) ); 

    return;
}


template<class T> void
ValueTestCase::testBaciValueTypeOperators(const T smallValue, const T largeValue)
{
    BACIValue bSmall(smallValue);
    BACIValue bLarge(largeValue);
    BACIValue bLargeClone(largeValue);

    CPPUNIT_ASSERT_MESSAGE("Less than operator is broken1.", bSmall<bLarge );
    CPPUNIT_ASSERT_MESSAGE("Less than operator is broken2.", !(bLargeClone<bLarge) );
    CPPUNIT_ASSERT_MESSAGE("Less than operator is broken3.", !(bLarge<bSmall));
    CPPUNIT_ASSERT_MESSAGE("Less than equal to operator is broken1.", bSmall<=bLarge);
    CPPUNIT_ASSERT_MESSAGE("Less than equal to operator is broken2.", bLargeClone<=bLarge );
    CPPUNIT_ASSERT_MESSAGE("Less than equal to operator is broken3.", !(bLarge<=bSmall));
    CPPUNIT_ASSERT_MESSAGE("Equal to operator is broken1.", bLarge==bLarge);
    CPPUNIT_ASSERT_MESSAGE("Equal to operator is broken2.", !(bLarge==bSmall));
    CPPUNIT_ASSERT_MESSAGE("Equal to operator is broken3.", bLarge==bLargeClone);
	
    return;
}

//------------------------
void
ValueTestCase::testEnum()
{
    //create the enums
    BACI_TEST::BaciTestEnum smallValueEnum = BACI_TEST::ENUM_A;
    BACI_TEST::BaciTestEnum largeValueEnum = BACI_TEST::ENUM_C;
    //create the patterns
    ACS::pattern smallValuePattern = (ACS::pattern)smallValueEnum;
    ACS::pattern largeValuePattern = (ACS::pattern)largeValueEnum;
    //create some anys
    CORBA::Any smallValueAny;
    CORBA::Any largeValueAny;
    //put the enums in the anys
    smallValueAny <<= smallValueEnum;
    largeValueAny <<= largeValueEnum;
    
    //create the BACIValue objects
    BACIValue bSmall(smallValuePattern, smallValueAny);
    BACIValue bLarge(largeValuePattern, largeValueAny);
    BACIValue bLargeClone(largeValuePattern, largeValueAny);
    
    //now time for some standard tests
    CPPUNIT_ASSERT_MESSAGE("Less than operator is broken1.", bSmall<bLarge );
    CPPUNIT_ASSERT_MESSAGE("Less than operator is broken2.", !(bLargeClone<bLarge) );
    CPPUNIT_ASSERT_MESSAGE("Less than operator is broken3.", !(bLarge<bSmall));
    CPPUNIT_ASSERT_MESSAGE("Less than equal to operator is broken1.", bSmall<=bLarge);
    CPPUNIT_ASSERT_MESSAGE("Less than equal to operator is broken2.", bLargeClone<=bLarge );
    CPPUNIT_ASSERT_MESSAGE("Less than equal to operator is broken3.", !(bLarge<=bSmall));
    CPPUNIT_ASSERT_MESSAGE("Equal to operator is broken1.", bLarge==bLarge);
    CPPUNIT_ASSERT_MESSAGE("Equal to operator is broken2.", !(bLarge==bSmall));
    CPPUNIT_ASSERT_MESSAGE("Equal to operator is broken3.", bLarge==bLargeClone);
    
    //test to make sure similar methods have the same return value
    {
    CORBA::Any extractedValueAnyA;
    CORBA::Any extractedValueAnyB;

    //get the anys
    extractedValueAnyA = bSmall.enumValue();
    extractedValueAnyB = bSmall.getValue((CORBA::Any *)0);
    //get the real enum values
    BACI_TEST::BaciTestEnum extractedValueEnumA;
    BACI_TEST::BaciTestEnum extractedValueEnumB;
    extractedValueAnyA >>= extractedValueEnumA;
    extractedValueAnyB >>= extractedValueEnumB;
    
    CPPUNIT_ASSERT_MESSAGE("enumValue and getValue return different values.", 
			   extractedValueEnumA==extractedValueEnumB);

    CPPUNIT_ASSERT_MESSAGE("get enum methods return the wrong value", 
			   extractedValueEnumA==BACI_TEST::ENUM_A);

    CPPUNIT_ASSERT_MESSAGE("get enum methods return the wrong value", 
			   !(extractedValueEnumA==BACI_TEST::ENUM_B));

    //ok, now try to set the value to something else
    extractedValueEnumA = BACI_TEST::ENUM_B;
    //set the any to ENUM_B
    extractedValueAnyA <<= extractedValueEnumA;

    //set the BACIValue
    CPPUNIT_ASSERT_MESSAGE("Failed to set the enum on a pre-existing BACIValue", 
			   bSmall.enumValue(7L, extractedValueAnyA));
			   
    CORBA::Any anotherAny = bSmall.enumValue();
    BACI_TEST::BaciTestEnum anotherEnum;
    anotherAny >>= anotherEnum;
    CPPUNIT_ASSERT_MESSAGE("set followed by a get failed.", 
			   anotherEnum==extractedValueEnumA);

    }
}
//------------------------


#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>

int main(int argc, char *argv[])
{

    // Create the event manager and test controller
  CPPUNIT_NS::TestResult controller;

  // Add a listener that colllects test result
  CPPUNIT_NS::TestResultCollector result;
  controller.addListener( &result );        

  // Add a listener that print dots as test run.
  CPPUNIT_NS::BriefTestProgressListener progress;
  controller.addListener( &progress );      

  // Add the top suite to the test runner
  CPPUNIT_NS::TestRunner runner;
  runner.addTest( CPPUNIT_NS::TestFactoryRegistry::getRegistry().makeTest() );
  runner.run( controller );

  // Print test in a compiler compatible format.
  CPPUNIT_NS::CompilerOutputter outputter( &result, std::cerr );
  std::cout.flush();
  outputter.write(); 

  return result.wasSuccessful() ? 0 : 1;
}

