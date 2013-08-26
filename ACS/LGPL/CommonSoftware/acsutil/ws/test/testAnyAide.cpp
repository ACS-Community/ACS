/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005 
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
* "@(#) $Id: testAnyAide.cpp,v 1.8 2008/08/21 15:35:49 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-09-20  created
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "acsutilAnyAide.h"
#include <baciC.h>

static char *rcsId="@(#) $Id: testAnyAide.cpp,v 1.8 2008/08/21 15:35:49 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

void 
printId(const char* predictedType,
	const CORBA::Any &any)
{
    std::string blah = AnyAide::getId(any);
    printf("The '%s' any has an ID of '%s'\n", predictedType, blah.c_str());
}

template <class T>
T
testAny(const char* type,
	const CORBA::Any &any)
{
    printId(type, any);

    try
	{
	printf("Stringified any value is: %s\n", AnyAide::anyToString(any).c_str());
	}
    catch(...)
	{
	printf("Cannot stringify any's of type '%s' yet!\n", type);
	}

    T tVal = AnyAide::getValue<T>(any);

    printf("Extracted '%s' any value is: ", type);
    return tVal;
}

#define IS_XYZ_TEST(any, isXyz) \
if (AnyAide::isXyz(any)) \
{ \
printf("'%s' test passed!\n", #isXyz); \
} \
else \
{ \
printf("'%s' test failed!\n", #isXyz); \
}

#include "acsutilORBHelper.h"

int main(int argc, char *argv[])
{
    CORBA::ORB_ptr orb_p = ORBHelper::getORB();

    CORBA::Any nilCORBAObjectAny;
    CORBA::Object_ptr nilCORBAObjectVal = 0;
    AnyAide::setValue<CORBA::Object_ptr>(nilCORBAObjectAny, nilCORBAObjectVal);

    CORBA::Any nullAny;

    CORBA::Any stringAny;
    const char* stringVal = "a string";
    AnyAide::setValue<const char*>(stringAny, stringVal);

    CORBA::Any doubleAny;
    CORBA::Double doubleVal = 3.14;
    AnyAide::setValue<CORBA::Double>(doubleAny, doubleVal);

    CORBA::Any floatAny;
    CORBA::Float floatVal = 3.1;
    AnyAide::setValue<CORBA::Float>(floatAny, floatVal);

    CORBA::Any longAny;
    CORBA::Long longVal = 1;
    AnyAide::setValue<CORBA::Long>(longAny, longVal);

    CORBA::Any longLongAny;
    CORBA::LongLong longLongVal = 2;
    AnyAide::setValue<CORBA::LongLong>(longLongAny, longLongVal);

    CORBA::Any uLongLongAny;
    CORBA::ULongLong uLongLongVal = 3;
    AnyAide::setValue<CORBA::ULongLong>(uLongLongAny, uLongLongVal);

    CORBA::Any patternAny;
    ACS::pattern patternVal = 4;
    AnyAide::setValue<ACS::pattern>(patternAny, patternVal);

    CORBA::Any enumAny;
    ACS::Condition enumVal = ACS::RED;
    AnyAide::setValue<ACS::Condition>(enumAny, enumVal);

    CORBA::Any doubleSeqAny;
    ACS::doubleSeq doubleSeqVal(1);
    doubleSeqVal.length(1);
    doubleSeqVal[0] = 3.14;
    AnyAide::setValue<ACS::doubleSeq>(doubleSeqAny, doubleSeqVal);

    CORBA::Any longSeqAny;
    ACS::longSeq longSeqVal(1);
    longSeqVal.length(1);
    longSeqVal[0] = 17;
    AnyAide::setValue<ACS::longSeq>(longSeqAny, longSeqVal);

    CORBA::Any stringSeqAny;
    ACS::stringSeq stringSeqVal(1);
    stringSeqVal.length(1);
    stringSeqVal[0] = "another string";
    AnyAide::setValue<ACS::stringSeq>(stringSeqAny, stringSeqVal);

    CORBA::Any floatSeqAny;
    ACS::floatSeq floatSeqVal(1);
    floatSeqVal.length(1);
    floatSeqVal[0] = 3.1;
    AnyAide::setValue<ACS::floatSeq>(floatSeqAny, floatSeqVal);


//    CORBA::Any doubleSeqSeqAny;
//    ACS::doubleSeqSeq doubleSeqSeqVal(1);
//    doubleSeqVal.length(1);
//    doubleSeqVal[0] = 3.14;
//    AnyAide::setValue<ACS::doubleSeqSeq>(doubleSeqSeqAny, doubleSeqSeqVal);
    //TODO - provide test for CORBA objects. Should this really be necessary
    //since we technically do not support them?
    //testAny<CORBA::Object_ptr>("nilCORBAObject", nilCORBAObjectAny);
    //CORBA::Object_var tObject = AnyAide::getValue<CORBA::Object_ptr>(nilCORBAObjectAny);
    
    //null values are a very special case and cannot be tested using the template
    IS_XYZ_TEST(nullAny, isNull);
    {
    printId("null", nullAny);
    printf("Stringified any value is: %s\n\n", AnyAide::anyToString(nullAny).c_str());
    }

    IS_XYZ_TEST(stringAny, isString);
    const char* tString = testAny<const char*>("string", stringAny);
    printf("%s.\n\n", tString);

    IS_XYZ_TEST(doubleAny, isDouble);
    CORBA::Double tDouble = testAny<CORBA::Double>("double", doubleAny);
    printf("%f.\n\n", tDouble);

    IS_XYZ_TEST(floatAny, isFloat);
    CORBA::Float tFloat = testAny<CORBA::Float>("float", floatAny);
    printf("%f.\n\n", tFloat);
    
    IS_XYZ_TEST(longAny, isLong);
    CORBA::Long tLong = testAny<CORBA::Long>("long", longAny);
    printf("%d.\n\n", tLong);

    IS_XYZ_TEST(longLongAny, isLongLong);
    CORBA::LongLong tLongLong = testAny<CORBA::LongLong>("longlong", longLongAny);
    printf("%d.\n\n", tLongLong);
    
    IS_XYZ_TEST(uLongLongAny, isULongLong);
    CORBA::ULongLong tULongLong = testAny<CORBA::ULongLong>("ulonglong", uLongLongAny);
    printf("%d.\n\n", tULongLong);

    IS_XYZ_TEST(patternAny, isPattern);
    CORBA::ULongLong tULong = testAny<CORBA::ULongLong>("pattern", patternAny);
    printf("%d.\n\n", tULong);

    IS_XYZ_TEST(enumAny, isEnum);
    ACS::Condition tCondition = testAny<ACS::Condition>("enum", enumAny);
    printf("%d.\n\n", (unsigned long)tCondition);

    IS_XYZ_TEST(doubleSeqAny, isDoubleSeq);
    ACS::doubleSeq* tDoubleSeq = testAny<ACS::doubleSeq *>("doubleSeq", doubleSeqAny);
    printf("[ %f ].\n\n", (*tDoubleSeq)[0]);
    
    IS_XYZ_TEST(longSeqAny, isLongSeq);
    ACS::longSeq* tLongSeq = testAny<ACS::longSeq *>("longSeq", longSeqAny);
    printf("[ %d ].\n\n", (*tLongSeq)[0]);

    IS_XYZ_TEST(stringSeqAny, isStringSeq);
    ACS::stringSeq* tStringSeq = testAny<ACS::stringSeq *>("stringSeq", stringSeqAny);
    printf("[ %s ].\n\n", (*tStringSeq)[0].in());

    IS_XYZ_TEST(floatSeqAny, isFloatSeq);
    ACS::floatSeq* tFloatSeq = testAny<ACS::floatSeq *>("floatSeq", floatSeqAny);
    printf("[ %f ].\n\n", (*tFloatSeq)[0]);

//    IS_XYZ_TEST(doubleSeqSeqAny, isDoubleSeqSeq);
//    ACS::doubleSeqSeq* tDoubleSeqSeq = testAny<ACS::doubleSeqSeq *>("doubleSeqSeq", doubleSeqSeqAny);
    //TODO - test isEnum
    //TODO - test isStruct
    
    return 0;
}
