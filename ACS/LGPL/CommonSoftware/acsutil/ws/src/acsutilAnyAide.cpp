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
* "@(#) $Id: acsutilAnyAide.cpp,v 1.10 2008/10/09 02:23:13 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-09-19  created 
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


#include <iomanip>
#include <acscommonC.h>
#include <tao/DynamicAny/DynamicAny.h>

#include "acsutilAnyAide.h"
#include "acsutilORBHelper.h"

static char *rcsId="@(#) $Id: acsutilAnyAide.cpp,v 1.10 2008/10/09 02:23:13 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

const std::string AnyAide::nullType_m      = "null";
const std::string AnyAide::stringType_m    = "string";
const std::string AnyAide::doubleType_m    = "double";
const std::string AnyAide::floatType_m     = "float";
const std::string AnyAide::longType_m      = "long";
const std::string AnyAide::uLongType_m      = "long";
const std::string AnyAide::longLongType_m  = "longlong";
const std::string AnyAide::uLongLongType_m = "ulonglong-pattern";
const std::string AnyAide::patternType_m   = "pattern";
const std::string AnyAide::doubleSeqType_m = "IDL:alma/ACS/doubleSeq:1.0";
const std::string AnyAide::longSeqType_m   = "IDL:alma/ACS/longSeq:1.0";
const std::string AnyAide::stringSeqType_m = "IDL:alma/ACS/stringSeq:1.0";
const std::string AnyAide::floatSeqType_m  = "IDL:alma/ACS/floatSeq:1.0";
const std::string AnyAide::unknownType_m   = "Unknown";
//----------------------------------------------------------------------
bool
AnyAide::isNull(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_null);
}
//----------------------------------------------------------------------
bool
AnyAide::isString(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_string);
}
//----------------------------------------------------------------------
bool
AnyAide::isDouble(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_double);
}
//----------------------------------------------------------------------
bool
AnyAide::isFloat(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_float);
}
//----------------------------------------------------------------------
bool
AnyAide::isLong(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_long);
}
//----------------------------------------------------------------------
bool
AnyAide::isLongLong(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_longlong);
}
//----------------------------------------------------------------------
bool
AnyAide::isULongLong(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_ulonglong);
}
//----------------------------------------------------------------------
bool
AnyAide::isULong(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_ulong);
}
//----------------------------------------------------------------------
bool
AnyAide::isPattern(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_ulonglong);
}
//----------------------------------------------------------------------
bool
AnyAide::isDoubleSeq(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getId(any)==doubleSeqType_m);
}
//----------------------------------------------------------------------
bool
AnyAide::isLongSeq(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getId(any)==longSeqType_m);
}
//----------------------------------------------------------------------
bool
AnyAide::isStringSeq(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getId(any)==stringSeqType_m);
}
//----------------------------------------------------------------------
bool
AnyAide::isFloatSeq(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getId(any)==floatSeqType_m);
}
//----------------------------------------------------------------------
bool
AnyAide::isEnum(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_enum);
}
//----------------------------------------------------------------------
bool
AnyAide::isStruct(const CORBA::Any& any)
{
    //basically just delegate the call to another helper function
    //and include a check against the type.
    return (getRealType(any)==CORBA::tk_struct);
}
//----------------------------------------------------------------------
CORBA::TCKind
AnyAide::getRealType(const CORBA::Any& any)
{
    //have to create an _var type because this is actually a CORBA object
    CORBA::TypeCode_var tc;
    //get the type from the any
    tc = any.type();

    //return the kind...simple enough.
    return tc->kind();
}

//----------------------------------------------------------------------
std::string
AnyAide::getId(const CORBA::Any& any)
{
    CORBA::TCKind kind = getRealType(any);
    
    //great - the identifier is already provided
    if ((kind==CORBA::tk_objref) ||
	(kind==CORBA::tk_struct) ||
	(kind==CORBA::tk_union)  ||
	(kind==CORBA::tk_enum)   ||
	(kind==CORBA::tk_except))
	{
	//have to create an _var type because this is actually a CORBA object
	CORBA::TypeCode_var tc;
	//get the type from the any
	tc = any.type();
	return std::string(tc->id());
	}
    else if(kind==CORBA::tk_null)
	{
	return nullType_m;
	}
    else if(kind==CORBA::tk_string)
	{
	return stringType_m;
	}
    else if(kind==CORBA::tk_double)
	{
	return doubleType_m;
	}
    else if(kind==CORBA::tk_long)
	{
	return longType_m;
	}
    else if(kind==CORBA::tk_ulong)
	{
	return uLongType_m;
	}
    else if(kind==CORBA::tk_longlong)
	{
	return longLongType_m;
	}
    else if(kind==CORBA::tk_ulonglong)
	{
	return uLongLongType_m;
	}
    else if(kind==CORBA::tk_float)
	{
	return floatType_m;
	}
    //aliases can be ...
    else if(kind==CORBA::tk_alias)
	{
	//first get a hold of the IFR id
	CORBA::TypeCode_var tc;
	//get the type from the any
	tc = any.type();

	return std::string(tc->id());
	}
    // after TAO 1.5.2 we have to handel seqence separatly
    else if (kind==CORBA::tk_sequence)
	{
//!!! here we play dirty !!!
// this solution does not work with seq of seq
// we can change it but first we have to change it on the places where seqences are used !!
	CORBA::TypeCode_var tc;
	//get the type from the any
	tc = any.type();
	//create another any with type of content type (long/double ..)
	CORBA::Any a;
	a._tao_set_typecode(tc->content_type());
	// get recursivly the ID of contained type
	std::string c = getId(a);
	return std::string("IDL:alma/ACS/" + c + "Seq:1.0"); // very dirty but should be OK 
	}
    
    //bad case
    else
	{
	UnsupportedType except;
	except.type = unknownType_m;
	throw except;
	}
}
//----------------------------------------------------------------------
std::string
AnyAide::anyToString(const CORBA::Any& value, unsigned short precision)
{
    
    std::ostringstream ostr;
    //if they've changed the precision from the default value, set it
    if(precision!=0)
	{
	ostr.precision(precision);
	}
    
    if (AnyAide::isString(value)==true)
	{
	ostr << AnyAide::getValue<const char *>(value);
	}
    
    else if (AnyAide::isDouble(value)==true)
	{
	ostr << AnyAide::getValue<CORBA::Double>(value);
	}
    
    else if (AnyAide::isFloat(value)==true)
	{
	ostr <<  AnyAide::getValue<CORBA::Float>(value);
	}
    
    else if (AnyAide::isLong(value)==true)
	{
	ostr << AnyAide::getValue<CORBA::Long>(value);
	}
    
    else if (AnyAide::isLongLong(value)==true)
	{
	ostr << AnyAide::getValue<CORBA::LongLong>(value);
	}
    
    else if (AnyAide::isULongLong(value)==true)
	{
	ostr << AnyAide::getValue<CORBA::ULongLong>(value);
	}
    
    else if (AnyAide::isPattern(value)==true)
	{
	ostr << AnyAide::getValue<CORBA::ULong>(value);
	}

    else if (AnyAide::isDoubleSeq(value)==true)
	{
	ACS::doubleSeq* seqVar = AnyAide::getValue<ACS::doubleSeq *>(value);
	for (unsigned int i = 0; i < seqVar->length(); i++)
	    {
	    ostr << (*seqVar)[i] << " ";
	    }
	}
    
    else if (AnyAide::isLongSeq(value)==true)
	{
	ACS::longSeq* seqVar = AnyAide::getValue<ACS::longSeq *>(value);
	for (unsigned int i = 0; i < seqVar->length(); i++)
	    {
	    ostr << (*seqVar)[i] << " ";
	    }
	}
    
    else if (AnyAide::isStringSeq(value)==true)
	{
	ACS::stringSeq* seqVar = AnyAide::getValue<ACS::stringSeq *>(value);
	for (unsigned int i = 0; i < seqVar->length(); i++)
	    {
	    ostr << (*seqVar)[i] << " ";
	    }
	}
    
    else if (AnyAide::isFloatSeq(value)==true)
	{
	ACS::floatSeq* seqVar = AnyAide::getValue<ACS::floatSeq *>(value);
	for (unsigned int i = 0; i < seqVar->length(); i++)
	    {
	    ostr <<  (*seqVar)[i] << " ";
	    }
	}

    else if (AnyAide::isEnum(value)==true)
	{
	//just delegate to a helper method
	return enumToString(value);
	}

    else if (AnyAide::isNull(value)==true)
	{
	ostr << AnyAide::nullType_m;
	}
    
    else
	{
	UnsupportedType except;
	except.type = "type not supported yet";
	throw except;
	}

    ostr << std::ends; //end it

    //Doublecheck to see if this is a memory leak!!!
    return ostr.str();
}
//----------------------------------------------------------------------
std::string
AnyAide::enumToString(const CORBA::Any& value)
{
    CORBA::ORB_ptr orb_p = ORBHelper::getORB();

    //get the dynamic any factory
    CORBA::Object_var factory_obj = orb_p->resolve_initial_references ("DynAnyFactory");
    //narrow it
    DynamicAny::DynAnyFactory_var dynany_factory = DynamicAny::DynAnyFactory::_narrow(factory_obj.in());
    //sanity check
    ACE_ASSERT(!CORBA::is_nil(dynany_factory.in()));
    
    //get the dynamic any
    DynamicAny::DynAny_var dynValue = dynany_factory->create_dyn_any(value);
    //sanity check
    ACE_ASSERT(!CORBA::is_nil(dynValue.in()));
    //narrow it to an enum
    DynamicAny::DynEnum_var dynEnum = DynamicAny::DynEnum::_narrow(dynValue.in());
    //another sanity check
    ACE_ASSERT(!CORBA::is_nil(dynEnum.in()));

    char* tString = dynEnum->get_as_string();
    std::string retVal(tString);

    //destroy the dynamic any
    dynEnum->destroy();

    //free up some dynamic memory
    CORBA::string_free(tString);

    return retVal;
}
//----------------------------------------------------------------------
