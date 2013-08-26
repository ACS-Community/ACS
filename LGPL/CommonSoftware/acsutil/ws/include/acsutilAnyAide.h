#ifndef ACSUTIL_ANY_AIDE_H
#define ACSUTIL_ANY_AIDE_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsutilAnyAide.h,v 1.11 2008/10/09 02:23:13 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-08-11  created
*/
////////////////////////////////////////////////////////////////////////
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif
////////////////////////////////////////////////////////////////////////
/** @file acsutilAnyAide.h
 * Header file for tools to assist developers with CORBA anys.
 */

#include <sstream>
#include <tao/AnyTypeCode/Any.h>
#include <tao/AnyTypeCode/TypeCode.h>

/**
 * AnyAide is a class whose sole purpose is to assist ALMA developers
 * with using CORBA anys. It just provides a bunch of static methods
 * at the moment and has not been tested yet.
 */
class AnyAide
{
  public:

    /**
     * Exception thrown when the wrong template parameter is
     * encountered.
     * @param correctParameter The correct template parameter that 
     * should have been used.
     */
    struct WrongTemplateParameter
    {
	std::string correctParameter;
    };
    
    /**
     * An exception thrown when you try to use an any that has some
     * kind of unsupported type of value encoded within it.
     * @param type ID of the unsupported type.
     */
    struct UnsupportedType
    {
	std::string type;
    };


    /**
     * Returns the CORBA::TCKind of the any's underlying type. TCKind is
     * an enum created by OMG/CORBA (see page 693 of Advanced CORBA 
     * Programming with C++) which defines enumerations like tk_null, 
     * tk_ulong, tk_enum, tk_struct, etc.
     */
    static CORBA::TCKind
    getRealType(const CORBA::Any&);

    /**
     * Returns the IFR ID of the any (i.e., "IDL:alma/someModule/someEnum:1.0").
     * This method can only be used on any's with the following TCKind:
     * - tk_objref
     * - tk_struct
     * - tk_union
     * - tk_enum
     * - tk_alias
     * - tk_except
     * - simple CORBA types supported by BACI (i.e., tk_float, tk_ulonglong, etc)
     * 
     * @throws an UnsupportedType exception if we cannot determine the any's IFR ID.
     
     * this method thro.
     */
    static std::string
    getId(const CORBA::Any&);

    /**
     * Converts the any's value to a string. Only works for native CORBA types such
     * as longs, doubles, strings, etc at the moment and sequence typdefs defined 
     * within acscommon.idl. Support for enumerations is given however.
     * @param a CORBA any with a supported BACI type embedded within it
     * @param An optional parameter which sets the the precision of the return value.
     *        Only used when non-zero (zero is the default value).
     * @return the any param converted to a string
     * @throws UnsupportedType exception if the implementation does not know about the 
     * CORBA type embedded within the any parameter.
     */
    static std::string
    anyToString(const CORBA::Any&, unsigned short precision=0);

    /**
     * Converts a CORBA any to its real value and returns that. In the event
     * that the template parameter T is not compatible with the any, an exception
     * is thrown. Typical usage of this method will be something similar too:
     * @throw WrongTemplateParameter
     * ...
     * CORBA::any joe = ...;
     * if (AnyAide::isPattern(joe) == true)
     * {
     *   CORBA::Ulong myLong = getValue<CORBA::Ulong>(joe);
     * }
     */
    template <class T> 
    static T
    getValue(const CORBA::Any& any)
	{
	    T returnVal;
	    //standard CORBA way of extracting an any
	    if((any >>= returnVal)==false)
		{
		//it failed because it's the wrong type
		//just throw an exception.
		WrongTemplateParameter except;
		except.correctParameter = getId(any);
		throw except;
		}
	    return returnVal;
	}

    /**
     * Sets the value of a CORBA any.
     * This method sets the value of a CORBA any by copying
     * the value.
     * @param any CORBA any the value will be embedded into
     * @param value Value to be placed in the CORBA any
     */
    template <class T>
    static void
    setValue(CORBA::Any& any,
	     const T& value)
	{
	    any <<= value;
	}


    //------------------------------------------------
    //set of functions below designed to tell developers
    //if the any is of a given type.

    /**
     * Returns true if the any contains a null value.
     */
    static bool
    isNull(const CORBA::Any&);

    /**
     * Returns true if the any contains a string value.
     */
    static bool
    isString(const CORBA::Any&);

    /**
     * Returns true if the any contains a double value.
     */
    static bool
    isDouble(const CORBA::Any&);

    /**
     * Returns true if the any contains a float value.
     */
    static bool
    isFloat(const CORBA::Any&);

    /**
     * Returns true if the any contains a long value.
     */
    static bool
    isLong(const CORBA::Any&);

    /**
     * Returns true if the any contains a long long value.
     */
    static bool
    isLongLong(const CORBA::Any&);

    /**
     * Returns true if the any contains a unsigned long long value.
     */
    static bool
    isULongLong(const CORBA::Any&);

    /**
     * Returns true if the any contains a pattern value.
     */
    static bool
    isULong(const CORBA::Any&);

    /**
     * Returns true if the any contains a pattern value.
     */
    static bool
    isPattern(const CORBA::Any&);

    /**
     * Returns true if the any contains a doubleSeq value.
     */
    static bool
    isDoubleSeq(const CORBA::Any&);

    /**
     * Returns true if the any contains a longSeq value.
     */
    static bool
    isLongSeq(const CORBA::Any&);

    /**
     * Returns true if the any contains a stringSeq value.
     */
    static bool
    isStringSeq(const CORBA::Any&);

    /**
     * Returns true if the any contains a floatSeq value.
     */
    static bool
    isFloatSeq(const CORBA::Any&);

    /**
     * Returns true if the any contains an enum value.
     */
    static bool
    isEnum(const CORBA::Any&);

    /**
     * Returns true if the any contains a struct value.
     */
    static bool
    isStruct(const CORBA::Any&);

    //------------------------------------------------------
    //--members below define the ID returned by the getId
    //--method for SIMPLE TYPES ONLY! this is done because
    //--there is no way to get the IFR ID of these types
    //--from the CORBA any. with complex types like enumerations,
    //--structs, etc we do not have to declare these here
    //--because this info can be obtained dynamically

    /** ACS(CORBA) null IFR ID */
    static const std::string nullType_m;

    /** ACS(CORBA) string IFR ID */
    static const std::string stringType_m;

    /** ACS(CORBA) double IFR ID */
    static const std::string doubleType_m;
    
    /** ACS(CORBA) float IFR ID */
    static const std::string floatType_m;
 
    /** ACS(CORBA) long IFR ID */
    static const std::string longType_m;
    
    /** ACS(CORBA) longlong IFR ID */
    static const std::string longLongType_m;
    
    /** ACS(CORBA) ulonglong IFR ID */
    static const std::string uLongLongType_m;

    /** ACS(CORBA) ulong  IFR ID */
    static const std::string uLongType_m;

    /** ACS(CORBA) ACS::Pattern (aka unsigned long long) IFR ID */
    static const std::string patternType_m;
    
    /** ACS(CORBA) doubleSeq IFR ID */
    static const std::string doubleSeqType_m;
    
    /** ACS(CORBA) longSeq IFR ID */
    static const std::string longSeqType_m; 
    
    /** ACS(CORBA) stringSeq IFR ID */
    static const std::string stringSeqType_m;
    
    /** ACS(CORBA) floatSeq IFR ID */
    static const std::string floatSeqType_m;
    
    /** This ID is returned by methods in the AnyAide
     * class if the IFR ID cannot be determined for
     * some reason or another. This is possible for
     * certain simple CORBA types (short, char, etc)
     * that are not supported by BACI
     */
    static const std::string unknownType_m;


  private:
    /**
     * Converts the (enum) any's value to a string.
     * @throw UnsupportedType
     * @return the any param converted to a string
     */
    static std::string
    enumToString(const CORBA::Any&);

};

#endif
