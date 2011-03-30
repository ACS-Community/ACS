#ifndef baciValue_H
#define baciValue_H
/*******************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2004
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
 * "@(#) $Id: baciValue.h,v 1.111 2011/03/30 17:57:23 tstaig Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * bjeram    2003/08/08  changed octet inlineData_m[8] to double inlineData_m[2] duo to having problem on Sun with alignment
 * bjeram    2003/02/11  added MUTATOR(string, ACE_CString)
 * bjeram    2002/12/19  added accesor mutator template functions (temporary!!!)
 * msekoran  2001/07/26  fixed string type
 * msekoran  2001/03/02  created
 */

/**
 * @file
 * Header file for BACI Values.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <baciExport.h>
#include <baciTypes.h>
#include <ace/SString.h>

namespace baci {

/**
 * WhyNull return value definitions
 */
/// Value is not a null type value
#define NOT_NULL_VALUE      0
/// A null type value
#define VALUE_NULL          1
/// An uninitialized value
#define VALUE_UNINITIALIZED 2
/// Value type not implemented by BACIValue
#define VALUE_NONEXISTENT   3

/**
 * Generic data type used in BACI
 * Class BACIValue is a data type wrapper for all
 * BACI supported types. It provides several methods
 * needed by BACI (compare operators, abs. diff., etc.)
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */
class baci_EXPORT BACIValue
{

  public:

    /**
     * Enumeration of all supported data types. If it's not listed
     * here, it cannot be packed into a #BACIValue.
     * <br><hr>
     */
    enum Type
    {
	/// No-value. Can also be used to imply special meaning.
	type_null=0,
	/// Not really a type. This is just a void pointer used to pass any user-defined
	/// structures as a BACI Action parameter. <b>THIS COULD NOT BE USED AS BACI TYPE!</b>
	type_pointer=1,
	/// A string.
	type_string=2,
	/// A IEEE 8-byte floating point number.
	type_double=3,
	/// 32-bit signed integer.
	type_long=4,
	/// A bit pattern.
	type_pattern=9, //5,
	/// Sequence of double-s.
	type_doubleSeq=6,
	/// Sequencs of long-s.
	type_longSeq=7,
	/// 64-bit signed integer.
	type_longLong=8,
	/// 64-bit unsigned integer.
	type_uLongLong=9,
	/// Sequence of string-s.
	type_stringSeq=10,
	/// A IEEE 4-byte floating point number.
	type_float=11,
	/// Sequence of float-s.
	type_floatSeq=12
    };

    /**
     * An 8-bit unsigned integer.
     * <br><hr>
     */
    typedef unsigned char octet;

    /**
     * Array of strings initialized to contain what are essentially
     * the same values as the #Type enum. Values are:
     *     "null","pointer","string","double","long","pattern","doubleSeq",
     *     "longSeq","longLong","uLongLong", "longString"
     * Furthermore, this array can be indexed using the #Type enum.
     * <br><hr>
     */
    static const ACE_CString typeName[];

    /**
     * Array of strings initialized to contain what are essentially
     * the same values as the #Type enum. Values are:
     *     "invalid","invalid","string","double","long","long",
     *     "doubleSeq" (not supported by logging),"longSeq","longLong",
     *     "uLongLong","longString"
     * Furthermore, this array can be indexed using the #Type enum.
     * <br><hr>
     */
    static const ACE_CString archiveTypeName[];

    /**
     * A global predefined null BACIValue.
     * <br><hr>
     */
    static const BACIValue NullValue;

    // --Constructors--------------------------------------------------------
    /**
     * Standard Contructor. The wrapped value will carry a Type::type_null value.
     * <br><hr>
     */
    BACIValue();

    /**
     * Constructor with initialized type.
     * By using this constructor, one can create a #BACIValue object with any
     * type, although the value is not directly settable.
     * @param type Type of the wrapped value.
     * @param bound bound (for varialble length types)
     * <br><hr>
     */
    BACIValue(const Type type, const unsigned long bound);

    /**
     * Copy constructor: construct a value that will store the same data as the specified value.
     * @param value A pre-existing #BACIValue object
     * <br><hr>
     */
    BACIValue(const BACIValue &value);

    /** Constructor BACIdouble
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const BACIdouble &value);
    /** Constructor BACIfoat
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const BACIfloat &value);
    /** Constructor BACIlong
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const BACIlong &value);
    /** Constructor BACIpattern
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
//TOBE deleted    BACIValue(const BACIpattern &value);
    /** Constructor BACIlongLong
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const BACIlongLong &value);
    /** Constructor realType
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const BACIuLongLong &value);
    /** Constructor ACE_CString
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const ACE_CString &value);
    /** Constructor char*
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const char* value);
    /** Constructor - <b>Special case</b>
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const void* &value);
    /** Constructor - pointer (<b>Special case</b>
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(void * value);
    /** Constructor BACIdoubleSeq
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const BACIdoubleSeq &value);
    /** Constructor BACIfloatSeq
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const BACIfloatSeq &value);
    /** Constructor BACIlongSeq
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const BACIlongSeq &value);
    /** Constructor BACIstringSeq
     * @param value A constant reference to a value which will be this #BACIValue
     * object's underlying value.
     * <br><hr>
     */
    BACIValue(const BACIstringSeq &value);

    /**
     * Constructor CORBA::Any.
     * Deprecated. Used to support CORBA enums.
     * It is used by test in baci module. 
     */
    BACIValue(const BACIpattern &value, const CORBA::Any& any);

    /**
     * Resets value to non-initialized state(VALUE_UNINITIALIZED, type_null type).
     * <br><hr>
     */
    void reset();

    // --Operators-----------------------------------------------------------
    /**
     * Equality Operator
     * <br><hr>
     */
    BACIValue& operator=(const BACIValue &value);
    /**
     * Check equals to Operator
     * <br><hr>
     */
    bool operator==(const BACIValue &value) const;
    /**
     * Check less-than Operator
     * <br><hr>
     */
    bool operator<(const BACIValue &value) const;
    /**
     * Check less-than or equal to Operator
     * <br><hr>
     */
    bool operator<=(const BACIValue &value) const;

    /**
     * Check if absolute difference between values is less than delta value
     * @param value value
     * @param delta delta value
     * @return true if difference between this object's value and given value is less that delta value
     * <br><hr>
     */
    bool lessThanDelta(const BACIValue &value, const BACIValue &delta) const;

    /**
     * Check if percentual difference between values is less than delta value
     * @param value value
     * @param delta percentual delta value
     * @return true if percentual difference between this object's value and given value is less that delta value
     * <br><hr>
     */
    bool lessThanPercentDelta(const BACIValue &value, const BACIValue &delta) const;

    // --Methods-------------------------------------------------------------
    /**
     * Set the data type that this value will store.
     * Makes the value capable of storing data of type Type. If the type
     * currently being stored by the filed does not match Type, it is properly
     * disposed. After using this function, the user should not assume that
     * the data has been initialized, i.e. using accessor before a mutator is illegal.
     * Example: <pre>
     *       BACIValue value;
     *       value.setType(BACIValue::type_string);
     * </pre>
     * <br><hr>
     */
    bool setType(Type type, unsigned long bound = 0);

    /**
     * Get value type
     * @return value type
     * <br><hr>
     */
    Type getType() const { return type_m;}

    /**
     * Get value type
     * @return archive type of the value
     * <br><hr>
     */
    const char * getArchiveType() const { return archiveTypeName[type_m].c_str(); }

    /**
     * Get value bound (for variable length data types; eg. strings)
     * @return value bound
     * <br><hr>
     */
    unsigned long getBound() const { return ptr_m.bound; };

    /**
     * Is value null value (not initialized)
     * @return true if is null, false otherwise
     * <br><hr>
     */
    unsigned long isNull() const { return type_m==type_null; }

    /**
     * Return the reason why the value was set as null value.
     * @return A code explaining the reason why the value is set to null.
     * If NOT_NULL_VALUE is returned, the value is not null.
     * Otherwise, one of these can be expected:
     *    VALUE_NULL - The value is null because it is meant to carry a null value.
     *    VALUE_UNINITIALIZED - The value is null because it has not yet been initialized to another value.
     *    VALUE_NONEXISTENT - The value does not exist (data type was not implemented).
     * <br><hr>
     */
    unsigned long whyIsNull() const { return (type_m==type_null)?whyNull_m:NOT_NULL_VALUE; }

    /**
     * Check if value equals no change (e.g. for double 0.0, int 0)
     * @return if value equals no change
     * <br><hr>
     */
    bool noDelta() const;

    /**
     * Destructor
     * <br><hr>
     */
    ~BACIValue();

    // ----------------------------------------------------------------------
    /**
     * Given a pointer to a baci::BACIdouble, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_double
     * <br><hr>
     */
    static Type mapType(BACIdouble *v=0){  ACE_UNUSED_ARG(v); return type_double; }
    /**
     * Given a pointer to a baci::BACIfloat, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_float
     * <br><hr>
     */
    static Type mapType(BACIfloat *v=0){  ACE_UNUSED_ARG(v); return type_float; }
    /**
     * Given a pointer to a baci::BACIlong, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_long
     * <br><hr>
     */
    static Type mapType(BACIlong *v=0){  ACE_UNUSED_ARG(v); return type_long; }
    /**
     * Given a pointer to a baci::BACIpattern, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_pattern
     * <br><hr>
     */
//TOBE deleted    static Type mapType(BACIpattern *v=0){  ACE_UNUSED_ARG(v); return type_pattern; }
    /**
     * Given a pointer to a char*, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_string
     * <br><hr>
     */
    static Type mapType(char* *v=0){  ACE_UNUSED_ARG(v); return type_string; }
    /**
     * Given a pointer to an ACE_CString, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_string
     * <br><hr>
     */
    static Type mapType(ACE_CString *v=0){  ACE_UNUSED_ARG(v); return type_string; }
    /**
     * Given a pointer to a void*, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_pointer
     * <br><hr>
     */
    static Type mapType(void* *v=0){  ACE_UNUSED_ARG(v); return type_pointer; }
    /**
     * Given a pointer to a baci::BACIdoubleSeq, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_doubleSeq
     * <br><hr>
     */
    static Type mapType(BACIdoubleSeq *v=0){  ACE_UNUSED_ARG(v); return type_doubleSeq; }
    /**
     * Given a pointer to a baci::BACIfloatSeq, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_floatSeq
     * <br><hr>
     */
    static Type mapType(BACIfloatSeq *v=0){  ACE_UNUSED_ARG(v); return type_floatSeq; }
    /**
     * Given a pointer to a baci::BACIlongSeq, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_longSeq
     * <br><hr>
     */
    static Type mapType(BACIlongSeq *v=0){  ACE_UNUSED_ARG(v); return type_longSeq; }
    /**
     * Given a pointer to a baci::BACIlongLong, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_longLong
     * <br><hr>
     */
    static Type mapType(BACIlongLong *v=0){  ACE_UNUSED_ARG(v); return type_longLong; }
    /**
     * Given a pointer to a baci::BACIuLongLong, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_uLongLong
     * <br><hr>
     */
    static Type mapType(BACIuLongLong *v=0){  ACE_UNUSED_ARG(v); return type_uLongLong; }
    /**
     * Given a pointer to a baci::BACIstringSeq, this static method returns a the #Type enumeration value.
     * @param v Pointer to a BACI data type
     * @return BACIValue::type_stringSeq
     * <br><hr>
     */
    static Type mapType(BACIstringSeq *v=0){  ACE_UNUSED_ARG(v); return type_stringSeq; }
    // ------------------------------------------------------------------
    //accessors
    /**
     * Stringifies this object's underlying BACI value.
     * @return String value of this object or null if it's not a string.
     * <br><hr>
     */
    const ACE_TCHAR* stringValue() const;
    /**
     * Given a pointer to an ACE C string, this method returns the stringified
     * value of this object.
     * @param v is an ACE_CString *. Invoker should just provide a null reference.
     * @return String value of this object
     * <br><hr>
     */
    ACE_CString getValue(ACE_CString *v=0) const;
    /**
     * Given a const pointer to character pointers, this method returns the stringified
     * value of this object.
     * @param v is a constant char**. Invoker should just provide a null reference.
     * @return String value of this object
     * <br><hr>
     */
    char* getValue(const char **v=0) const;

    /**
     * Given a pointer to character pointers, this method returns the stringified
     * value of this object.
     * @param v is a char**. Invoker should just provide a null reference.
     * @return String value of this object
     * <br><hr>
     */
    char* getValue(char **v=0) const;

    /**
     * Returns this object's underlying BACI value as a double.
     * @return double value of this object or 0 if it's not a double.
     * <br><hr>
     */
    BACIdouble doubleValue() const;
    /**
     * Returns this object's underlying BACI value as a double.
     * @param v is a BACIdouble *. Invoker should just provide a null reference.
     * @return double value of this object or 0 if it's not a double.
     * <br><hr>
     */
    BACIdouble getValue(BACIdouble *v=0) const;
    /**
     * Returns this object's underlying BACI value as a long.
     * @return long value of this object or 0 if it's not a long.
     * <br><hr>
     */
    /**
     * Returns this object's underlying BACI value as a double.
     * @return double value of this object or 0 if it's not a double.
     * <br><hr>
     */
    BACIfloat floatValue() const;
    /**
     * Returns this object's underlying BACI value as a float.
     * @param v is a BACIfloat *. Invoker should just provide a null reference.
     * @return float value of this object or 0 if it's not a float.
     * <br><hr>
     */
    BACIfloat getValue(BACIfloat *v=0) const;
    /**
     * Returns this object's underlying BACI value as a long.
     * @return long value of this object or 0 if it's not a long.
     * <br><hr>
     */
    BACIlong longValue() const;
    /**
     * Returns this object's underlying BACI value as a long.
     * @param v is a BACIlong *. Invoker should just provide a null reference.
     * @return long value of this object or 0 if it's not a long.
     * <br><hr>
     */
    BACIlong getValue(BACIlong *v=0) const;
    /**
     * Returns this object's underlying BACI value as a long long.
     * @return long long value of this object or 0 if it's not a long.
     * <br><hr>
     */
    BACIlongLong longLongValue() const;
    /**
     * Returns this object's underlying BACI value as a long long.
     * @param v is a BACIlongLong *. Invoker should just provide a null reference.
     * @return long long value of this object or 0 if it's not a long.
     * <br><hr>
     */
    BACIlongLong getValue(BACIlongLong *v=0) const;
    /**
     * Returns this object's underlying BACI value as an unsigned long long.
     * @return unsigned long long value of this object or 0 if it's not a unsigned long long.
     * <br><hr>
     */
    BACIuLongLong uLongLongValue() const;
    /**
     * Returns this object's underlying BACI value as an unsigned long long.
     * @param v is a BACIuLongLong *. Invoker should just provide a null reference.
     * @return unsigned long long value of this object or 0 if it's not a unsigned long long.
     * <br><hr>
     */
    BACIuLongLong getValue(BACIuLongLong *v=0) const;
    /**
     * Returns this object's underlying BACI value as a pattern.
     * @return pattern value of this object or 0 if it's not a pattern.
     * <br><hr>
     */
    BACIpattern patternValue() const { return uLongLongValue(); }

    /**
     * Deprecated.
     * @return enum value of this object encoded into a CORBA any.
     * <br><hr>
     */
    CORBA::Any enumValue() const;

    /**
     * Returns this object's underlying BACI value as a pattern.
     * @param v is a BACIpattern *. Invoker should just provide a null reference.
     * @return pattern value of this object or 0 if it's not a pattern.
     * <br><hr>
     */
//TOBE deleted    BACIpattern getValue(BACIpattern *v=0) const;

    /**
     * Deprecated.
     * @param v is a CORBA::Any *. Invoker should just provide a null reference.
     * @return enum value of this object encoded into a CORBA any.
     * <br><hr>
     */
    CORBA::Any getValue(CORBA::Any *v=0) const;

    /**
     * Returns this object's underlying BACI value as a void pointer.
     * @return void pointer value of this object or null if it's not a void pointer.
     * <br><hr>
     */
    void* pointerValue() const;
    /**
     * Returns this object's underlying BACI value as a void pointer.
     * @param v is a void pointer *. Invoker should just provide a null reference.
     * @return void pointer value of this object or null if it's not a void pointer.
     * <br><hr>
     */
    void* getValue(void* *v=0) const;
    /**
     * Returns this object's underlying BACI value as a double sequence.
     * @return double sequence of this object or 0 if it's not a double sequence.
     * <br><hr>
     */
    BACIdoubleSeq doubleSeqValue() const;
    /**
     * Returns this object's underlying BACI value as a double sequence.
     * @param v is a BACIdoubleSeq *. Invoker should just provide a null reference.
     * @return double sequence of this object or 0 if it's not a double sequence.
     * <br><hr>
     */
    BACIdoubleSeq getValue(BACIdoubleSeq *v=0) const;
    /**
     * Returns this object's underlying BACI value as a double sequence.
     * @return float sequence of this object or 0 if it's not a float sequence.
     * <br><hr>
     */
    BACIfloatSeq floatSeqValue() const;
    /**
     * Returns this object's underlying BACI value as a float sequence.
     * @param v is a BACIfloatSeq *. Invoker should just provide a null reference.
     * @return float sequence of this object or 0 if it's not a float sequence.
     * <br><hr>
     */
    BACIfloatSeq getValue(BACIfloatSeq *v=0) const;
    /**
     * Returns this object's underlying BACI value as a long sequence.
     * @return long sequence value of this object or 0 if it's not a long sequence.
     * <br><hr>
     */
    BACIlongSeq longSeqValue() const;
    /**
     * Returns this object's underlying BACI value as a long sequence.
     * @param v is a BACIlongSeq *. Invoker should just provide a null reference.
     * @return long sequence value of this object or 0 if it's not a long sequence.
     * <br><hr>
     */
    BACIlongSeq getValue(BACIlongSeq *v=0) const;
    /**
     * Returns this object's underlying BACI value as a string sequence.
     * @return string sequence value of this object or 0 if it's not a not a string sequence.
     * <br><hr>
     */
    BACIstringSeq stringSeqValue() const;
    /**
     * Returns this object's underlying BACI value as a string sequence.
     * @param v is a BACIstringSeq *. Invoker should just provide a null reference.
     * @return string sequence value of this object or 0 if it's not a not a string sequence.
     * <br><hr>
     */
    BACIstringSeq getValue(BACIstringSeq *v=0) const;
    // ------------------------------------------------------------------
    /**
     * Mutator
     * @param value Reference to a BACIdouble that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool doubleValue(const BACIdouble &value);
    /**
     * Mutator
     * @param value Reference to a BACIdouble that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue(const BACIdouble &value);
    /**
     * Mutator
     * @param value Reference to a BACIfloat that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool floatValue(const BACIfloat &value);
    /**
     * Mutator
     * @param value Reference to a BACIfloat that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue(const BACIfloat &value);
    /**
     * Mutator
     * @param value Reference to a BACIlong that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool longValue(const BACIlong &value);
    /**
     * Mutator
     * @param value Reference to a BACIlong that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue(const BACIlong &value);
    /**
     * Mutator
     * @param value Reference to a BACIlongLong that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool longLongValue(const BACIlongLong &value);
    /**
     * Mutator
     * @param value Reference to a BACIuLongLong that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue(const BACIlongLong &value);
    /**
     * Mutator
     * @param value Reference to a BACIuLongLong that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool uLongLongValue(const BACIuLongLong &value);
    /**
     * Mutator
     * @param value Reference to a BACIuLongLong that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue(const BACIuLongLong &value);
    /**
     * Mutator
     * @param value Reference to a BACIpattern that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool patternValue(const BACIpattern &value){ return setValue(value); }

    /**
     * Mutator
     * @param value Reference to a CORBA::Any that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
//??    bool enumValue(const int &value, const CORBA::Any &anyVal);
    bool enumValue(const BACIpattern &value, const CORBA::Any &anyVal);

    /**
     * Mutator
     * @param value Reference to a BACIpattern that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
//TOBE deleted    bool setValue(const BACIpattern &value);

    /**
     * Deprecated mutator.
     * @param value Reference to a enum that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
//TOBE deleted    bool setValue(const BACIpattern &value, const CORBA::Any &anyVal);

    /**
     * Mutator
     * @param value Reference to an ACE C string that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool stringValue(const ACE_CString &value);
    /**
     * Mutator
     * @param value Reference to an ACE C string that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue(const ACE_CString &value);
    /**
     * Mutator
     * @param value Reference to a character pointer that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool stringValue (const char * value);
    /**
     * Mutator
     * @param value Reference to a character pointer that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue (const char * value);
    /**
     * Mutator
     * @param value Reference to a void pointer that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool pointerValue (void * value);
    /**
     * Mutator
     * @param value Reference to a BACIdoubleSeq that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool doubleSeqValue(const BACIdoubleSeq &value);
    /**
     * Mutator
     * @param value Reference to a BACIdoubleSeq that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue(const BACIdoubleSeq &value);
    /**
     * Mutator
     * @param value Reference to a BACIfloatSeq that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool floatSeqValue(const BACIfloatSeq &value);
    /**
     * Mutator
     * @param value Reference to a BACIfloatSeq that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue(const BACIfloatSeq &value);
    /**
     * Mutator
     * @param value Reference to a BACIlongSeq that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool longSeqValue(const BACIlongSeq &value);
    /**
     * Mutator
     * @param value Reference to a BACIlongSeq that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue(const BACIlongSeq &value);
    /**
     * Mutator
     * @param value Reference to a BACIstringSeq that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool stringSeqValue(const BACIstringSeq &value);
    /**
     * Mutator
     * @param value Reference to a BACIstringSeq that this object will copy.
     * @return If the value currently contains other type of data than the one being
     * stored, the operation has no effect and false is returned.
     * <br><hr>
     */
    bool setValue(const BACIstringSeq &value);
    // ------------------------------------------------------------------
    //Conversion helpers
    /**
     * Convert to string
     * @param value where the string's value will be written to
     * @param bool if true (false is default) value type is also given (eg. <double>12.8),
     * which could help in decoding data value (as implemented in fromString method)
     * @return true on succes, false on failure
     * @see fromString
     * <br><hr>
     */
    bool toString(ACE_CString &value, bool specifyType = false) const;

    /**
     * Convert from a string into a BACI type and encode it into this object.
     * @param value value represented as string
     * @param bool if true (false is default) value type is also given (eg. <double>12.8)
     * in input string, otherwise set type is used
     * @return true on succes, false on failure
     * @see toString
     * <br><hr>
     */
    bool fromString(const ACE_CString value, bool specifyType = false);
    // ------------------------------------------------------------------

    /**
     * Returns the BACIValue embedded within a CORBA any. Useful
     * with archiving.
     * @param Any reference in which we will store the underlying
     * BACIValue.
     */
    void
    getAny(CORBA::Any &any) const;

  protected:

    /**
     * Identifies the type of data currently being stored.
     */
    Type type_m;

    union
    {
	/**
	 * Inlined data can be up to 8 bytes in length. This is sufficient for
	 * storing octets, short, long, and longlong integers, doubles and
	 * floats.
	 * <br><hr>
	 */
	double inlineData_m[2];

	/**
	 * Other types store a pointer to the actual data and a bound
	 * on the amount of data that can be stored there (e.g., number of
	 * characters for strings). A bound of 0 indicates "no limits".
	 * <br><hr>
	 */
	struct
	{
	    unsigned long bound;
	    void *pointer;
	} ptr_m;

	/**
	 * Describes why the value has type null
	 */
	unsigned long whyNull_m;
    };

    /**
     * This any is used as a hack to support enumeration properties.
     * Unless this BACIValue object is hiding an enumeration, do not
     * use this.
     */
    CORBA::Any any_m;

    /**
     * This is set to true if the BACIValue is really an enumeration.
     */
    bool isEnum_m;

};



#include "baciValue.i"

 }; 

// ------------------[ Stream extraction ]-----------------------

std::istream& operator>>(std::istream &is, ACE_CString &data);

#endif /*! baciValue_H*/




