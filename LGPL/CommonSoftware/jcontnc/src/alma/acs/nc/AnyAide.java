/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2004
 * Copyright by ESO (in the framework of the ALMA collaboration), All rights
 * reserved
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

/**
 * 
 */
package alma.acs.nc;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.Any;
import org.omg.CORBA.portable.IDLEntity;

import alma.ACS.doubleSeqHelper;
import alma.ACS.floatSeqHelper;
import alma.ACS.longSeqHelper;
import alma.ACS.stringSeqHelper;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.ACSErrTypeJavaNative.wrappers.AcsJJavaAnyEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;

/**
 * Intended to be used as an aide to developers working with CORBA anys. If
 * there's some method you think should be added to this class to ease
 * developers' lives, please send this suggestion to the alma-sw-common@nrao.edu
 * or acs-discuss@nrao.edu mailing lists.
 * 
 * @TODO make package-private once we no longer have a separate "refactored" subpackage.
 * 
 * @author dfugate
 * @version $Id$
 */
public class AnyAide {

	/** reference to the container services */
	private final ContainerServicesBase m_containerServices;

	/** our own logger */
	private final Logger m_logger;

	/**
	 * Standard constructor.
	 * 
	 * @param cs
	 *           Container services reference of the component.
	 */
	public AnyAide(ContainerServicesBase cs) {
		// save a local reference
		m_containerServices = cs;

		// just copy the reference
		m_logger = cs.getLogger();
	}

	/**
	 * Moved here from method arrayToCorbaAny (deleted in ACS 9.0).
	 */
	protected Any internalArrayToCorbaAny(Object objs) throws AcsJException {
		// class object for the array
		Class cl = objs.getClass();
		if (!cl.isArray()) {
			Throwable cause = new Throwable("Object of type " + cl.getName() + " is not an array.");
			throw new AcsJJavaAnyEx(cause);
		}
		// class object for the array elements
		Class objClass = cl.getComponentType();
		int length = Array.getLength(objs);

		// doubleSeq
		if (objClass.equals(double.class)) {
			double[] values = new double[length];
			System.arraycopy(objs, 0, values, 0, length);
			return doubleArrayToCorbaAny(values);
		}
		// longSeq
		else if (objClass.equals(int.class)) {
			int[] values = new int[length];
			System.arraycopy(objs, 0, values, 0, length);
			return intArrayToCorbaAny(values);
		}
		// stringSeq
		else if (objClass.equals(String.class)) {
			String[] values = new String[length];
			System.arraycopy(objs, 0, values, 0, length);
			return stringArrayToCorbaAny(values);
		}
		// floatSeq
		else if (objClass.equals(float.class)) {
			float[] values = new float[length];
			System.arraycopy(objs, 0, values, 0, length);
			return floatArrayToCorbaAny(values);
		} 
		else {
			// if we do not know what it is, there's not much we can
			// do.
			Throwable cause = new Throwable(cl.getName() + " not supported!");
			throw new AcsJJavaAnyEx(cause);
		}
	}

	public Any doubleArrayToCorbaAny(double[] doubles) {
		Any retVal = m_containerServices.getAdvancedContainerServices().getAny();
		doubleSeqHelper.insert(retVal, doubles);
		return retVal;
	}

	public Any floatArrayToCorbaAny(float[] floats) {
		Any retVal = m_containerServices.getAdvancedContainerServices().getAny();
		floatSeqHelper.insert(retVal, floats);
		return retVal;
	}

	public Any intArrayToCorbaAny(int[] ints) {
		Any retVal = m_containerServices.getAdvancedContainerServices().getAny();
		longSeqHelper.insert(retVal, ints);
		return retVal;
	}

	public Any stringArrayToCorbaAny(String[] strings) {
		Any retVal = m_containerServices.getAdvancedContainerServices().getAny();
		stringSeqHelper.insert(retVal, strings);
		return retVal;
	}

	
	/**
	 * Converts a generic Java object to a CORBA Any. May fail.
	 * 
	 * @param obj
	 *            Object to be converted to a CORBA any
	 * @return A CORBA any with obj's data embedded within it.
	 * @throws AcsJException
	 *             Thrown if there's some problem converting the object to an
	 *             any. TODO: make sure this works with enumerations.
	 */
	public Any objectToCorbaAny(Object obj) throws AcsJException {

		if (obj != null && obj.getClass().isArray()) {
			return internalArrayToCorbaAny(obj);
		}

		Any retVal = m_containerServices.getAdvancedContainerServices().getAny();

		// null case
		if (obj == null) {
			retVal.insert_Object(null);
		}
		// check against string
		else if (obj instanceof String) {
			retVal.insert_string((String) obj);
		}
		// check against double
		else if (obj instanceof Double) {
			double value = ((Double) obj).doubleValue();
			retVal.insert_double(value);
		}
		// check against long - CORBA long long and unsigned long long
		else if (obj instanceof Long) {
			long value = ((Long) obj).longValue();
			retVal.insert_longlong(value);
		}
		// check against integer - CORBA long or unsigned long
		else if (obj instanceof Integer) {
			int value = ((Integer) obj).intValue();
			retVal.insert_long(value);
		}
		// check against float
		else if (obj instanceof Float) {
			float value = ((Float) obj).floatValue();
			retVal.insert_float(value);
		} 
		else if (obj instanceof IDLEntity) {
			// as a last ditch attempt, we assume the object
			// is some sort of complex IDL struct/union/etc
			// and that this method will work.
			return complexObjectToCorbaAny((IDLEntity) obj);
		} 
		else {
			Throwable cause = new Throwable("Bad arg of type " + obj.getClass().getName());
			throw new AcsJBadParameterEx(cause);
		}
		return retVal;
	}

	/**
	 * Converts a complex CORBA-based object to a CORBA any.
	 * 
	 * @param obj
	 *            A complex CORBA-based object such as a user-defined IDL struct.
	 * @return A CORBA any containing obj.
	 * @throws AcsJException
	 *             if any problem occurs with the conversion.
	 */
	public Any complexObjectToCorbaAny(IDLEntity obj) throws AcsJException {

		if (obj == null) {
			Throwable cause = new Throwable("Method arg 'obj' was null");
			throw new AcsJBadParameterEx(cause);
		}

		Any retVal = m_containerServices.getAdvancedContainerServices().getAny();

		// ------
		Class structHelperClass = null;

		// first double-check that the Java Object they are attempting to
		// actually looks like a CORBA type.
		try {
			// This is the CORBA helper class which is capable of inserting/extracting data from CORBA Anys.
			structHelperClass = Class.forName(obj.getClass().getName() + "Helper");
		} 
		catch (Exception e) {
			// If what's above fails...then the developer has specified a native Java
			// class which has nothing to do with CORBA.
			String msg = "The non-CORBA class '" + obj.getClass().getName()
					+ "' cannot be converted to a CORBA Any.";
			Throwable cause = new Throwable(msg);
			m_logger.warning(msg);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotFoundEx(cause);
		}

		try {
			// get at the static insert method defined for all IDL structures
			// and sequences.
			Method insert = structHelperClass.getMethod("insert", new Class[] { Any.class, obj.getClass() });

			// arguments to insert method are just the newly created Any and the
			// IDL struct instance passed to this method.
			Object[] args = { retVal, obj };
			insert.invoke(null, args);

			return retVal;
		} 
		catch (NoSuchMethodException e) {
			// we got a Helper class, but it seems to be not the CORBA-generated kind
			Throwable cause = new Throwable("Class '" + structHelperClass.getName()
					+ "' associated with the given object of type '" + obj.getClass().getName()
					+ "' is incompatiable with CORBA: " +  e.getMessage());
			throw new AcsJBadParameterEx(cause);
		} 
		catch (java.lang.reflect.InvocationTargetException e) {
			Throwable realEx = e.getCause();
			String reason = "Failed to insert the given CORBA object into a CORBA Any: the helper class insert method threw an exception.";
			m_logger.log(Level.FINE, reason, realEx);
			Throwable cause = new Throwable(reason + realEx.getMessage());
			throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(cause); // todo: NC-specific exception type
		}
		catch (Throwable thr) {
			String reason = "Failed to insert the given CORBA object into a CORBA Any.";
			m_logger.log(Level.FINE, reason, thr);
			Throwable cause = new Throwable(reason + thr.getMessage());
			throw new AcsJUnexpectedExceptionEx(cause);
		}
	}

	/**
	 * Method which attempts to (and under normal circumstances should succeed)
	 * convert a CORBA any object to the corresponding Java object. For simple
	 * CORBA types such as long, this method will extract the long and embed it
	 * within a java.lang.Long object. In the event of failure, a null object is
	 * returned.
	 * 
	 * @param any
	 *            A CORBA any containing some sort of CORBA object
	 * @return the CORBA any converted into the corresponding Java type, or <code>null</code> if it failed.
	 */
	public Object corbaAnyToObject(Any any) {

		// @TODO check any==null

		// initialize the return value
		Object returnValue = null;

		// get the CORBA typecode enum.
		// we need this to deal with the simple types
		org.omg.CORBA.TCKind anyKind = any.type().kind();
		String anyType = any.type().toString();

		// within this switch block, returnValue is set
		// to be some real (native) Java object rather
		// than a CORBA any type. at this time, ACS only
		// support "BACI Value types" defined within
		// $ACSROOT/include/baciValue.h (the "Type" enum).
		switch (anyKind.value()) {
		case org.omg.CORBA.TCKind._tk_null:
			// this case is quite simple. A null
			// CORBA reference is null in Java as
			// well
			returnValue = null;
			break;

		case org.omg.CORBA.TCKind._tk_string:
			// simple type in which we have an
			// extract method
			returnValue = any.extract_string();
			break;

		case org.omg.CORBA.TCKind._tk_double:
			// simple type in which we have an
			// extract method
			returnValue = new Double(any.extract_double());
			break;

		case org.omg.CORBA.TCKind._tk_long:
			// simple type in which we have an
			// extract method
			returnValue = new Integer(any.extract_long());
			break;

		case org.omg.CORBA.TCKind._tk_alias:
			if (anyType.compareTo("alma::ACS::Pattern") == 0) {
				// simple type in which we have an
				// extract method
				returnValue = new Integer(any.extract_ulong());
			} else if (anyType.compareTo("sequence <double>") == 0 || anyType.compareTo("alma::ACS::doubleSeq") == 0) {
				returnValue = doubleSeqHelper.extract(any);
			}
			// IDL://alma:ACS:longSeq:1.0
			else if (anyType.compareTo("sequence <long>") == 0 || anyType.compareTo("alma::ACS::longSeq") == 0) {
				returnValue = alma.ACS.longSeqHelper.extract(any);
			}
			// IDL://alma:ACS:strSeq:1.0
			else if (anyType.compareTo("sequence <string>") == 0 || anyType.compareTo("alma::ACS::stringSeq") == 0) {
				returnValue = alma.ACS.stringSeqHelper.extract(any);
			}
			// IDL://alma:ACS:floatSeq:1.0
			else if (anyType.compareTo("sequence <float>") == 0 || anyType.compareTo("alma::ACS::floatSeq") == 0) {
				returnValue = alma.ACS.floatSeqHelper.extract(any);
			} else {
				m_logger.severe("Alias unexpected:" + anyType);
			}
			break;

		case org.omg.CORBA.TCKind._tk_ulong:
			// simple type in which we have an
			// extract method

			returnValue = new Integer(any.extract_ulong());
			break;

		// handle all sequences here
		case org.omg.CORBA.TCKind._tk_sequence:
			// IDL://alma:ACS:doubleSeq:1.0
			if (anyType.compareTo("sequence <double>") == 0) {
				returnValue = doubleSeqHelper.extract(any);
			}
			// IDL://alma:ACS:longSeq:1.0
			else if (anyType.compareTo("sequence <long>") == 0) {
				returnValue = alma.ACS.longSeqHelper.extract(any);
			}
			// IDL://alma:ACS:strSeq:1.0
			else if (anyType.compareTo("sequence <string>") == 0) {
				returnValue = alma.ACS.stringSeqHelper.extract(any);
			}
			// IDL://alma:ACS:floatSeq:1.0
			else if (anyType.compareTo("sequence <float>") == 0) {
				returnValue = alma.ACS.floatSeqHelper.extract(any);
			}
			// pretty severe situation if we cannot find
			// the sequence type using normal BACI value types
			else {
				m_logger.severe("Sequence unexpected:" + anyType);
			}
			break;

		case org.omg.CORBA.TCKind._tk_longlong:
			// simple type in which we have an
			// extract method
			returnValue = new Long(any.extract_longlong());
			break;

		case org.omg.CORBA.TCKind._tk_ulonglong:
			// simple type in which we have an
			// extract method
			returnValue = new Long(any.extract_ulonglong());
			break;

		case org.omg.CORBA.TCKind._tk_float:
			// simple type in which we have an
			// extract method
			returnValue = new Float(any.extract_float());
			break;

		case org.omg.CORBA.TCKind._tk_enum:
			// very special case. at the moment,
			// we just support enumerations defined within
			// the uppermost IDL module
			try {
				String localHelperName = anyType + "Helper";
				localHelperName = localHelperName.replaceAll("::", ".");
				Class localHelper = Class.forName(localHelperName);

				// Extract method of helper class
				// Need access to this to convert an Any to the Java language
				// type.
				Method extract = localHelper.getMethod("extract", new Class[] { Any.class });
				Object[] args = { any };
				returnValue = extract.invoke(null, args);
			} catch (Exception ex) {
				m_logger.log(Level.SEVERE, "Failed to extract enum!", ex);
			}
			break;

		// pretty bad if we get this far!
		default:
			m_logger.severe("Could not extract the any:" + anyType);
			break;
		}

		return returnValue;
	}

	/**
	 * Extracts from a Corba Any the embedded user-defined event data.
	 * The returned data can be either
	 * <ol>
	 * <li>a class implementing <code>IDLEntity</code> if an IDL-defined struct was sent, or
	 * <li>an array of IDL-defined structs
	 * </ol>
	 * Other non-IDL defined classes or primitive types are not allowed as event data 
	 * (not totally sure but it seems like that, HSO 2006-12).
	 * 
	 * @param any CORBA Any containing a complex, user-defined object within it
	 * @return the CORBA Any parameter converted to an object of the
	 *         corresponding Java type, or <code>null</code> if the conversion failed.
	 */
	public Object complexAnyToObject(Any any) 
	{
		// initialize the return value
		Object retValue = null;
		Class localHelper = null;
		// Create the IDL struct helper class
		// With Java Anys, we can extract the name of the underlying object
		// instance and from that all that needs to be done is to concatenate "Helper" 
		// to get.
		String qualHelperClassName = null;
		try {
			org.omg.CORBA.TCKind kind = any.type().kind();
			if (kind.equals(org.omg.CORBA.TCKind.tk_sequence)) {
				// the event data is a sequence instead of a single value or struct. Need to get the underlying type
				org.omg.CORBA.TypeCode sequenceType = any.type().content_type();

				// @TODO check if the following applies also for sequences of primitive types, 
				//       or if there is a rule that we always must have structs as event data 
				//       (which is implied by always calling complexAnyToObject in push_structured_event) 

				// Derive the Java package from the id. 
				// Note that we can't use the TypeCode#toString() method which gives a nice qualified name for JacORB, but perhaps not for other ORB impls.
				// First assume that the type is not defined nested inside an interface 
				qualHelperClassName = corbaStructToJavaClass(sequenceType.id(), false) + "SeqHelper";
				try {
					localHelper = Class.forName(qualHelperClassName);
				} catch (ClassNotFoundException ex) {
					// it could be that we are dealing with a sequence of nested structs
					qualHelperClassName = corbaStructToJavaClass(sequenceType.id(), true) + "SeqHelper";
					localHelper = Class.forName(qualHelperClassName);
				}
			} 
			else {
				// @TODO: check the effect of nested event structs (defined inside an interface), whether "Package" must be inserted.
				// If necessary, also call method corbaStructToJavaClass to find the correct class name
				qualHelperClassName = any.type() + "Helper";
				qualHelperClassName = qualHelperClassName.replaceAll("::", ".");
				localHelper = Class.forName(qualHelperClassName);
			}

			// Extract method of helper class
			// Need access to this to convert an Any to the Java language type.
			Method extract = localHelper.getMethod("extract", new Class[] { Any.class });
			Object[] args = { any };

			retValue = extract.invoke(null, args);
		} 
		catch (ClassNotFoundException e) {
			// should never happen...
			String msg = "Failed to extract the event struct data from a CORBA Any because the helper class '"
					+ qualHelperClassName + "' does not exist.";
			m_logger.log(Level.WARNING, msg, e);
		} 
		catch (NoSuchMethodException e) {
			// should never happen...
			String msg = "Failed to process an any because the helper class '" + qualHelperClassName + "' does not provide the 'extract' method.";
			m_logger.log(Level.WARNING, msg, e);
		}
		//		catch (ClassCastException e) {
		//			// should never happen...
		//			String msg = "Failed to process an any because the contained data does not seem to come from an IDL struct.";
		//			m_logger.log(Level.WARNING, msg, e);
		//		} 
		catch (Throwable thr) { // IllegalAccessException, InvocationTargetException, TypeCodePackage.BadKind or any other throwable
			// should never happen...
			String msg = "Failed to process an any because of unexpected problem.";
			m_logger.log(Level.WARNING, msg, thr);
		}
		return retValue;
	}

	/**
	 * Derives the qualified Java class name for an IDL-defined struct from the Corba ID of that struct.
	 * 
	 * @param isNestedStruct  if true, "Package" will be inserted according to 
	 *                        <i>"IDL to Java LanguageMapping Specification" version 1.2: 1.17 Mapping for Certain Nested Types</i> apply.
	 */
	protected String corbaStructToJavaClass(String id, boolean isNestedStruct)
			throws IllegalArgumentException 
	{
		String prefix = "IDL:";
		String suffix = ":1.0";
		if (!id.startsWith(prefix) || !id.endsWith(suffix)) {
			throw new IllegalArgumentException("Struct ID is expected to start with 'IDL:' and end with ':1.0'");
		}
		String qualNameWithSlashes = id.substring(prefix.length(), id.length() - suffix.length());
		String qualName = qualNameWithSlashes.replace('/', '.');
		if (isNestedStruct) {
			int lastDotIndex = qualName.lastIndexOf('.');
			if (lastDotIndex > 0) {
				String className = qualName.substring(lastDotIndex + 1);
				String jPackage = qualName.substring(0, lastDotIndex);
				jPackage += "Package."; // defined in IDL-Java mapping spec
				qualName = jPackage + className;
			}
		}
		return qualName;
	}

}
