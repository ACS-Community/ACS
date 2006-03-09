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

import java.lang.reflect.Method;
import java.lang.reflect.Array;
import java.util.logging.Logger;

import org.omg.CORBA.Any;

import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;

import alma.ACS.doubleSeqHelper;
import alma.ACS.floatSeqHelper;
import alma.ACS.longSeqHelper;
import alma.ACS.stringSeqHelper;
import alma.ACSErrTypeJavaNative.wrappers.AcsJJavaAnyEx;

/**
 * Intended to be used as an aide to developers working with CORBA anys. If
 * there's some method you think should be added to this class to ease
 * developers' lives, please send this suggestion to the alma-sw-common@nrao.edu
 * or acs-discuss@nrao.edu mailing lists.
 * 
 * @author dfugate
 * @version $Id$
 */
class AnyAide {
   /**
    * Standard constructor.
    * 
    * @param cs
    *           Container services reference of the component.
    */
   public AnyAide(ContainerServices cs) {
      // save a local reference
      m_containerServices = cs;

      // just copy the reference
      m_logger = cs.getLogger();
   }

   /**
    * Converts an array of simple type instances to a CORBA any.
    * 
    * @param objs
    *           An array a CORBA simple types supported by BACI. For example,
    *           double[], string[], etc.
    * @return A CORBA any with the array of simple types.
    * @throws AcsJException
    *            Thrown when the array type is not supported.
    */
   public Any arrayToCorbaAny(Object objs) throws AcsJException {
      Any retVal = m_containerServices.getAdvancedContainerServices().getAny();

      // class object for the array
      Class cl = objs.getClass();
      // class object for the array elements
      Class objClass = cl.getComponentType();
      int length = Array.getLength(objs);

      // doubleSeq
      if (objClass.equals(double.class)) {
         double[] values = new double[length];
         System.arraycopy(objs, 0, values, 0, length);
         doubleSeqHelper.insert(retVal, values);
      }
      // longSeq
      else if (objClass.equals(int.class)) {
         int[] values = new int[length];
         System.arraycopy(objs, 0, values, 0, length);
         longSeqHelper.insert(retVal, values);
      }
      // stringSeq
      else if (objClass.equals(String.class)) {
         String[] values = new String[length];
         System.arraycopy(objs, 0, values, 0, length);
         stringSeqHelper.insert(retVal, values);
      }
      // floatSeq
      else if (objClass.equals(float.class)) {
         float[] values = new float[length];
         System.arraycopy(objs, 0, values, 0, length);
         floatSeqHelper.insert(retVal, values);
      }
      else {
         // if we do not know what it is, there's not much we can
         // do.
         throw new AcsJJavaAnyEx(cl.getName() + " not supported!");
      }
      return retVal;
   }

   /**
    * Converts a generic Java object to a CORBA. May fail.
    * 
    * @param obj
    *           Object to be converted to a CORBA any
    * @return A CORBA any with obj embedded within it.
    * @throws AcsJException
    *            Thrown if there's some problem converting the object to an any.
    *            TODO: make sure this works with enumerations.
    */
   public Any objectToCorbaAny(Object obj) throws AcsJException {
      if (obj.getClass().isArray() == true) {
         return arrayToCorbaAny(obj);
      }

      Any retVal = m_containerServices.getAdvancedContainerServices().getAny();

      // null case
      if (obj == null) {
         retVal.insert_Object(null);
      }
      // check against string
      else if (String.class.isInstance(obj) == true) {
         retVal.insert_string((String) obj);
      }
      // check against double
      else if (Double.class.isInstance(obj) == true) {
         double value = ((Double) obj).doubleValue();
         retVal.insert_double(value);
      }
      // check against long - CORBA long long and unsigned long long
      else if (Long.class.isInstance(obj) == true) {
         long value = ((Long) obj).longValue();
         retVal.insert_longlong(value);
      }
      // check against integer - CORBA long or unsigned long
      else if (Integer.class.isInstance(obj) == true) {
         int value = ((Integer) obj).intValue();
         retVal.insert_long(value);
      }
      // check against float
      else if (Float.class.isInstance(obj) == true) {
         float value = ((Float) obj).floatValue();
         retVal.insert_float(value);
      }
      else {
         // as a last ditch attempt, we assume the object
         // is some sort of complex IDL struct/union/etc
         // and that this method will work.
         return complexObjectToCorbaAny(obj);
      }
      return retVal;
   }

   /**
    * Converts a complex CORBA-based object to a CORBA any.
    * 
    * @param obj
    *           A complex CORBA-based object such as a user-defined IDL struct.
    * @return A CORBA any containing obj.
    * @throws AcsJException
    *            Thrown if any problem occurs with the conversion.
    */
   public Any complexObjectToCorbaAny(Object obj) throws AcsJException {
      Any retVal = m_containerServices.getAdvancedContainerServices().getAny();

      // ------
      Class structHelperClass = null;

      // first double-check that the Java Object they are attempting to
      // publish is actually a CORBA type.
      try {
         // This is the CORBA helper class which is capable of
         // inserting/extracting
         // data from CORBA Anys.
         structHelperClass = Class.forName(obj.getClass().getName() + "Helper");
      }
      catch (Exception e) {
         m_logger.warning(e.getMessage());
         // If what's above fails...then the developer has specified a native
         // Java
         // class which has nothing to do with CORBA.
         String msg = "The non-CORBA class '" + obj.getClass().getName()
               + "' cannot be converted to a CORBA any.";
         throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotFoundEx(msg);
      }

      try {
         // get at the static insert method defined for all IDL structures
         // and sequences.
         Method insert = structHelperClass.getMethod("insert", new Class[] {
               org.omg.CORBA.Any.class, obj.getClass() });

         // arguments to insert method are just the newly created Any and the
         // IDL struct instance passed to this method.
         Object[] args = { retVal, obj };
         insert.invoke(null, args);

         return retVal;
      }
      catch (java.lang.NoSuchMethodException e) {
         m_logger.warning(e.getMessage());
         String reason = "The '" + structHelperClass
               + "' object is incompatiable with CORBA: ";
         throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason
               + e.getMessage());
      }
      catch (java.lang.IllegalAccessException e) {
         m_logger.warning(e.getMessage());
         String reason = "The '" + structHelperClass
               + "' object is incompatiable with CORBA: ";
         throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason
               + e.getMessage());
      }
      catch (java.lang.reflect.InvocationTargetException e) {
         m_logger.warning(e.getMessage());
         String reason = "The '" + structHelperClass
               + "' object is incompatiable with CORBA: ";
         throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason
               + e.getMessage());
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
    *           A CORBA any containing some sort of CORBA object
    * @return the CORBA any converted into the corresponding Java type.
    */
   public Object corbaAnyToObject(Any any) {
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
      switch (anyKind.value())
      {
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
         }
         else if (anyType.compareTo("alma::ACS::doubleSeq") == 0) {
            returnValue = doubleSeqHelper.extract(any);
         }
         // IDL://alma:ACS:longSeq:1.0
         else if (anyType.compareTo("alma::ACS::longSeq") == 0) {
            returnValue = alma.ACS.longSeqHelper.extract(any);
         }
         // IDL://alma:ACS:strSeq:1.0
         else if (anyType.compareTo("alma::ACS::stringSeq") == 0) {
            returnValue = alma.ACS.stringSeqHelper.extract(any);
         }
         // IDL://alma:ACS:floatSeq:1.0
         else if (anyType.compareTo("alma::ACS::floatSeq") == 0) {
            returnValue = alma.ACS.floatSeqHelper.extract(any);
         }
         else {
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
         if (anyType.compareTo("alma::ACS::doubleSeq") == 0) {
            returnValue = doubleSeqHelper.extract(any);
         }
         // IDL://alma:ACS:longSeq:1.0
         else if (anyType.compareTo("alma::ACS::longSeq") == 0) {
            returnValue = alma.ACS.longSeqHelper.extract(any);
         }
         // IDL://alma:ACS:strSeq:1.0
         else if (anyType.compareTo("alma::ACS::stringSeq") == 0) {
            returnValue = alma.ACS.stringSeqHelper.extract(any);
         }
         // IDL://alma:ACS:floatSeq:1.0
         else if (anyType.compareTo("alma::ACS::floatSeq") == 0) {
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
            // Need access to this to convert an Any to the Java language type.
            Method extract = localHelper.getMethod("extract",
                  new Class[] { org.omg.CORBA.Any.class });
            Object[] args = { any };
            returnValue = extract.invoke(null, args);
         }
         catch (Exception ex) {
            m_logger.severe("Failed to extract enum!");
            ex.printStackTrace();
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
    * Method used to convert an any with a complex user-defined struct embedded
    * within it to it's corresponding Java type.
    * 
    * @param any
    *           CORBA Any containing a complex, user-defined object within it
    * @return the CORBA Any parameter converted to an object of the
    *         corresponding Java type.
    */
   public Object complexAnyToObject(org.omg.CORBA.Any any) {
      // initialize the return value
      Object retValue = null;

      try {
         // Create the IDL struct helper class
         // With Java Anys, we can extract the name of the underlying object
         // instance and
         // from that all that needs to be done is to concatenate "Helper" to
         // get.

         String localHelperName = any.type() + "Helper";
         localHelperName = localHelperName.replaceAll("::", ".");
         Class localHelper = Class.forName(localHelperName);

         // Extract method of helper class
         // Need access to this to convert an Any to the Java language type.
         Method extract = localHelper.getMethod("extract",
               new Class[] { org.omg.CORBA.Any.class });
         Object[] args = { any };

         retValue = extract.invoke(null, args);
      }
      catch (java.lang.ClassNotFoundException e) {
         // should never happen...
         String msg = "Failed to process an any because the helper class does not exist: ";
         msg = msg + e.getMessage();
         m_logger.warning(msg);
      }
      catch (java.lang.NoSuchMethodException e) {
         // should never happen...
         String msg = "Failed to process an any because the helper class does not provide";
         msg = msg + " the 'extract' method: ";
         msg = msg + e.getMessage();
         m_logger.warning(msg);
      }
      catch (java.lang.IllegalAccessException e) {
         // should never happen...
         String msg = "Failed to process an any because: ";
         msg = msg + e.getMessage();
         m_logger.warning(msg);
      }
      catch (java.lang.reflect.InvocationTargetException e) {
         // should never happen...
         String msg = "Failed to process an any because: ";
         msg = msg + e.getMessage();
         m_logger.warning(msg);
      }

      return retValue;
   }

   /** reference to the container services */
   private ContainerServices m_containerServices = null;

   /** our own logger */
   private Logger            m_logger            = null;

}
