/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.exceptions;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;

import org.omg.CORBA.UserException;

import alma.ACSErr.ErrorTrace;
import alma.acs.util.UTCUtility;

/**
 * @author hsommer
 * created Sep 26, 2003 10:53:40 AM
 */
public class CorbaExceptionConverter
{
	public static final String PROPERTY_JAVAEXCEPTION_CLASS = "javaex.class";
	public static final String PROPERTY_JAVAEXCEPTION_MESSAGE = "javaex.msg";

	/**
	 * Converts an <code>ErrorTrace</code> object to a Java <code>Throwable</code>.
	 * The chain of caused-by exceptions is translated, too.
	 * <p>
	 * See the comment on class substitution at
	 * {@link DefaultAcsJException}.
	 * 
	 * @param et
	 * @return Throwable  current implementation always returns a subclass of 
	 * 					<code>AcsJException</code>, but don't rely on it yet.
	 */
	static Throwable recursiveGetThrowable(ErrorTrace et)
	{
		// check if underlying Java exception class is stored as a property
		String classname = ErrorTraceManipulator.getProperty(et, PROPERTY_JAVAEXCEPTION_CLASS);
				
		String message = ErrorTraceManipulator.getProperty(et, PROPERTY_JAVAEXCEPTION_MESSAGE);    
        if (message == null) {
            message = "";
        }
        
		Throwable thr = null;
		
        // TODO: define some fields in ErrorTrace that hold the IDL information
        // about the exception name (module, name).
        // Then (based on naming conventions) we can attempt to construct 
        // the right AcsJ-style exception from the error trace even if 
        // the ErrorTrace was originally not produced in Java.
        // An alternative solution would be a map that goes from (type, code) 
        // to the matching exception class. This seems difficult to maintain across modules though.
		if (classname != null)
		{
			// try if that class is available to be reconstructed
			try
			{
				Class<? extends Throwable> exClass = Class.forName(classname).asSubclass(Throwable.class);
				if (AcsJException.class.isAssignableFrom(exClass)) 
				{
					if (exClass != DefaultAcsJException.class) { // DefaultAcsJException has a non-standard constructor and will be constructed below.
						Constructor<? extends Throwable> msgCtor = exClass.getConstructor(String.class);
						thr = msgCtor.newInstance(new Object[]{message});
					}
				}
				else {
					// non-ACS Java exceptions we don't reconstruct directly
					// because we'd lose the additional information like line number etc. that ErrorTrace has
					// and that will be copied below.
					thr = new DefaultAcsJException(message, 0, 0, et.shortDescription, classname);
				}
			}
			catch (Exception e)
			{
				message = "failed to reconstruct Java exception '" + classname + "'. Original msg was '" + message + "'.";
				// just leave thr == null
			}
		}
		
		if (thr == null)
		{
			// default ex represents ErrorTrace that comes from other languages than Java,
			// or when reconstruction of the same Java exception failed for whatever reason. 
			thr = new DefaultAcsJException(message, et.errorType, et.errorCode, et.shortDescription);
		}
		
		resurrectThrowable(thr, et);
		
		return thr;
	}




	private static void resurrectThrowable(Throwable thr, ErrorTrace et)
	{
		if (thr == null || et == null)
		{
			throw new NullPointerException("resurrectThrowable: parameters must not be null!"); 
		}

		if (thr instanceof AcsJException)
		{
			// data specific to AcsJException
			AcsJException acsJEx = (AcsJException) thr;
			
			acsJEx.m_host = et.host;
			acsJEx.m_process = et.process;

			acsJEx.m_file = et.file;
			acsJEx.m_method = et.routine;
			acsJEx.m_line = et.lineNum;
			
			// todo check if error type and code match
			//et.errorType == acsJEx.getErrorType();
			//et.errorCode == acsJEx.getErrorCode();
			
			acsJEx.m_severity = et.severity; 
			acsJEx.m_threadName = et.thread;
			acsJEx.m_timeMilli = UTCUtility.utcOmgToJava(et.timeStamp);
			acsJEx.m_properties = ErrorTraceManipulator.getProperties(et);
		}

		// We redundantly put source information also into a fake StackTrace,
		// which then looks better when logging such an AcsJException directly,
		// even though the rest of a single java exception's stack trace did not get 
		// forwarded over corba/ErrorTrace.
		StackTraceElement[] stackTrace = new StackTraceElement[1];
		stackTrace[0] = new StackTraceElement("---", et.routine, et.file, et.lineNum);
		thr.setStackTrace(stackTrace);

		if (et.previousError != null && et.previousError.length > 0 && 
			et.previousError[0] != null)
		{
			ErrorTrace etCause = et.previousError[0];
			if (etCause != null)
			{
				// recursion
				Throwable thrCause = recursiveGetThrowable(etCause);
				thr.initCause(thrCause);
			}
		}
	}


	public static void convertErrorTraceToJavaException(ErrorTrace et, AcsJException jEx)
	{
		resurrectThrowable(jEx, et);
	}
		
	
	/**
	 * Checks if a given Throwable is of a CORBA type generated by the ACS error system.
	 * If so, the embedded error trace is converted to a chain of AcsJ-style exceptions.
	 * <p>
	 * The check is based on the fact that all ACS error system exceptions derive 
	 * from <code>org.omg.CORBA.UserException</code>, and have a field <code>errorTrace</code>
	 * of type <code>alma.ACSErr.ErrorTrace</code>.
	 * 
	 * @param thr
	 * @return The converted Throwable, or <code>thr</code> itself if no conversion is necessary,
	 *         or if the conversion failed with an exception.
	 */
	public static Throwable convertHiddenErrorTrace(Throwable thr)
	{
        // thr may be null when a Completion is constructed from another completion 
        // that does not have an associated error trace 
        if (thr == null) {
            return null;
        }
		// all ACS error system exceptions derive from org.omg.CORBA.UserException 
		if (UserException.class.isAssignableFrom(thr.getClass())) {
			// check if thr is of an exception class generated by the ACS error system, then use its ErrorTrace
			try {
				Field errorTraceField = thr.getClass().getField("errorTrace");
				ErrorTrace etCause = (ErrorTrace) errorTraceField.get(thr);
				thr = recursiveGetThrowable(etCause);
			}
			catch (Exception ex) {
				// ignore: we assume that thr is not an ACS-style exception
			} 
		}
		return thr;
	}
	
}
