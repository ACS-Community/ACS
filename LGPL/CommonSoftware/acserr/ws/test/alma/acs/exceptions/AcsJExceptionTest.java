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

import java.util.Map;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import alma.ACSErr.ACSErrTypeTest;
import alma.ACSErr.Completion;
import alma.ACSErr.ErrorTrace;
import alma.ACSErr.Severity;
import alma.ACSErrTypeTest.wrappers.ACSErrTestOKAcsJCompletion;
import alma.ACSErrTypeTest.wrappers.AcsJACSErrTest0Ex;
import alma.ACSErrTypeTest.ACSErrTest0Ex;
import alma.ACSErrTypeTest.ACSErrTestOK;
import alma.acs.testsupport.TestLogger;
import alma.acs.util.UTCUtility;

import junit.framework.TestCase;

/**
 * Tests for {@link AcsJException} and example subclasses 
 * 
 * @author hsommer Jun 18, 2003 3:54:40 PM
 */
public class AcsJExceptionTest extends TestCase
{
	public AcsJExceptionTest()
	{
		super("AcsJExceptionTest");
	}

	
	/**
	 * Uses {@link #throwOriginalAcsJACSErrTest0Ex} to verify the output 
	 * of the method {@link AcsJException#getErrorTrace}.
	 * Thus what gets tested is the conversion from the AcsJ- to the CORBA world.
	 * <P>
	 * Fields which are more difficult to get out from an ErrorTrace object will 
	 * be checked in subsequent tests.
	 */
	public void testGetErrorTrace()
	{
		try
		{
			throwOriginalAcsJACSErrTest0Ex();
		}
		catch (AcsJException e)
		{
			ErrorTrace et = e.getErrorTrace();
			assertNotNull("errorTrace obj not null", et);
			
			ErrorTrace[] prev = et.previousError;
			assertNotNull("chained child ex", prev);
			assertEquals("1 chained child ex", 1, prev.length);
			assertEquals("no grandchild ex", 0, prev[0].previousError.length);
            
            // check data of ErrorTrace that was constructed from a non-ACS exception
            assertEquals("time stamp for non-ACS exceptions should be defaulted to be 1 millisec less than the parent's time stamp", 
                    et.timeStamp-10000, prev[0].timeStamp);
		}
	}
	
	
	/**
	 * Tests the conversion from {@link AcsJACSErrTest0Ex}
	 * to {@link ErrorTrace} and back, first by wrapping and later by directly
	 * converting a CORBA exception with/to a native Java exception.
	 * <p>
	 * Calls {@link #throwWrapperAcsJACSErrTest0Ex} and 
	 * {@link #throwConvertedAcsJACSErrTest0Ex} to get the 
	 * three linked exceptions
	 * <ul>
	 * <li> <code>AcsJACSErrTest0Ex</code> thrown locally ("client side"),
	 * 			only in case of <code>throwWrapperAcsJACSErrTest0Ex</code>
	 * <li> <code>AcsJException</code> caused by a 
	 * 		<code>NullPointerException</code>, both thrown "remotely"
	 * 		in the implementation of the "server side" method 
	 * 		{@link #throwACSErrTest0Ex}
	 * 		and translated back and forth from an <code>ErrorTrace</code>
	 * </ul>
	 * Checks that the exceptions are correctly chained up
	 * and the data is correct.
	 */
	public void testErrorTraceToAcsJExConversion()
	{
		// first the variant with a new exception created on the client side
		try
		{
			throwWrapperAcsJACSErrTest0Ex();
		}
		catch (AcsJException e)
		{
			assertTrue(e instanceof AcsJACSErrTest0Ex);
			assertEquals("remote call failed", e.getMessage());
			assertEquals("AcsJExceptionTest.java", e.getFile());
			assertEquals(3, e.getTraceDepth());
			String host = e.getHost();
			assertNotNull(host);
			assertTrue(host.trim().length() > 0);
			String threadName = e.getThreadName();
			assertNotNull(threadName);
			assertTrue(threadName.trim().length() > 0);
			assertTrue(e.getLine() > 0);
			assertNotNull(e.getMethod());
			assertTrue(e.getMethod().trim().length() > 0);
			assertEquals(Severity.Error, e.getSeverity());

			Throwable cause1 = e.getCause();
			assertNotNull(cause1);
			assertTrue(cause1 instanceof AcsJACSErrTest0Ex && cause1 != e);
			assertEquals("low level ex", cause1.getMessage());
			AcsJACSErrTest0Ex acsJCause1 = (AcsJACSErrTest0Ex) cause1;
			assertEquals("AcsJExceptionTest.java", acsJCause1.getFile());
			assertEquals("Poverty", acsJCause1.getProperty("MyStupidProperty"));
			assertEquals(2, acsJCause1.getTraceDepth());
			long timeCause1 = acsJCause1.getTimestampMillis();
			assertEquals("AcsJExceptionTest.java", acsJCause1.getFile());
			assertEquals(host, acsJCause1.getHost());
			assertEquals(threadName, acsJCause1.getThreadName());
			assertTrue(acsJCause1.getLine() > 0);
			assertNotNull(acsJCause1.getMethod());
			assertTrue(acsJCause1.getMethod().trim().length() > 0);			
			assertEquals(Severity.Error, acsJCause1.getSeverity());
			
			Throwable cause2 = cause1.getCause();
			assertNotNull(cause2);
			assertTrue(cause2 instanceof DefaultAcsJException);
			assertEquals("original type java.lang.NullPointerException: mean NPE", cause2.getMessage());
			DefaultAcsJException acsJCause2 = (DefaultAcsJException) cause2;
			assertEquals("AcsJExceptionTest.java", acsJCause2.getFile());
			assertEquals(timeCause1 - 1, acsJCause2.getTimestampMillis());
			assertEquals(1, acsJCause2.getTraceDepth());
			assertEquals(host, acsJCause2.getHost());
			assertEquals("NA", acsJCause2.getThreadName()); // NPE did not carry thread name info
			assertTrue(acsJCause2.getLine() > 0);
			assertNotNull(acsJCause2.getMethod());
			assertTrue(acsJCause2.getMethod().trim().length() > 0);			
			assertEquals(Severity.Error, acsJCause2.getSeverity());
		}
		
		// next we try the client-side conversion instead of wrapping
		try
		{
			throwConvertedAcsJACSErrTest0Ex();
		}
		catch (AcsJException e)
		{
			assertTrue(e instanceof AcsJACSErrTest0Ex);
			assertEquals("low level ex", e.getMessage());
			assertEquals(2, e.getTraceDepth());
			assertEquals("Poverty", e.getProperty("MyStupidProperty"));

			Throwable cause1 = e.getCause();
			assertNotNull(cause1);
			assertTrue(cause1 instanceof DefaultAcsJException);
			assertEquals("original type java.lang.NullPointerException: mean NPE", cause1.getMessage());
			DefaultAcsJException acsJCause1 = (DefaultAcsJException) cause1;
			assertEquals(1, acsJCause1.getTraceDepth());
		}
		
	}
	
	
	public void testJCompletion()
	{
		// a completion without associated error
		AcsJCompletion jcompl1 = new ACSErrTestOKAcsJCompletion();
		assertEquals(ACSErrTypeTest.value, jcompl1.getType());
		assertEquals(ACSErrTestOK.value, jcompl1.getCode());
		assertFalse(jcompl1.isError());
		assertTrue(jcompl1.getTimeStamp() > 0);
		Completion corbacompl = jcompl1.toCorbaCompletion();
		assertTrue(corbacompl.previousError.length == 0);
		assertEquals(UTCUtility.utcJavaToOmg(jcompl1.getTimeStamp()), corbacompl.timeStamp);
		
		// completion with associated error
		// TODO: here we get an NPE because jcompl1 has no exception!
		AcsJCompletion jcompl2a = new ExampleWithErrAcsJCompletion(jcompl1);
		assertTrue(jcompl2a.isError());
		AcsJException acsjex2a = jcompl2a.getAcsJException();
		assertTrue(acsjex2a instanceof AcsJACSErrTest0Ex);
		assertNull(acsjex2a.getCause());
		
		AcsJCompletion jcompl2b = new ExampleWithErrAcsJCompletion(corbacompl);
		assertTrue(jcompl2b.isError());
		
		AcsJCompletion jcompl3 = new ExampleWithErrAcsJCompletion(jcompl2b);
	}
	
    
	/**
	 * Checks if logging of ACS exceptions (including caused-by exceptions) works.
     * See also <code>alma.demo.client.XmlComponentClient#testException()</code> in module jcontexmpl.
	 */
	public void testLogAcsJException() {
        Logger logger = TestLogger.getLogger("AcsJExceptionTest#testLogAcsJException");

        try {
            throwOriginalAcsJACSErrTest0Ex();
        } catch (AcsJException e) {
            assertEquals(2, e.getTraceDepth());
            logger.info("Will log exception coming from throwOriginalAcsJACSErrTest0Ex:");
            e.log(logger);
        }        

        
        try {
            throwWrapperAcsJACSErrTest0Ex();
        } catch (AcsJException e) {
            assertEquals(3, e.getTraceDepth());
            logger.info("Will log exception coming from throwWrapperAcsJACSErrTest0Ex:");
            e.log(logger);
            
            // now check some values of a log record (which only shop up in the log when AcsXMLLogFormatter is used, e.g. in the Java container)
            long defaultTimeStamp = e.getTimestampMillis();
            LogRecord logRec = e.createSingleErrorTraceLogRecord(AcsJException.createSingleErrorTrace(e, defaultTimeStamp), "myTestStackID", 123);
            assertEquals("throwWrapperAcsJACSErrTest0Ex", logRec.getSourceMethodName());            
            Map properties = (Map) logRec.getParameters()[0];
            assertEquals("myTestStackID", properties.get("StackId"));
            assertEquals(123, ((Long)properties.get("StackLevel")).intValue());
        }        
    }
	
	
	/////////////////////////////////////////////////////////////////////////////////
	// Methods that simulate the various layers and boundaries of a real application
	/////////////////////////////////////////////////////////////////////////////////
    
	/**
	 * Represents an implementation method that does not rely on
	 * any other remote method (lowest level). 
	 * Therefore, the thrown <code>AcsJACSErrTest0Ex</code> is the VM original, 
	 * i.e. not converted from an <code>ErrorTrace</code>.
	 * 
	 * @throws AcsJACSErrTest0Ex (always), caused by a NPE with message "mean NPE".
	 */
	private void throwOriginalAcsJACSErrTest0Ex() throws AcsJACSErrTest0Ex
	{
		Throwable causedByEx = new NullPointerException("mean NPE");
		AcsJACSErrTest0Ex ex = new AcsJACSErrTest0Ex("low level ex", causedByEx);
		ex.setProperty("MyStupidProperty", "Poverty");
		throw ex;
	}


	/**
	 * This method can be seen as an example for a CORBA/remote method,
	 * such as the implementation of a method from a Java component's interface
	 * for which in IDL the exception <code>ACSErrTest0Ex</code> is declared.
	 * <p>
	 * The AcsJ-style exception is converted to its CORBA-equivalent,
	 * but no new exception is added to the chain (ErrorTrace). 
	 *  
	 * @throws ACSErrTest0Ex always, internally converted 
	 * 			from <code>AcsJACSErrTest0Ex</code>
	 * @see #throwOriginalAcsJACSErrTest0Ex()
	 */
	private void throwACSErrTest0Ex() throws ACSErrTest0Ex
	{
		// top-level try-catch block in the interface implementation
		try
		{
			// the interface impl internally works with AcsJExceptions...
			throwOriginalAcsJACSErrTest0Ex();
		}
		catch (AcsJACSErrTest0Ex e)
		{
			// but to the outside we must convert it to be CORBA compliant
			throw e.toACSErrTest0Ex();
		}
	}

	
	/**
	 * Represents an implementation method of some client application that calls 
	 * a remote method, here {@link #throwACSErrTest0Ex()}.
	 * <p>
	 * The <code>AcsJACSErrTest0Ex</code> that will be thrown by this "client" method 
	 * is caused by an exception in the "remote" method.
	 * That remote exception is in general not the VM original, but has been 
	 * converted from an <code>ACSErrTest0Ex</code>/<code>ErrorTrace</code>
	 * which is the format the exception took on to travel over the CORBA wire.
	 * <p>
	 * Once caught on the client side, the remote exception is wrapped
	 * by a new <code>AcsJACSErrTest0Ex</code> exception whose message is "remote call failed".
	 * Thus unlike in method {@link #throwConvertedAcsJACSErrTest0Ex()} here we actually 
	 * add a new exception to the top of the chain (or ErrorTrace in the CORBA picture).
	 *  
	 * @throws AcsJACSErrTest0Ex (always)
	 * @see #throwACSErrTest0Ex()
	 */
	private void throwWrapperAcsJACSErrTest0Ex() throws AcsJACSErrTest0Ex
	{
		try
		{
			// make (=fake) a remote call
			throwACSErrTest0Ex();
		}
		catch (ACSErrTest0Ex e)
		{
			AcsJACSErrTest0Ex acsJACSErrTest0Ex = new AcsJACSErrTest0Ex("remote call failed", e.errorTrace);
			throw acsJACSErrTest0Ex;
		}
	}

	/**
	 * Represents an implementation method of some client application that calls 
	 * a remote method, here {@link #throwACSErrTest0Ex()}.
	 * <p>
	 * The <code>AcsJACSErrTest0Ex</code> that will be thrown by this "client" method 
	 * is caused by an exception in the "remote" method.
	 * That remote exception is in general not the VM original, but has been 
	 * converted from an <code>ACSErrTest0Ex</code>/<code>ErrorTrace</code>
	 * which is the format the exception took on to travel over the CORBA wire.
	 * <p>
	 * Once caught on the client side, the remote exception is converted to 
	 * an <code>AcsJACSErrTest0Ex</code> exception which does not have an independent text message.
	 * Thus unlike in method {@link #throwWrapperAcsJACSErrTest0Ex()} here we do not  
	 * add a new exception to the top of the chain (or ErrorTrace in the CORBA picture).
	 *  
	 * @throws AcsJACSErrTest0Ex (always)
	 * @see #throwACSErrTest0Ex()
	 */
	private void throwConvertedAcsJACSErrTest0Ex() throws AcsJACSErrTest0Ex
	{
		try
		{
			// make a remote call
			throwACSErrTest0Ex();
		}
		catch (ACSErrTest0Ex e)
		{
			throw AcsJACSErrTest0Ex.fromACSErrTest0Ex(e);
		}
	}


	public static void main(String[] args)
	{
		alma.acs.testsupport.tat.TATJUnitRunner.run(AcsJExceptionTest.class);
	}

}
