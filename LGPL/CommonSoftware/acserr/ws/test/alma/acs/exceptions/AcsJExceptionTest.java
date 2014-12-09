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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.InetAddress;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.ACSErr.ACSErrTypeTest;
import alma.ACSErr.Completion;
import alma.ACSErr.ErrorTrace;
import alma.ACSErr.Severity;
import alma.ACSErrTypeTest.ACSErrTest0;
import alma.ACSErrTypeTest.ACSErrTest3;
import alma.ACSErrTypeTest.ACSErrTestOK;
import alma.ACSErrTypeTest.wrappers.ACSErrTest1AcsJCompletion;
import alma.ACSErrTypeTest.wrappers.ACSErrTest2AcsJCompletion;
import alma.ACSErrTypeTest.wrappers.ACSErrTest3AcsJCompletion;
import alma.ACSErrTypeTest.wrappers.ACSErrTestOKAcsJCompletion;
import alma.ACSErrTypeTest.wrappers.AcsJACSErrTest0Ex;
import alma.ACSErrTypeTest.wrappers.AcsJACSErrTest1Ex;
import alma.ACSErrTypeTest.wrappers.AcsJACSErrTest2Ex;
import alma.ACSErrTypeTest.wrappers.AcsJACSErrTest3Ex;
import alma.acs.testsupport.LogRecordCollectingLogger;
import alma.acs.util.UTCUtility;


/**
 * Tests for {@link AcsJException}, {@link alma.acs.exceptions.AcsJCompletion} 
 * and the generated example subclasses. 
 * 
 * @author hsommer Jun 18, 2003 3:54:40 PM
 */
public class AcsJExceptionTest extends TestCase
{
	private ClientServerExceptionExample exSystem;
	private String hostName;

	public AcsJExceptionTest() throws Exception {
		super("AcsJExceptionTest");
		exSystem = new ClientServerExceptionExample();
		hostName = InetAddress.getLocalHost().getHostName();
	}
	
	/**
	 * Uses {@link ClientServerExceptionExample#throwOriginalAcsJACSErrTest0Ex} to verify the output 
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
			exSystem.throwOriginalAcsJACSErrTest0Ex();
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
	 * Calls {@link ClientServerExceptionExample#throwWrapperAcsJACSErrTest0Ex} and 
	 * {@link ClientServerExceptionExample#throwConvertedAcsJACSErrTest0Ex} to get the 
	 * three linked exceptions
	 * <ul>
	 * <li> <code>AcsJACSErrTest0Ex</code> thrown locally ("client side"),
	 * 			only in case of <code>throwWrapperAcsJACSErrTest0Ex</code>
	 * <li> <code>AcsJException</code> caused by a 
	 * 		<code>NullPointerException</code>, both thrown "remotely"
	 * 		in the implementation of the "server side" method 
	 * 		{@link ClientServerExceptionExample#throwACSErrTest0Ex}
	 * 		and translated back and forth from an <code>ErrorTrace</code>
	 * </ul>
	 * Checks that the exceptions are correctly chained up
	 * and the data is correct.
	 */
	public void testErrorTraceToAcsJExConversion()
	{
		// first the variant with a new exception created on the client side,
        // and with explicit extraction of the ErrorTrace object from the CORBA exception
		try {
			exSystem.throwWrapperAcsJACSErrTest0Ex(false);
		}
		catch (AcsJACSErrTest0Ex ex1) {
			checkWrapperAcsJEx(ex1);
		}
		
        // same as above, but with the variation of 
        // automatic extraction of the ErrorTrace object from the CORBA exception
        try {
            exSystem.throwWrapperAcsJACSErrTest0Ex(true);
        }
        catch (AcsJACSErrTest0Ex ex2) {
            checkWrapperAcsJEx(ex2);
        }
        
		// next we try the client-side conversion instead of wrapping
		try
		{
			exSystem.throwConvertedAcsJACSErrTest0Ex();
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
			assertEquals("mean NPE", cause1.getMessage());
			assertEquals("java.lang.NullPointerException", ((AcsJException)cause1).getShortDescription());
			DefaultAcsJException acsJCause1 = (DefaultAcsJException) cause1;
			assertEquals(1, acsJCause1.getTraceDepth());
		}		
	}


    /**
     * Checks the structure and values of the exception obtained from 
     * {@link ClientServerExceptionExample#throwWrapperAcsJACSErrTest0Ex(boolean)}. 
     * @param ex 
     */
    private void checkWrapperAcsJEx(AcsJACSErrTest0Ex ex) {
        assertEquals("remote call failed", ex.getMessage());
        assertEquals("ClientServerExceptionExample.java", ex.getFile());
        assertEquals(3, ex.getTraceDepth());
        assertEquals(hostName, ex.getHost());
        String threadName = ex.getThreadName();
        assertNotNull(threadName);
        assertTrue(threadName.trim().length() > 0);
        assertTrue(ex.getLine() > 0);
        assertNotNull(ex.getMethod());
        assertTrue(ex.getMethod().trim().length() > 0);
        assertEquals(Severity.Error, ex.getSeverity());
        Throwable cause1 = ex.getCause();
        assertNotNull(cause1);
        assertTrue(cause1 instanceof AcsJACSErrTest0Ex && cause1 != ex);
        assertEquals("low level ex", cause1.getMessage());
        AcsJACSErrTest0Ex acsJCause1 = (AcsJACSErrTest0Ex) cause1;
        assertEquals("ClientServerExceptionExample.java", acsJCause1.getFile());
        assertEquals("Poverty", acsJCause1.getProperty("MyStupidProperty"));
        assertEquals(2, acsJCause1.getTraceDepth());
        long timeCause1 = acsJCause1.getTimestampMillis();
        assertEquals("ClientServerExceptionExample.java", acsJCause1.getFile());
        assertEquals(hostName, acsJCause1.getHost());
        assertEquals(threadName, acsJCause1.getThreadName());
        assertTrue(acsJCause1.getLine() > 0);
        assertNotNull(acsJCause1.getMethod());
        assertTrue(acsJCause1.getMethod().trim().length() > 0);
        assertEquals(Severity.Error, acsJCause1.getSeverity());
        Throwable cause2 = cause1.getCause();
        assertNotNull(cause2);
        assertTrue(cause2 instanceof DefaultAcsJException);
        assertEquals("mean NPE", cause2.getMessage());
        assertEquals("java.lang.NullPointerException", ((DefaultAcsJException)cause2).getShortDescription());
        DefaultAcsJException acsJCause2 = (DefaultAcsJException) cause2;
        assertEquals("ClientServerExceptionExample.java", acsJCause2.getFile());
        assertEquals(timeCause1 - 1, acsJCause2.getTimestampMillis());
        assertEquals(1, acsJCause2.getTraceDepth());
        assertEquals(hostName, acsJCause2.getHost());
        assertEquals("NA", acsJCause2.getThreadName()); // NPE did not carry thread name info
        assertTrue(acsJCause2.getLine() > 0);
        assertNotNull(acsJCause2.getMethod()); 
        assertTrue(acsJCause2.getMethod().trim().length() > 0);
        assertEquals(Severity.Error, acsJCause2.getSeverity());
    }
	
	
	/**
	 * Creates Completion objects from scratch, from other completion objects,
     * with and without associated exception, and checks the values.
	 */
	public void testJCompletion()
	{
		// a completion without associated exception
		AcsJCompletion jcompl1 = new ACSErrTestOKAcsJCompletion();
		assertEquals(ACSErrTypeTest.value, jcompl1.getType());
		assertEquals(ACSErrTestOK.value, jcompl1.getCode());
		assertFalse(jcompl1.isError());
		assertTrue(jcompl1.getTimeStamp() > 0);
		Completion corbacompl = jcompl1.toCorbaCompletion();
        assertEquals(ACSErrTypeTest.value, corbacompl.type);
        assertEquals(ACSErrTestOK.value, corbacompl.code);
		assertTrue(corbacompl.previousError.length == 0);
		assertEquals(UTCUtility.utcJavaToOmg(jcompl1.getTimeStamp()), corbacompl.timeStamp);
		
        long timeMillisBeforeExceptionCreation = System.currentTimeMillis();
        
		// completion with associated exception, 
        // trying to wrap the previous completion (which has no exception that could be wrapped)
		AcsJCompletion jcompl2 = new ACSErrTest1AcsJCompletion(jcompl1);
		assertTrue(jcompl2.isError());
		AcsJException acsjex2 = jcompl2.getAcsJException();
		assertTrue(acsjex2 instanceof AcsJACSErrTest1Ex);
		assertNull(acsjex2.getCause());

        // completion with associated exception, 
        // but this time trying to wrap the plain CORBA completion instead of its AcsJ-equivalent.
		AcsJCompletion jcompl2b = new ACSErrTest2AcsJCompletion(corbacompl);
		assertTrue(jcompl2b.isError());
        AcsJException acsjex2b = jcompl2b.getAcsJException();
        assertTrue(acsjex2b instanceof AcsJACSErrTest2Ex);
        assertNull(acsjex2b.getCause());
		
        // now wrapping a completion with exception with another completion with exception
		AcsJCompletion jcompl3 = new ACSErrTest3AcsJCompletion(jcompl2);
        assertTrue(jcompl3.isError());
        AcsJException acsjex3 = jcompl3.getAcsJException();
        assertTrue(acsjex3 instanceof AcsJACSErrTest3Ex);
        Throwable acsjex_cause = acsjex3.getCause();
        assertNotNull(acsjex_cause);
        Throwable acsjex_cause2 = acsjex_cause.getCause();
        assertNull(acsjex_cause2);
        Completion corbacomp3 = jcompl3.toCorbaCompletion();
        assertEquals(ACSErrTypeTest.value, corbacomp3.type);
        assertEquals(ACSErrTest3.value, corbacomp3.code);
        assertNotNull(corbacomp3.previousError);
        assertEquals(1, corbacomp3.previousError.length);
        ErrorTrace trace_3_1 = corbacomp3.previousError[0];
        assertNotNull(trace_3_1);
        ErrorTrace trace_3_0 = trace_3_1.previousError[0];
        assertNotNull(trace_3_0);
        assertEquals(0, trace_3_0.previousError.length);

        // check the various timestamps
        long timeMillisAfterExceptionCreation = System.currentTimeMillis();
        assertTrue(timeMillisBeforeExceptionCreation <= acsjex2.getTimestampMillis());
        assertTrue(timeMillisAfterExceptionCreation >= acsjex2.getTimestampMillis());
        assertTrue(timeMillisBeforeExceptionCreation <= acsjex2b.getTimestampMillis());
        assertTrue(timeMillisAfterExceptionCreation >= acsjex2b.getTimestampMillis());
        long corbacomp3_millis = UTCUtility.utcOmgToJava(corbacomp3.timeStamp);
        assertTrue(timeMillisBeforeExceptionCreation <= corbacomp3_millis);
        assertTrue(timeMillisAfterExceptionCreation >= corbacomp3_millis);
        long trace_3_0_millis = UTCUtility.utcOmgToJava(trace_3_0.timeStamp);
        assertTrue(timeMillisBeforeExceptionCreation <= trace_3_0_millis);
        assertTrue(timeMillisAfterExceptionCreation >= trace_3_0_millis);
	}
	
    
	/**
	 * Checks if logging of ACS exceptions (including caused-by exceptions) works.
	 * Here "logging" refers to special logging via
	 * {@link AcsJException#log(java.util.logging.Logger)}.
	 * <p>
	 * See also <code>alma.demo.client.XmlComponentClient#testException()</code> in module jcontexmpl.
	 */
	public void testLogAcsJException() {
        LogRecordCollectingLogger logger = LogRecordCollectingLogger.getCollectingLogger("AcsJExceptionTest-Logger");

        long timeBefore = System.currentTimeMillis();
        try {
            exSystem.throwWrapperAcsJACSErrTest0Ex(true);
        } catch (AcsJACSErrTest0Ex e) {
            long timeAfter = System.currentTimeMillis();
            assertEquals(3, e.getTraceDepth());
            String firstLogMsg = "Will log exception coming from throwWrapperAcsJACSErrTest0Ex";
            logger.info(firstLogMsg);
            e.log(logger);
            LogRecord[] logRecords = logger.getCollectedLogRecords();
            
            assertNotNull(logRecords);
            assertEquals(4, logRecords.length);
            assertEquals(firstLogMsg, logRecords[0].getMessage());
            
            // The top-level exception is logged first, with stack level = 2
            LogRecord lr2 = logRecords[1];
            assertEquals("Test error 0 (type=" + ACSErrTypeTest.value + ", code=" + ACSErrTest0.value + ") :: remote call failed", lr2.getMessage());
            assertTrue(lr2.getMillis() >= timeBefore);
            assertTrue(lr2.getMillis() <= timeAfter);
            Map logProperties = (Map) lr2.getParameters()[0];
            assertTrue(logProperties.containsKey("Line"));
            assertTrue(logProperties.containsKey("ThreadName"));
            assertEquals(hostName, logProperties.get("HostName"));
            assertEquals("throwWrapperAcsJACSErrTest0Ex", lr2.getSourceMethodName());
            final String stackId = (String) logProperties.get("StackId");
            assertNotNull(stackId);
            assertEquals(2, ((Long)logProperties.get("StackLevel")).intValue());
            Map userProps2 = (Map) lr2.getParameters()[1];
            assertEquals(0, userProps2.size());
            
            // The causing exception is logged next, with stack level = 1
            LogRecord lr1 = logRecords[2];
            String msg1 = lr1.getMessage();
            assertEquals("Test error 0 (type=" + ACSErrTypeTest.value + ", code=" + ACSErrTest0.value + ") :: low level ex", msg1);
            assertTrue(lr1.getMillis() >= timeBefore);
            assertTrue(lr1.getMillis() <= timeAfter);
            logProperties = (Map) lr1.getParameters()[0];
            assertTrue(logProperties.containsKey("Line"));
            assertTrue(logProperties.containsKey("ThreadName"));
            assertEquals(hostName, logProperties.get("HostName"));
            assertEquals("throwOriginalAcsJACSErrTest0Ex", lr1.getSourceMethodName());
            assertEquals(stackId, logProperties.get("StackId"));
            assertEquals(1, ((Long)logProperties.get("StackLevel")).intValue());
            Map userProps1 = (Map) lr1.getParameters()[1];
            assertEquals(1, userProps1.size());
            assertEquals("Poverty", userProps1.get("MyStupidProperty"));

            // last comes the wrapped NullPointerException, with stack level = 0
            LogRecord lr0 = logRecords[3];
            String msg0 = lr0.getMessage();
            assertEquals("java.lang.NullPointerException (type=0, code=0) :: mean NPE", msg0);
            logProperties = (Map) lr0.getParameters()[0];
            assertTrue(logProperties.containsKey("Line"));
            assertTrue(logProperties.containsKey("ThreadName"));
            assertEquals(hostName, logProperties.get("HostName"));
            assertEquals("throwOriginalAcsJACSErrTest0Ex", lr0.getSourceMethodName());
            assertEquals(stackId, logProperties.get("StackId"));
            assertEquals(0, ((Long)logProperties.get("StackLevel")).intValue());
            Map userProps0 = (Map) lr0.getParameters()[1];
            assertEquals(0, userProps0.size());
            
        }
    }

	/**
	 * Tests how an exception is logged via the standard method {@link Logger#log(Level, String, Throwable)}.
	 * In particular, this test verifies that also JDK (= non-AcsJ) exceptions are correctly transformed 
	 * into Corba-style ACS exceptions and back.
	 */
	public void testStackTraceLog() {
		
		try {
			exSystem.throwConvertedAcsJACSErrTest0Ex();
		} catch (AcsJACSErrTest0Ex jex) {
			// This jex has already gone from AcsJ to Corba to AcsJ. 
			// Just to be sure we catch all possible issues, we do another round of conversion here
			AcsJACSErrTest0Ex jex2 = AcsJACSErrTest0Ex.fromACSErrTest0Ex(
					jex.toACSErrTest0Ex()
				);
			
			// Check the stack (should be the original location)
			StringWriter wr = new StringWriter();
			jex2.printStackTrace(new PrintWriter(wr));
			String linesep = System.getProperty("line.separator");
			String expectedTrace = "alma.ACSErrTypeTest.wrappers.AcsJACSErrTest0Ex: low level ex [ MyStupidProperty='Poverty' ] " + linesep
				+ "	at ---.throwOriginalAcsJACSErrTest0Ex(ClientServerExceptionExample.java:48)" + linesep
				+ "Caused by: alma.acs.exceptions.DefaultAcsJException (java.lang.NullPointerException): mean NPE" + linesep
				+ "	at ---.throwOriginalAcsJACSErrTest0Ex(ClientServerExceptionExample.java:47)" + linesep;
			assertEquals(expectedTrace, wr.toString());
		}
	}
}
