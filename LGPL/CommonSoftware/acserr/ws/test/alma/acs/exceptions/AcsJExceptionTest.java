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

import alma.ACSErr.Completion;
import alma.ACSErr.ErrorTrace;
import alma.ACSErrTypeTest.wrappers.AcsJACSErrTest0Ex;
import alma.ACSErrTypeTest.ACSErrTest0Ex;
import alma.acs.testsupport.TestLogger;

import junit.framework.TestCase;

/**
 * Tests for {@link AcsJException} and example subclasses 
 * 
 * @author hsommer Jun 18, 2003 3:54:40 PM
 */
public class AcsJExceptionTest extends TestCase
{

	/**
	 * Constructor for AcsJExceptionTest.
	 * @param name
	 */
	public AcsJExceptionTest(String name)
	{
		super(name);
	}

	
	/**
	 * Uses {@link #throwOriginalAcsJACSErrTest0Ex} to verify the output 
	 * of the method {@link AcsJException#getErrorTrace}.
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
			assertTrue("1 chained child ex", prev.length == 1);
			assertTrue("no grandchild ex", prev[0].previousError.length == 0);
            
            // check data of ErrorTrace that was constructed from a non-ACS exception
            assertEquals("time stamp for non-ACS exceptions should be defaulted to be 1 millisec less than the parent's time stamp", 
                    et.timeStamp-10000, prev[0].timeStamp);
		}
	}
	
	
	/**
	 * Tests the conversion from {@link AcsJACSErrTest0Ex}
	 * to {@link ErrorTrace} and back, both by wrapping and by directly
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
		try
		{
			throwWrapperAcsJACSErrTest0Ex();
		}
		catch (AcsJException e)
		{
			// todo check other data besides type and message
			
			assertTrue(e instanceof AcsJACSErrTest0Ex);
			assertEquals("remote call failed", e.getMessage());

			Throwable cause1 = e.getCause();
			assertNotNull(cause1);
			assertTrue(cause1 instanceof AcsJACSErrTest0Ex && cause1 != e);
			assertEquals("low level ex", cause1.getMessage());
			
			Throwable cause2 = cause1.getCause();
			assertNotNull(cause2);
			assertTrue(cause2 instanceof DefaultAcsJException);
			assertEquals("original type java.lang.NullPointerException: mean NPE", cause2.getMessage());
		}
		
		try
		{
			throwConvertedAcsJACSErrTest0Ex();
		}
		catch (AcsJException e)
		{
			// todo check other data besides type and message
			
			assertTrue(e instanceof AcsJACSErrTest0Ex);
			assertEquals("low level ex", e.getMessage());

			Throwable cause1 = e.getCause();
			assertNotNull(cause1);
			assertTrue(cause1 instanceof DefaultAcsJException);
			assertEquals("original type java.lang.NullPointerException: mean NPE", cause1.getMessage());
		}
		
	}
	
	
	public void testJCompletion()
	{
		AcsJCompletion jcompl1 = new ExampleNoErrAcsJCompletion();
		assertFalse(jcompl1.isError());
		Completion corbacompl = jcompl1.toCorbaCompletion();
		assertTrue(corbacompl.previousError.length == 0);
		
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
	
    
	/**
	 * Represents an implementation method that does not rely on
	 * any other remote method (lowest level). 
	 * Therefore, the thrown AcsJACSErrTest0Ex is the VM original, 
	 * i.e. not converted from an <code>ErrorTrace</code>.
	 * 
	 * @throws AcsJACSErrTest0Ex (always), caused by a NPE
	 */
	private void throwOriginalAcsJACSErrTest0Ex() throws AcsJACSErrTest0Ex
	{
		Throwable causedByEx = new NullPointerException("mean NPE");
		AcsJACSErrTest0Ex ex = new AcsJACSErrTest0Ex("low level ex", causedByEx);
		throw ex;
	}


	/**
	 * Represents a CORBA/remote method,
	 * for example one from a Java component's interface.
	 * @throws ACSErrTest0Ex always, internally converted 
	 * 			from <code>AcsJACSErrTest0Ex</code>
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
			// but to the outside (CORBA) we must convert it
			throw e.toACSErrTest0Ex();
		}
	}

	
	/**
	 * Represents an implementation method that relies on
	 * another remote method. 
	 * Therefore, the thrown <code>AcsJACSErrTest0Ex</code> is caused by 
	 * an exception that is not the VM original, but has been 
	 * converted from an <code>ACSErrTest0Ex</code>/<code>ErrorTrace</code>.
	 * @throws AcsJACSErrTest0Ex (always)
	 */
	private void throwWrapperAcsJACSErrTest0Ex() throws AcsJACSErrTest0Ex
	{
		try
		{
			// make a remote call
			throwACSErrTest0Ex();
		}
		catch (ACSErrTest0Ex e)
		{
			AcsJACSErrTest0Ex acsJACSErrTest0Ex = new AcsJACSErrTest0Ex("remote call failed", e.errorTrace);
			throw acsJACSErrTest0Ex;
		}
	}

	/**
	 * Represents an implementation method that relies on
	 * another remote method. 
	 * Therefore, the thrown <code>AcsJACSErrTest0Ex</code> is converted from 
	 * its CORBA representation as <code>ACSErrTest0Ex</code>.
	 * @throws AcsJACSErrTest0Ex (always)
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
