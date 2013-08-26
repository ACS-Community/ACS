/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acsexmpl.clients;

import alma.ACSErrTypeCommon.ACSErrTypeCommonEx;
import alma.ACSErrTypeCommon.GenericErrorEx;
import alma.ACSErrTypeCommon.UnexpectedExceptionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.ACSErrTypeCommon.wrappers.AcsJGenericErrorEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnknownEx;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.exceptions.AcsJCompletion;
import alma.acs.exceptions.AcsJException;
import alma.acsexmplErrorComponent.ErrorComponent;
import alma.acsexmplErrorComponent.ErrorComponentHelper;


public class ErrorComponentTest extends ComponentClientTestCase {

	private static final String errorCompName = "ERRORCOMP_JAVA";
	private ErrorComponent errorComp;
	
	public ErrorComponentTest() throws Exception {
		super("ErrorComponentTest");
	}
	
	protected void setUp() throws Exception {
		super.setUp();
		errorComp = ErrorComponentHelper.narrow(getContainerServices().getComponent(errorCompName));
		assertEquals(errorCompName, errorComp.name());
	}

	protected void tearDown() throws Exception {
		getContainerServices().releaseComponent(errorCompName);
		super.tearDown();
	}

	
	public void testDisplayMessage() {
		errorComp.displayMessage();
	}

	
	public void testBadMethod() {
		try {
			// call the component method
			errorComp.badMethod((short)0);
		} catch (GenericErrorEx e) {
			fail("No exception should be thrown for depth=0");
		} catch (UnexpectedExceptionEx e){
                    AcsJUnexpectedExceptionEx jEx = AcsJUnexpectedExceptionEx.fromUnexpectedExceptionEx(e);
                    jEx.log(m_logger);
                    fail("No exception should be thrown for depth=0");
                }

		short[] depths = new short[] {1, 2, 5};
		for (int i = 0; i < depths.length; i++) {
			try {
				// call the component method
				errorComp.badMethod((depths[i]));
				fail("Expected a GenericErrorEx in ErrorComponent#badMethod for depth=" + depths[i]);
			} catch (GenericErrorEx e) {
				m_logger.info("Caught GenericErrorEx as expected. Depth=" + depths[i]);
				AcsJGenericErrorEx jEx = AcsJGenericErrorEx.fromGenericErrorEx(e);
                                verifyErrorTrace(depths[i], jEx);
                        } catch (UnexpectedExceptionEx e){
                                AcsJUnexpectedExceptionEx jEx = AcsJUnexpectedExceptionEx.fromUnexpectedExceptionEx(e);
                                jEx.log(m_logger);
                                fail("Expected a GenericErrorEx in ErrorComponent#badMethod for depth=" + depths[i]);
                        }
                }
	}

	
	public void testExceptionFromCompletion() {
		// depth == 0
		try {
			// call the component method
			errorComp.exceptionFromCompletion((short)0);
		} catch (GenericErrorEx e) {
			fail("No exception should be thrown for depth=0");
		}			
		// depth > 0
		short[] depths = new short[] {1, 2, 3, 5, 13};
		for (int i = 0; i < depths.length; i++) {
			try {
				// call the component method
				errorComp.exceptionFromCompletion((depths[i]));
				fail("Expected a GenericErrorEx in ErrorComponent#badMethod for depth=" + depths[i]);
			} catch (GenericErrorEx e) {
				m_logger.info("Caught GenericErrorEx as expected. Depth=" + depths[i]);
				AcsJGenericErrorEx jEx = AcsJGenericErrorEx.fromGenericErrorEx(e);
				verifyErrorTrace(depths[i], jEx);
			}
		}
	}

	public void testTypeException() {
		// depth == 0
		try {
			// call the component method
			errorComp.typeException((short)0);
		} catch (GenericErrorEx e) {
			fail("No exception should be thrown for depth=0");
		} catch (ACSErrTypeCommonEx e) {
			fail("No exception should be thrown for depth=0");
		}			
		// depth > 0
		short[] depths = new short[] {1, 2, 3, 5, 13};
		for (int i = 0; i < depths.length; i++) {
			try {
				// call the component method
				errorComp.typeException((depths[i]));
				fail("Expected a GenericErrorEx in ErrorComponent#badMethod for depth=" + depths[i]);
			} catch (GenericErrorEx e) {
				fail("The declared 'GenericErrorEx' should never be thrown by the Java implementation.");
			} catch (ACSErrTypeCommonEx e) {
				m_logger.info("Caught GenericErrorEx as expected. Depth=" + depths[i]);
// TODO: add 'fromXYZ' method to generated type-exceptions. These would no longer be abstract, but have protected ctors.				
			//	AcsJACSErrTypeCommonEx jEx = AcsJACSErrTypeCommonEx.fromAcsJACSErrTypeCommonEx(e);
			//	verifyErrorTrace(depths[i], jEx);
				// while we do not have the static fromXYZ method, we convert the exception by wrapping it, and take account of the depth increase
				verifyErrorTrace(depths[i] + 1, new AcsJGenericErrorEx(e));
			}						
		}
	}

        public void testReceiveCorbaSystemException() {
	    try {
	        errorComp.corbaSystemException();
	    }
	    /*
	     * This shows how to map a CORBA System Exception into the
	     * corresponding ACS Exception wrapper
	     */
	    catch(org.omg.CORBA.SystemException ex) {
	        m_logger.info("Caught CORBA.SystemException");
		AcsJCORBAProblemEx corbaProblemEx = new AcsJCORBAProblemEx(ex);
		corbaProblemEx.setMinor(ex.minor);
		corbaProblemEx.setCompletionStatus(ex.completed.value());
		corbaProblemEx.setInfo(ex.toString());
		corbaProblemEx.log(m_logger);
	    } catch(Throwable th) {
	        m_logger.info("Caught an unexpected Exception");
   	        AcsJGenericErrorEx badMethodEx = new AcsJGenericErrorEx(th);
		badMethodEx.setErrorDesc("corbaSystemException has thrown an UNEXPECTED exception");
		badMethodEx.log(m_logger);
		fail("Expected a CORBA System Exception");
	    }

        }

	public void testCompletionFromException() {
		// depth == 0
                AcsJCompletion comp=null;
		try {
			// call the component method
			comp = AcsJCompletion.fromCorbaCompletion(errorComp.completionFromException((short)0));
		} catch (Throwable th) {
                        m_logger.info("Caught an unexpected Exception");
                        AcsJUnknownEx ex = new AcsJUnknownEx(th);
                        ex.log(m_logger);
			fail("No exception should be thrown");
		}			
                verifyErrorTrace(1, new AcsJGenericErrorEx(comp.getAcsJException()));

		// depth > 0
		short[] depths = new short[] {1, 2, 3, 5, 13};
		for (int i = 0; i < depths.length; i++) {
			try {
				// call the component method
				comp = AcsJCompletion.fromCorbaCompletion(errorComp.completionFromException((depths[i])));
			} catch (Throwable th) {
				m_logger.info("Caught an unexpected Exception at depth "+depths[i]);
                                AcsJUnknownEx ex = new AcsJUnknownEx(th);
                                ex.log(m_logger);
                                fail("No exception should be thrown");
			}
                        verifyErrorTrace(depths[i]+1, new AcsJGenericErrorEx(comp.getAcsJException()));
		}

	}

	public void testCompletionFromCompletion() {
		// depth == 0
                AcsJCompletion comp=null;
		try {
			// call the component method
			comp = AcsJCompletion.fromCorbaCompletion(errorComp.completionFromCompletion((short)0));
		} catch (Throwable th) {
                        m_logger.info("Caught an unexpected Exception");
                        AcsJUnknownEx ex = new AcsJUnknownEx(th);
                        ex.log(m_logger);
			fail("No exception should be thrown");
		}			
                verifyErrorTrace(1, new AcsJGenericErrorEx(comp.getAcsJException()));
 
		// depth > 0
		short[] depths = new short[] {1, 2, 3, 5, 13};
		for (int i = 0; i < depths.length; i++) {
			try {
				// call the component method
				comp = AcsJCompletion.fromCorbaCompletion(errorComp.completionFromCompletion((depths[i])));
			} catch (Throwable th) {
				m_logger.info("Caught an unexpected Exception at depth "+depths[i]);
                                AcsJUnknownEx ex = new AcsJUnknownEx(th);
                                ex.log(m_logger);
                                fail("No exception should be thrown");
			}
                        verifyErrorTrace(depths[i]+1, new AcsJGenericErrorEx(comp.getAcsJException()));
		}
	}

	public void testCompletionOnStack() {
		//fail("Not yet implemented");
	}
        
        public void testOutCompletion() {
            alma.ACSErr.CompletionHolder comp= new alma.ACSErr.CompletionHolder();
            try{
                    errorComp.outCompletion(comp);
            } catch(Throwable th) {
                    m_logger.info("Caught an unexpected Exception");
                    AcsJUnknownEx ex = new AcsJUnknownEx(th);
                    ex.log(m_logger);
                    fail("No exception should be thrown");
            }
            assertEquals(0 , comp.value.type );
            assertEquals(0 , comp.value.code );
        }
	
	///////////////////////////////////////////////////////////////
	
	private void verifyErrorTrace(int depth, AcsJException jEx) {
		Throwable cause = jEx;
		for (int i = 1; i < depth; i++) {
			assertNotNull("ErrorTrace too short for depth=" + depth+ ": exception cause #" + i + " missing.", cause.getCause());
			cause = cause.getCause();
		}
		assertNull("ErrorTrace too long for depth=" + depth + ": exception cause #" + depth + " has another cause. ", cause.getCause());
	}


}
