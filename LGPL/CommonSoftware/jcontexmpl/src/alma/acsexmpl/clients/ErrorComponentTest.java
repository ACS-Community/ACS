package alma.acsexmpl.clients;

import org.omg.CORBA.SystemException;

import alma.acs.exceptions.AcsJException;

import alma.ACSErrTypeCommon.GenericErrorEx;
import alma.ACSErrTypeCommon.ACSErrTypeCommonEx;

import alma.ACSErrTypeCommon.wrappers.AcsJACSErrTypeCommonEx;
import alma.ACSErrTypeCommon.wrappers.AcsJGenericErrorEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;

import alma.acs.component.client.ComponentClientTestCase;
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
		//fail("Not yet implemented");
	}

	public void testCompletionFromCompletion() {
		//fail("Not yet implemented");
	}

	public void testCompletionOnStack() {
		//fail("Not yet implemented");
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
