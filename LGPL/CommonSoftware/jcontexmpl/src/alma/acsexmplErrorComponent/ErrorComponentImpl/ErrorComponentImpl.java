/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

package alma.acsexmplErrorComponent.ErrorComponentImpl; 

import org.omg.CORBA.NO_IMPLEMENT;

import alma.ACSErr.Completion;
import alma.ACSErrTypeCommon.ACSErrTypeCommonEx;
import alma.ACSErrTypeCommon.GenericErrorEx;
import alma.ACSErrTypeCommon.wrappers.AcsJACSErrTypeCommonEx;
import alma.ACSErrTypeCommon.wrappers.AcsJGenericErrorEx;
import alma.ACSErrTypeCommon.wrappers.GenericErrorAcsJCompletion;
import alma.ACSErrTypeOK.wrappers.ACSErrOKAcsJCompletion;
import alma.acs.component.ComponentImplBase;
import alma.acs.exceptions.AcsJCompletion;
import alma.acs.exceptions.AcsJException;
import alma.acsexmplErrorComponent.ErrorComponentOperations;

/**
 * Implementation of the <code>ErrorComponent</code> interface,
 * which should demonstrate proper use of the ACS error system in Java.
 * <p>
 * Note on logging: unlike in the C++ implementation of this component, here we do not log the invocation 
 * of every method, because the Java container does this automatically.
 * In general the component should only trace the invocations itself if it needs to add custom data to the log.
 * @author hsommer
 */
public class ErrorComponentImpl extends ComponentImplBase implements ErrorComponentOperations {
	
	public void displayMessage() {
		m_logger.info("ErrorComponent");
	}
	
	
	/**
	 * An intentionally bad method that throws an exception (<code>GenericErrorEx</code>) 
	 * if <code>depth</code> > 0. This exception has causing exception if depth > 1. 
	 * All exceptions get added to the <code>ErrorTrace</code> list which will transport them over CORBA.
	 * <p>
	 * Note that the thrown CORBA exception <code>GenericErrorEx</code> only acts as a vehicle for the embedded <code>ErrorTrace</code> linked list 
	 * which contains the real data of the exception(s).
	 * @see alma.acsexmplErrorComponent.ErrorComponentOperations#badMethod(short)
	 */
	@Override
	public void badMethod(short depth) throws GenericErrorEx {
		try {
			// our internal method uses the convenient "AcsJ-" JDK-style variant of GenericErrorEx
			internalBadMethod(depth);
		} catch (AcsJGenericErrorEx e) {
			// when leaving the Java component boundary, we must convert the JDK-style-exception to its plain CORBA/ACS-style peer.
			throw e.toGenericErrorEx();
		} catch (Throwable thr) {
			throw new AcsJGenericErrorEx("Got unexpected exception", thr).toGenericErrorEx();
		}
	}
	
	
	/**
	 * An intentionally bad method that throws a CORBA::BAD_PARAM system exception
         * to show how to handle CORBA System Exceptions
	 * @see alma.acsexmplErrorComponent.ErrorComponentOperations#corbaSystemException()
	 */
	@Override
	public void corbaSystemException() throws org.omg.CORBA.SystemException {
			throw new org.omg.CORBA.BAD_PARAM("Test throwing a CORBA Exception");
	}
	
	
	/**
	 * This method throws a <code>GenericErrorEx</code> exception if the completion
	 * returned from {@link #internalCompletionMethod(int)} has an associated exception.
	 * This will be the case if the <code>depth</code> parameter is > 0.
	 * @see alma.acsexmplErrorComponent.ErrorComponentOperations#exceptionFromCompletion(short)
	 */
	@Override
	public void exceptionFromCompletion(short depth) throws GenericErrorEx {
		try {
			if (depth == 1) {
				// this case is ugly if we really want to fulfill the contract and throw a single exception (i.e. error trace of length one).
				// It seems that for an example component we should work without a 'depth' parameter, and rather have two different methods instead of this highly powerful one method.
				AcsJCompletion completion = internalCompletionMethod(depth);			
				// here we cannot wrap the contained exception, because then the total error trace would be of depth == 2.
				// Therefore we must check for the correct exception type, and use that exception directly.
				if (completion.getAcsJException() instanceof AcsJGenericErrorEx) {
					AcsJGenericErrorEx knownEx = (AcsJGenericErrorEx) completion.getAcsJException();
					throw knownEx.toGenericErrorEx();
				}
				else {
					// this should never happen
					throw new AcsJGenericErrorEx("Unexpected component implementation bug. AcsJGenericErrorEx was expected in Completion object.").toGenericErrorEx();
				}
			}
			else { 
				// with depth != 1 we can construct a completion with (depth-1) exceptions, and wrap those with a new exception if they exist
				AcsJCompletion completion = internalCompletionMethod(depth - 1);
				// completion should have an error unless depth was 0
				if (completion.isError()) {
					AcsJException acsjEx = completion.getAcsJException();
					throw (new AcsJGenericErrorEx(acsjEx)).toGenericErrorEx();
				}
			}
		} catch (GenericErrorEx ex) {
			throw ex; // that's our checked exception
		} catch (Throwable thr) {
			// that's an unchecked exception which we must wrap
			throw new AcsJGenericErrorEx("Got unexpected exception", thr).toGenericErrorEx();
		} 
	}
	
	
	@Override
	public void typeException(short depth) throws ACSErrTypeCommonEx { //, GenericErrorEx {
		try {
			internalBadMethod(depth);
		} catch (AcsJACSErrTypeCommonEx ex) {
			// here we made use of the inheritance hierarchy in the AcsJ-style exceptions, 
			// and caught the flying AcsJGenericErrorEx through its base type AcsJACSErrTypeCommonEx
			throw ex.toACSErrTypeCommonEx();
		} catch (Throwable thr) {
			throw new AcsJGenericErrorEx("Got unexpected exception", thr).toACSErrTypeCommonEx();
		}
	}
	
	
	/**
	 * 
	 * @see alma.acsexmplErrorComponent.ErrorComponentOperations#completionFromException(short)
	 */
	@Override
	public Completion completionFromException(short depth) {
		// this completion will be built from a chain of exceptions if depth > 0 
		AcsJCompletion completion = internalCompletionMethod(depth);
		
		return completion.toCorbaCompletion();
	}
	
	
	/**
	 * 
	 * @see alma.acsexmplErrorComponent.ErrorComponentOperations#completionFromCompletion(short)
	 */
	@Override
	public Completion completionFromCompletion(short depth) {
		AcsJCompletion completion = null;
		if (depth <= 1) {
			completion = internalCompletionMethod(depth);
			return completion.toCorbaCompletion();
		} 
		else {
			completion = internalCompletionMethod(depth - 1);
			// here we show how to wrap the error from a given completion with a new completion
			AcsJCompletion newCompletion = new GenericErrorAcsJCompletion(completion);
			return newCompletion.toCorbaCompletion();
		}
	}
	
	/**
	 * Forwards to {@link #completionFromException(short)} because in Java we can't create objects on the stack, so
	 * there is no distinction to be made.
	 * 
	 * @see alma.acsexmplErrorComponent.ErrorComponentOperations#completionOnStack(short)
	 */
	@Override
	public Completion completionOnStack(short depth) {
		return completionFromException(depth);
	}

	/**
	 * this method returns a Completion as an out parameter.
	 * 
	 * @see alma.acsexmplErrorComponent.ErrorComponentOperations#outCompletion(alma.ACSErr.CompletionHolder)
	 */
	public void outCompletion(alma.ACSErr.CompletionHolder comp) {
		comp.value = (new ACSErrOKAcsJCompletion()).toCorbaCompletion();
	}

	@Override
	public void generateSIGFPE(short way) {
		throw new NO_IMPLEMENT();
	}

	@Override
	public void generateSIGSEGV(short way) {
		throw new NO_IMPLEMENT();
	}

	
	
	///////////////////////////////////////////////////////////////////////
	
	/**
	 * Method that calls itself recursively to construct an AcsJGenericErrorEx exception
	 * with causing exceptions of the same type.
	 * <p>
	 * This behavior is meant to mimic a number of calls down into the implementation classes of a real-world component,
	 * even though there we would typically deal with different methods calling one another, 
	 * instead of the same method calling itself recursively.  
	 * @param depth the number of chained exceptions thrown in the end. May be zero, meaning that no exception will be thrown. 
	 * @throws AcsJGenericErrorEx, which is the JDK-style peer of {@link GenericErrorEx}.
	 */
	private void internalBadMethod(int depth) throws AcsJGenericErrorEx {
		if (depth < 1) {
			return;
		}
		
		if (depth == 1) {
			throw new AcsJGenericErrorEx("This exception is the original cause."); 
		}
		
		try {
			// recursion
			internalBadMethod(depth - 1);
		}
		catch (AcsJGenericErrorEx ex) {
			// we use the causing exception to build up the JDK-style chain of exceptions. 
			// That chain will later be converted into an ACS-style ErrorTrace.
			throw new AcsJGenericErrorEx("This exception was caused by another exception...", ex); 
		}
	}
	
	/**
	 * Creates a completion, optionally based on a chain of exceptions.
	 * @param depth The number of chained exceptions attached to the completion, which may be zero.
	 * @return
	 */
	private AcsJCompletion internalCompletionMethod(int depth) {
		if (depth <= 0) {
			// a completion type that has no exception associated
			return new ACSErrOKAcsJCompletion();
		}
		else if (depth == 1) {
			return new GenericErrorAcsJCompletion();
		}
		else {
			try {
				internalBadMethod(depth - 1);
			} catch (AcsJGenericErrorEx ex) {
				// a completion which takes associated exceptions (and wraps them with a GenericError exception)
				return new GenericErrorAcsJCompletion(ex);
			}
		}
		// this should never happen, given that for depth>0 we always get an exception from internalBadMethod.
		// We throw an unchecked exception to give a real-world example for such a case, 
		// because at the Java impl / CORBA boundary it is often not good enough to just consider checked exceptions.
		throw new RuntimeException("Program error: was never supposed to get here!");
	}


	@Override
	public void sleepingCmd(short nbSeconds) {
		try {
			Thread.sleep(nbSeconds * 1000);
		}
		catch (InterruptedException ex) {
			m_logger.warning("Woke up early from an InterruptedException");
		}
	}

}
