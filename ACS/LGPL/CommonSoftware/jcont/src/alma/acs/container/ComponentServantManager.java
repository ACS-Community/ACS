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
package alma.acs.container;

import java.util.logging.Logger;

import org.omg.CORBA.LocalObject;
import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.PortableServer.ForwardRequest;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.ServantActivator;

/**
 * We use one ComponentServantManager per component POA.
 * It's used to synchronize with component etherealization.
 * <p>
 * Impl note: until ACS 6.0.x this class inherited from <code>org.omg.PortableServer.ServantActivatorPOA</code>
 * and was activated like a normal Corba object (<code>componentPOA.set_servant_manager(servantManager._this(m_orb))</code>. 
 * However this form of activation attached the servant manager instance to the ORB or root poa, with the effect
 * that it was not garbage collected together with the component poa (memory leak!).
 * It seems that a POJO (no Corba activation) inheriting from <code>LocalObject</code> is the correct choice instead.
 * 
 * @author hsommer
 * $Id$
 */
public class ComponentServantManager extends LocalObject implements ServantActivator // extends ServantActivatorPOA 
{
	private Logger m_logger;
	private boolean DEBUG = false;
	
	private volatile boolean receivedEtherealizeCall;
	
	
	/**
	 * Constructor for ComponentServantManager.
	 * @param logger Logger to be used by this class. Should be the container logger.
	 */
	public ComponentServantManager(Logger logger)
	{
		super();
		m_logger = logger;
	}

	/**
	 * This method should never be called, because all components are activated explicitly by the container
	 * and therefore should be registered in the active object map.
	 * Thus the implementation just throws a <code>OBJECT_NOT_EXIST</code> exception.
	 * <p>
	 * Note that by definition, this method acts as a fallback, 
	 * if the POA can not find an object in that map (if RETAIN policy is used).
	 * The POA could call it after a component has been deactivated (<code>deactivate_object</code>),
	 * in an attempt to serve a new incoming call. This we don't allow though. 
	 * 
	 * @see org.omg.PortableServer.ServantActivatorOperations#incarnate(byte[], POA)
	 */
	public Servant incarnate(byte[] oid, POA adapter) 
		throws ForwardRequest
	{		
		throw new OBJECT_NOT_EXIST();
	}

    /**
     * See CORBA spec (2.4) 11.3.5.2 etherealize.
	 * This operation is invoked whenever a servant for an object is deactivated, assuming the POA has 
	 * the USE_SERVANT_MANAGER and RETAIN policies.
	 * <p>
	 * This method does not deal with the servant (component) at all, just notifies a thread that has called
	 * {@link #waitForEtherealize(int)}.
	 * 
	 * @param oid object Id associated with the object being deactivated.
	 * @param adapter object reference for the POA in which the object was active.
	 * @param serv contains reference to the servant associated with the object being deactivated.
	 * @param cleanup_in_progress if TRUE indicates that destroy or deactivate is called with 
	 *        etherealize_objects param of TRUE.  FALSE indicates that etherealize was called due to other reasons.
	 *        We ignore this parameter.
	 * @param remaining_activations indicates whether the Servant Manager can destroy a servant.  
	 * 	      If set to TRUE, the Servant Manager should wait until all invocations in progress have completed.
	 *        This method seems never to be called with <code>remaining_activations==true</code>. If so, the call is ignored.
	 * 
	 * @see org.omg.PortableServer.ServantActivatorOperations#etherealize(byte[], POA, Servant, boolean, boolean)
	 */
	public synchronized void etherealize(byte[] oid, POA adapter, Servant serv, 
										boolean cleanup_in_progress, boolean remaining_activations)
	{
		if (DEBUG) {
			m_logger.info("ComponentServantManager#etherealize called for " +  
					"servant class = '" + serv.getClass().getName() + "'; rem.actions=" + remaining_activations);
		}
		if (!remaining_activations) {
			if (DEBUG) {
				logStackTrace("Callstack for ServantActivator#etherealize");
			}
			// release thread that blocks on waitForEtherealize
			receivedEtherealizeCall = true;
			notifyAll();
		}
	}


	/**
	 * Resets the flag that gets raised when the <code>etherealize</code> method gets called.
	 * Such a flag is needed because servant etherealization occurs asynchonously some time after 
	 * POA.destroy has been called. 
	 * A thread that wants to wait for etherealization must first call <code>resetWaitForEtherealize</code>,
	 * then <code>POA.destroy</code>, and then <code>waitForEtherealize</code>.
	 * 
	 * @see #waitForEtherealize(int)
	 */
	public synchronized void resetWaitForEtherealize() {
		receivedEtherealizeCall = false;
	}
	
	/**
	 * Allows a thread to be notified of component servant etherealization.
	 * <p>
	 * Since we use one component POA per servant, it is not necessary to distinguish for which servant 
	 * the etherealize method was called.
	 * 
	 * @param maxWaitMillis  the maximum time to wait, or 0 if no timeout should be used.
	 * @return true if etherealization occured, false if the operation timed out.
	 */
	public synchronized boolean waitForEtherealize(int maxWaitMillis) {
		if (DEBUG) {
			m_logger.info("will wait for component etherealization...");
		}
		long startedWaitingTime = System.currentTimeMillis();
		long remainingWaitTime = maxWaitMillis;
		while (!receivedEtherealizeCall && remainingWaitTime > 0) {
			try {
				wait(remainingWaitTime);
			} catch (InterruptedException e) {
				// just wait again
			}
			remainingWaitTime = maxWaitMillis - (System.currentTimeMillis() - startedWaitingTime);
		}
		if (!receivedEtherealizeCall) {
			// timeout, no success
//			m_logger.warning("Waiting for component to be etherealized timed out after " + maxWaitMillis + "ms.");
		}
		else if (DEBUG) {
			m_logger.info("received component etherealization notification.");
		}
		return receivedEtherealizeCall;
	}

	
//	/**
//	 * @see org.omg.CORBA.portable.InvokeHandler#_invoke(String, InputStream, ResponseHandler)
//	 */
//	public OutputStream _invoke(String method, InputStream input, ResponseHandler handler)
//		throws SystemException
//	{
//		OutputStream os = super._invoke(method, input, handler);
//		m_logger.fine("ComponentServantManager#_invoke called.");
//		return os;
//	}

	
	/**
	 * Helper method for debugging, logs the stacktrace.
	 * @param msg
	 */
	private void logStackTrace(String msg) {
		if (DEBUG) {
			Exception fakeEx = new Exception("stacktrace fake ex");			
			StackTraceElement[] trace = fakeEx.getStackTrace();
			msg += "\n";
			for (int i=1; i < trace.length; i++) {
			    msg += "\tat " + trace[i] + '\n';
			}
			m_logger.info(msg);
		}
	}



}
