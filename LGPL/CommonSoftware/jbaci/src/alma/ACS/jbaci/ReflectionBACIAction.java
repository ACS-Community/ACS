/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.ACS.jbaci;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import alma.ACS.CBDescIn;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACSErrTypeCommon.wrappers.AcsJUnknownEx;
import alma.acs.exceptions.AcsJException;

/**
 * BACI action using reflection to invoke BACI method.
 * This class simplifies usage of <code>BACIAction</code>, there is no need
 * to extend the class and implement <code>execute()</code> method.
 * Called metod has to have the following signature: "public void <name>() throws AcsJException;".
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class ReflectionBACIAction extends BACIAction {

	/**
	 * Object on which to invoke the method.
	 */
	protected Object invokee;
	
	/**
	 * Method to be invoked.
	 */
	protected Method method;

	/**
	 * Constructor of NORMAL priority action (CBvoid callback).
	 * @param executor	executor to be used to execute action.
	 * @param invokee	object on which to invoke the method
	 * @param method	method to be invoked.
	 * @param callback	action callback.
	 * @param descIn	action callback in descriptor.
	 */
	public ReflectionBACIAction(
							PrioritizedExecutor executor,
							Object invokee, Method method,
							CBvoid callback, CBDescIn descIn)
	{
		this(executor, invokee, method, callback, descIn, (CallbackDispatcher)null);
	}
	
	/**
	 * Constructor of NORMAL priority action.
	 * @param executor	executor to be used to execute action.
	 * @param invokee	object on which to invoke the method
	 * @param method	method to be invoked.
	 * @param callback	action callback.
	 * @param descIn	action callback in descriptor.
	 * @param callbackDispatcher	callback dispatcher (value dependend).
	 */
	public ReflectionBACIAction(
					  PrioritizedExecutor executor,
					  Object invokee, Method method,
					  Callback callback, CBDescIn descIn,
					  CallbackDispatcher callbackDispatcher)
	{
		this(executor, invokee, method, callback, descIn, callbackDispatcher, BACIPriority.NORMAL);
	}

	/**
	 * Constructor.
	 * @param executor	executor to be used to execute action.
	 * @param invokee	object on which to invoke the method
	 * @param method	method to be invoked.
	 * @param callback	action callback.
	 * @param descIn	action in descriptor.
	 * @param priority	action priority.
	 */
	public ReflectionBACIAction(
							PrioritizedExecutor executor,
							Object invokee, Method method,
							CBvoid callback, CBDescIn descIn, BACIPriority priority)
	{
		this(executor, invokee, method, callback, descIn, null, priority);
	}

	/**
	 * Constructor.
	 * @param executor	executor to be used to execute action.
	 * @param invokee	object on which to invoke the method
	 * @param method	method to be invoked.
	 * @param callback	action callback.
	 * @param descIn	action in descriptor.
	 * @param callbackDispatcher	callback dispatcher (value dependend).
	 * @param priority	action priority.
	 */
	public ReflectionBACIAction(
					  PrioritizedExecutor executor,
					  Object invokee, Method method,
					  Callback callback, CBDescIn descIn, 
					  CallbackDispatcher callbackDispatcher, BACIPriority priority)
	{
		super(executor, callback, descIn, callbackDispatcher, priority);
		this.invokee = invokee;
		this.method = method;
	}

	/**
	 * Constructor of NORMAL priority action (CBvoid callback).
	 * @param executor	executor to be used to execute action.
	 * @param invokee	object on which to invoke the method
	 * @param invokeeClass <code>Class</code> of the invokeee instance.
	 * @param methodName name of the method.
	 * @param callback	action callback.
	 * @param descIn	action callback in descriptor.
	 */
	public ReflectionBACIAction(
							PrioritizedExecutor executor,
			  				Object invokee, Class invokeeClass, String methodName,
							CBvoid callback, CBDescIn descIn)
	{
		this(executor, invokee, invokeeClass, methodName,
			 callback, descIn, null, BACIPriority.NORMAL);
	}

	/**
	 * Constructor of NORMAL priority action.
	 * @param executor	executor to be used to execute action.
	 * @param invokee	object on which to invoke the method
	 * @param invokeeClass <code>Class</code> of the invokeee instance.
	 * @param methodName name of the method.
	 * @param callback	action callback.
	 * @param descIn	action callback in descriptor.
	 * @param callbackDispatcher	callback dispatcher (value dependend).
	 */
	public ReflectionBACIAction(
							PrioritizedExecutor executor,
							Object invokee, Class invokeeClass, String methodName,
							Callback callback, CBDescIn descIn,
							CallbackDispatcher callbackDispatcher)
	{
		this(executor, invokee, invokeeClass, methodName,
			 callback, descIn, callbackDispatcher, BACIPriority.NORMAL);
	}

	/**
	 * Constructor of priority action.
	 * @param executor	executor to be used to execute action.
	 * @param invokee	object on which to invoke the method
	 * @param invokeeClass <code>Class</code> of the invokeee instance.
	 * @param methodName name of the method.
	 * @param callback	action callback.
	 * @param descIn	action callback in descriptor.
	 * @param callbackDispatcher	callback dispatcher (value dependend).
	 * @param priority	action priority.
	 */
	public ReflectionBACIAction(
							PrioritizedExecutor executor,
							Object invokee, Class invokeeClass, String methodName,
							Callback callback, CBDescIn descIn,
							CallbackDispatcher callbackDispatcher, BACIPriority priority)
	{
		super(executor, callback, descIn, callbackDispatcher, priority);
		try
		{
			method = invokeeClass.getMethod(methodName, null);
			this.invokee = invokee;
		}
		catch (NoSuchMethodException nsme)
		{
			throw new IllegalArgumentException(nsme.getMessage());
		}
	}

	/**
	 * @see alma.ACS.jbaci.BACIAction#execute()
	 */
	public Object execute() throws AcsJException {
		
		try
		{
			return method.invoke(invokee, null);
		}
		catch (InvocationTargetException ite)
		{
			// unwrap the exception
			Throwable th = ite.getCause();
			if (th instanceof AcsJException)
				// rethrow
				throw (AcsJException)th;
			else
			{
				// wrap exception into AcsJException
				throw new AcsJUnknownEx("Exception caught invoking BACI action method.", th);
			}
		}
		catch (Throwable th)
		{
			// wrap exception into AcsJException
			throw new AcsJUnknownEx("Failed to invoke method.", th);
		}
	}

}
