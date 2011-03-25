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
package alma.acs.monitoring;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.logging.Logger;

/**
 * Dynamic interceptor, to be used for example to log or modify calls to a corba object like the CDB or a Java component.
 * <p>
 * This class has been extracted and generalized from <code>alma.acs.container.ContainerSealant</code> in module jcont, 
 * to allow reusing the code for intercepting other calls as well.
 * 
 * @author hsommer
 * @since ACS 9.0
 */
public class DynamicInterceptor implements InvocationHandler
{
	private final Object delegate;
	private final Logger logger;
	private final ClassLoader contextCL;
	private final InterceptionHandlerFactory interceptionHandlerFactory;
	
	/**
	 * To be called only from the proxy factory method 
	 * {@link #createDynamicInterceptor(Class, Object, Logger, ClassLoader, InterceptionHandlerFactory)}
	 */
	protected DynamicInterceptor(Object delegate, Logger logger, ClassLoader contextCL, InterceptionHandlerFactory interceptionHandlerFactory) {
		this.delegate = delegate;
		this.logger = logger;
		this.contextCL = contextCL;
		this.interceptionHandlerFactory = interceptionHandlerFactory;
	}


	/**
	 * Creates a <code>DynamicInterceptor</code> instance and uses it as the invocation handler for the returned dynamic proxy
	 * which implements <code>dynInterface</code>.
	 * <p>
	 * Note that if we want to allow a chain of handlers for each call, e.g. to more cleanly separate logging from permission checks,
	 * then we should change this method to accept a <code>List&lt;InterceptionHandlerFactory&gt;</code>, to create many
	 * <code>InterceptionHandler</code> objects for each call. These handlers must then be chained together in a way that the call aborts after the first 
	 * handler's <code>callReceived</code> has returned <code>false</code>, and the return value of the first handler's <code>callFinished</code>
	 * is fed to the second handler as the <code>retVal</code> argument.
	 * 
	 * @param dynInterface
	 * @param delegate      The delegation object.
	 *                      <br>
	 *                      Note about usage of java generics: Ideally this delegate would be declared "T" instead of "Object", 
	 *                      but did not get it to work with that...
	 * @param logger        The Logger to be used by this class.
	 * @param contextCL     The class loader to be associated with the current thread during the forwarding of the call to the delegate object,
	 *                      or <code>null</code> if the interceptor should not swap the classloader.
	 * @param interceptionHandlerFactory  Will be used to create the callback object (one per invocation).
	 */
	public static <T> T createDynamicInterceptor(Class<T> dynInterface, Object delegate, Logger logger, 
			ClassLoader contextCL, InterceptionHandlerFactory interceptionHandlerFactory)
	{
		DynamicInterceptor invHandler = new DynamicInterceptor(delegate, logger, contextCL, interceptionHandlerFactory);
		
		T proxy = (T) Proxy.newProxyInstance(dynInterface.getClassLoader(),
														new Class<?>[] { dynInterface },
														invHandler);
		return proxy;
	}


	/**
	 * Receives the intercepted calls, and forwards them to the handler and delegate object.
	 * Also sets the class loader passed to {@link #createDynamicInterceptor(Class, Object, Logger, ClassLoader, InterceptionHandlerFactory)} 
	 * as the current thread's context class loader (see <code>Thread.currentThread().getContextClassLoader()</code>).
	 * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
	 */
	public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
	{
		InterceptionHandler handler = interceptionHandlerFactory.createInterceptionHandler();
		
		boolean allowCall = handler.callReceived(method, args);
		
		Object retObj = null;
		Throwable realThr = null;
		
		if (allowCall) {
			try {
				ClassLoader oldContCL = Thread.currentThread().getContextClassLoader();
				if (contextCL != null) {
					Thread.currentThread().setContextClassLoader(contextCL);
				}
				try {
					// forward the call to the delegate object
					retObj = method.invoke(delegate, args);
				} finally {
					Thread.currentThread().setContextClassLoader(oldContCL);
				}
			} catch (Throwable thr) {
				realThr = unwindThrowableHierarchy(thr);
			}
		}
		else {
			logger.fine("Rejected call to method '" + method.getName() + "' as advised by handler " + handler.getClass().getName());
		}
		
		Object retObj2 = handler.callFinished(retObj, args, realThr);
		return retObj2;
	}


	/**
	 * Pops <code>InvocationTargetException</code>s and <code>UndeclaredThrowableException</code>s
	 * off the exception cause stack, until a "real" exception is found.
	 * @param thr
	 * @return
	 */
	private Throwable unwindThrowableHierarchy(Throwable thr) {
		Throwable realThr = thr;
		if (thr instanceof InvocationTargetException || thr instanceof UndeclaredThrowableException) {
			if (thr.getCause() != null) { // should always be true...
				realThr = thr.getCause();
				return unwindThrowableHierarchy(realThr);
			}
		}
		return realThr;
	}

	
	/**
	 * Users of the {@link DynamicInterceptor} class must provide a {@link InterceptionHandlerFactory}
	 * which will be used to create an instance of this {@link InterceptionHandler} for every intercepted call.
	 * This instance is then used to notify the user before and after the actual call to the delegate object.
	 * <p>
	 * (If we make this interface into an abstract base class, we should move the arguments of "callReceived"
	 * to the constructor, since there is only one method invoked during the lifetime of an InterceptionHandler).
	 */
	public static interface InterceptionHandler {
		/**
		 * Notification that a call to the {@link DynamicInterceptor#delegate} has been intercepted, 
		 * but the call has not yet been forwarded.
		 * @param method  The name of the interface method that has been called.
		 * @param args    The call arguments, see {@link InvocationHandler#invoke(Object, Method, Object[])}.
		 * @return <code>true</code> to continue with forwarding the call to the delegate object; 
		 *         <code>false</code> to prohibit the call to the delegate object
		 *         (in which case {@link #callFinished(Object, Throwable)} must provide the return value or exception).
		 */
		public boolean callReceived(Method method, Object[] args);
		
		/**
		 * @param retVal  The return value received from the delegate object.
		 * @param args    The call arguments (same as in <code>callReceived</code>,
		 *                but possibly with values modified during the call, e.g. for Corba out or inout parameters). 
		 * @param thr     The Throwable that was thrown by the delegate's method implementation, if any.
		 *                Note that wrapper exceptions {@link InvocationTargetException} and {@link UndeclaredThrowableException} 
		 *                get removed automatically, so that <code>thr</code> is the original exception thrown, 
		 *                or <code>null</code> if no exception/error was thrown.
		 * @return The value that should be returned by the interceptor (should be <code>retVal</code>
		 *         unless the interceptor wants to modify the return value). 
		 * @throws Throwable  The throwable that should be forwarded to the calling client. Normally this should be the same
		 *                    as the argument <code>thr</code> (which then <b>must</b> be thrown by the implementation of this method!), 
		 *                    but the handler is free to throw an exception even if the  
		 *                    delegate object did not throw any, or to throw a different exception, or to suppress the exception.
		 */
		public Object callFinished(Object retVal, Object[] args, Throwable thr) throws Throwable;
	}
	
	/**
	 * We use a factory to allow for the convenient (though slightly less performant) design 
	 * of grouping the methods for notification before and after the intercepted call is performed
	 * into a single dedicated {@link InterceptionHandler} object. 
	 * Then even in a multithreaded environment the "after" can be matched to the "before" easily.
	 */
	public static interface InterceptionHandlerFactory {
		/**
		 * Creates an InterceptionHandler instance to be used for a single call.
		 */
		public InterceptionHandler createInterceptionHandler();
	}
	
}
