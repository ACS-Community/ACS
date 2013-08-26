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

import java.lang.reflect.Method;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.monitoring.DynamicInterceptor.InterceptionHandler;
import alma.acs.monitoring.DynamicInterceptor.InterceptionHandlerFactory;
import alma.acs.util.StopWatch;



/**
 * Uses {@link DynamicInterceptor} to intercept and log calls to an interface.
 * @author hsommer
 */
public class SimpleCallInterceptor
{

	/**
	 * Calls {@link DynamicInterceptor#createDynamicInterceptor(Class, Object, Logger, ClassLoader, InterceptionHandlerFactory)},
	 * using {@link SimpleInterceptionHandler} objects to handle (log) the intercepted calls.
	 */
	public static <T> T createSimpleInterceptor(Class<T> corbaInterface, T delegate, Logger logger)
	{
		InterceptionHandlerFactory interceptionHandlerFactory = 
			new SimpleInterceptionHandlerFactory(logger);

		return DynamicInterceptor.createDynamicInterceptor(corbaInterface, delegate, logger, null, interceptionHandlerFactory);
	}

	
	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////// Callbacks for Dynamic Interceptor //////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * The handler created for every call to the DAL or DAO (with lightweight implementation).
	 * Will log method invocation and return.
	 */
	private static class SimpleInterceptionHandler implements DynamicInterceptor.InterceptionHandler {
		private final Logger logger;
		private final Level logLevel = Level.FINER;
		private StopWatch methodInvWatch;
		
		/**
		 * Whether the call should be logged (because of level or suppressed logging for the given interface method).
		 */
		private boolean isLoggable = false;
		
		/**
		 * The method that was called
		 */
		private Method method;

		
		SimpleInterceptionHandler(Logger logger) {
			this.logger = logger;
		}
		
		@Override
		public boolean callReceived(Method method, Object[] args) {
			this.method = method;
			isLoggable = logger.isLoggable(logLevel);
			if (isLoggable) {
				methodInvWatch = new StopWatch(logger);
				logger.log(logLevel, "intercepted a call to '" + method.getName() + "'.");
			}
			return true;
		}

		@Override
		public Object callFinished(Object retVal, Object[] args, Throwable realThr) throws Throwable {
			
			// log invocation time
			if (isLoggable) {
				String msg = "returning from " + method.getName() + " after " + methodInvWatch.getLapTimeMillis() + " ms. ";
				if (realThr != null) {
					msg += realThr.getClass().getSimpleName() + " was thrown."; // TODO log more exception details if needed.
				}
				logger.log(logLevel, msg);
			}

			if (realThr != null) {
				throw realThr;
			}
			
			return retVal;
		}
	}

	
	/**
	 * 
	 */
	private static class SimpleInterceptionHandlerFactory implements DynamicInterceptor.InterceptionHandlerFactory {
		private final Logger logger;

		SimpleInterceptionHandlerFactory(Logger logger) {
			this.logger = logger;
		}

		@Override
		public InterceptionHandler createInterceptionHandler() {
			return new SimpleInterceptionHandler(logger);
		}
	}

}
