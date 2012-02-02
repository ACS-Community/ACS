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

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.DATA_CONVERSION;
import org.omg.CORBA.UserException;

import alma.ACS.ACSComponentOperations;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.acs.component.dynwrapper.DynWrapperException;
import alma.acs.container.corba.CorbaNullFinder;
import alma.acs.exceptions.CorbaExceptionConverter;
import alma.acs.logging.AcsLogLevel;
import alma.acs.monitoring.DynamicInterceptor;
import alma.acs.monitoring.DynamicInterceptor.InterceptionHandler;
import alma.acs.monitoring.DynamicInterceptor.InterceptionHandlerFactory;
import alma.acs.util.StopWatch;

/**
 * Seals the container to make it a "tight container", as opposed to an "open container".
 * There's one sealant instance per component instance. The sealant is placed between the CORBA POATie 
 * on the one side, and the component implementation class or the component interface translator
 * (if present) on the other side. The sealant therefore intercepts all functional calls made
 * to the component.
 * <p>
 * This sealant class is not only used for components, but also for offshoots from that component,
 * if they follow the tie-approach. Since ACS 9.0 it uses {@link DynamicInterceptor}.
 * 
 * @author hsommer
 */
public class ContainerSealant
{

	/**
	 * If this property is enabled, the container will check the return value and out/inout parameters 
	 * for illegal null values that will cause exceptions during subsequent corba marshalling.
	 */
	public static final String CHECK_NULLS_CORBA_OUT_PROPERTYNAME = "alma.acs.container.check_nulls_corba_out";
	
	
	/**
	 * Creates a ContainerSealant and uses it as the invocation handler for the returned dynamic proxy
	 * which implements <code>corbaInterface</code>.
	 * 
	 * @param corbaInterface  the interface that the created sealant will implement;
	 *                         this should be the component's or offshoot's xxxOperations interface.
	 * @param component  the component/offshoot implementation class, or any translator class
	 *                    in front of the component implementation.
	 *                    <code>componentImpl</code> must implement <code>corbaInterface</code>
	 *                    so that the sealant can forward calls to the component. <br>
	 *                    Note about usage of java generics: Ideally this would be declared "T" instead of "Object", 
	 *                    but did not get it to work with that...
	 * @param name  the component instance name (used for logging) 
	 * @param isOffShoot true if the <code>component</code> object is actually an offshoot of a component
	 * @param logger  logger to be used by this class 
	 * @param componentContextCL  classloader used for {@link Thread#setContextClassLoader(java.lang.ClassLoader setContextClassLoader)}
	 *        before component method gets invoked. (after the call, the old context CL will be restored.)
	 * @param methodNamesExcludedFromInvocationLogging  
	 * @return  an instance of <code>corbaInterface<code> that intercepts calls to
	 *           <code>componentImpl</code> and forwards them if restrictions allow this.
	 * @throws ContainerException if the given <code>component</code> Object does not implement the given <code>corbaInterface</code>. 
	 */
	public static <T> T createContainerSealant(Class<T> corbaInterface, Object component, String name, boolean isOffShoot,
			Logger logger, ClassLoader componentContextCL, String[] methodNamesExcludedFromInvocationLogging )
			throws AcsJContainerEx
	{
		if (!corbaInterface.isInstance(component)) {
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo("sealant factory: component " + component.getClass().getName() 
			+ " must implement the sealant interface " + corbaInterface.getClass().getName());
			throw ex;
		}
		
		InterceptionHandlerFactory interceptionHandlerFactory = 
			new ComponentInterceptionHandlerFactory(name, isOffShoot, logger, methodNamesExcludedFromInvocationLogging);

		return DynamicInterceptor.createDynamicInterceptor(corbaInterface, component, logger, componentContextCL, interceptionHandlerFactory);
	}

	
	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////// Callbacks for Dynamic Interceptor //////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * The handler create for every call to a component or offshoot (with lightweight implementation).
	 * Will log method invocation and return.
	 */
	private static class ComponentInterceptionHandler implements DynamicInterceptor.InterceptionHandler {
		private final String name;
		private final Logger logger;
		private final Set<String> methodNamesExcludedFromInvocationLogging;
		private StopWatch methodInvWatch;
		
		/**
		 * The level at which we log the intercepted call.
		 */
		private final Level logLevel;
		
		/**
		 * Whether the call should be logged (because of level or suppressed logging for the given interface method).
		 */
		private boolean isLoggable = false;
		
		/**
		 * The method that was called
		 */
		private Method method;

		
		ComponentInterceptionHandler(String name, boolean isOffShoot, Logger logger, Set<String> methodNamesExcludedFromInvocationLogging) {
			this.name = name;
			this.logger = logger;
			this.methodNamesExcludedFromInvocationLogging = methodNamesExcludedFromInvocationLogging;
			this.logLevel = isOffShoot ? AcsLogLevel.TRACE : AcsLogLevel.DEBUG;
		}
		
		@Override
		public boolean callReceived(Method method, Object[] args) {
			this.method = method;
			isLoggable = ( !isExcludedFromInvocationLogging(method.getName()) && logger.isLoggable(logLevel) );
			if (isLoggable) {
				String qualMethodName = name + "#" + method.getName();
				methodInvWatch = new StopWatch(logger);
				logger.log(logLevel, "intercepted a call to '" + qualMethodName + "'.");
			}
			return true;
		}

		@Override
		public Object callFinished(Object retVal, Object[] args, Throwable realThr) throws Throwable {
			
			// log invocation time
			String qualMethodName = name + "#" + method.getName();
			if (isLoggable) {
				logger.log(this.logLevel, "returning from " + qualMethodName + " after " + methodInvWatch.getLapTimeMillis() + " ms.");
			}

			if (realThr != null) {
			
				if (realThr instanceof UserException) {
					// Any thrown Corba user exception should be declared in the IDL, but we better check!
					boolean declared = false;
					Class<?>[] declaredExceptions = method.getExceptionTypes();
					for (int i = 0; i < declaredExceptions.length; i++) {
						if (declaredExceptions[i] == realThr.getClass()) {
							declared = true;
							break;
						}
					}
					String msg = (declared ? "checked exception " : "unchecked user exception (should have been declared in IDL!) ");
					msg += "was thrown in functional method '" + qualMethodName + "':";
					Level exLogLevel = (declared ? AcsLogLevel.DEBUG : Level.WARNING);
					
					// log the exception including an optional embedded ErrorTrace (as chain of AcsJxxx exceptions), see COMP-3654
					Throwable thrExpanded = CorbaExceptionConverter.convertHiddenErrorTrace(realThr);
					logger.log(exLogLevel, msg, thrExpanded);
					
					throw realThr;
				}
				else if (realThr instanceof DynWrapperException) {
					String msg = "the dynamic xml entity translator failed with the functional method '" + qualMethodName + "': ";
					logger.log(Level.SEVERE, msg, realThr);
					// we slightly extend the foreseen usage of this CORBA system exception, which according to the spec should be
					// "raised if an ORB cannot convert the representation of data as marshaled into its native representation or vice-versa"
					throw new DATA_CONVERSION(msg + realThr.toString());
				} 
				else {
					logger.log(Level.WARNING, "unexpected exception was thrown in functional method '" + qualMethodName
							+ "': ", realThr);
					throw realThr;
				}
			}
			
			if (Boolean.getBoolean(CHECK_NULLS_CORBA_OUT_PROPERTYNAME)) {
				try {
					// check return value
					Class<?> clzzRet = method.getReturnType();
					if (!Void.TYPE.equals(clzzRet) && !CorbaNullFinder.isIDLInterfaceClass(clzzRet)) {
						CorbaNullFinder finder = new CorbaNullFinder(retVal);
						if (finder.hasErrors()) {
							List<String> errors = finder.getErrors();
							StringBuilder sb = new StringBuilder();
							for (String errorline : errors) {
								sb.append(errorline).append("\n");
							}
							logger.warning("Illegal null value returned by method " + method.getName() + ":\n" + sb.toString());
						}
					}
					
					// check out or inout parameters
					Class<?>[] argsClasses = method.getParameterTypes();
					StringBuilder sb = new StringBuilder();
					for (int argIndex = 0; argIndex < argsClasses.length; argIndex++) {
						Class<?> clzzOutParam = argsClasses[argIndex];
						if (clzzOutParam.getSimpleName().endsWith("Holder") &&
							!CorbaNullFinder.isIDLInterfaceClass(clzzOutParam) ) {
							CorbaNullFinder finder = new CorbaNullFinder(args[argIndex]);
							if (finder.hasErrors()) {
								List<String> errors = finder.getErrors();
								sb.append("  Parameter " + clzzOutParam.getSimpleName() + ": \n");
								for (String errorline : errors) {
									sb.append("    ").append(errorline).append("\n");
								}
							}
						}
					}
					String paramErrors = sb.toString();
					if (!paramErrors.isEmpty()) {
						logger.warning("Illegal null value in out parameter(s) of method " + method.getName() + ":\n" + paramErrors);
					}
				} catch (Exception ex) {
					logger.log(Level.FINE, "Failed to check returned data for illegal nulls.", ex);
				}
			}
			
			return retVal;
		}
		
		/**
		 * Checks if an invoked method is known to be excluded from logging.
		 * This is always true for {@link ACSComponentOperations#componentState() componentState},
		 * and for all methods that are set for no-logging.
		 */
		private boolean isExcludedFromInvocationLogging(String methodName) {
			return (methodNamesExcludedFromInvocationLogging.contains(methodName));
		}
	}

	
	
	/**
	 * 
	 */
	private static class ComponentInterceptionHandlerFactory implements DynamicInterceptor.InterceptionHandlerFactory {
		private final String name;
		private final boolean isOffShoot;
		private final Logger logger;
		private final Set<String> methodNamesExcludedFromInvocationLogging;

		ComponentInterceptionHandlerFactory(String name, boolean isOffShoot, Logger logger, String[] excludedMethods) {
			this.name = name;
			this.isOffShoot = isOffShoot;
			this.logger = logger;
			methodNamesExcludedFromInvocationLogging = new HashSet<String>(2);
			setExcludedMethods(excludedMethods);
		}

		/**
		 * Processes and stores the excludedMethods (taken out from constructor)
		 */
		private void setExcludedMethods(String[] excludedMethods) {
			// see comment for method 'isExcludedFromInvocationLogging'
			methodNamesExcludedFromInvocationLogging.add("componentState");

			if (excludedMethods != null) {
				try {

					if (isOffShoot) {
						// strip off component name from qualified offshoot name
						String actualInterfaceName = name.substring(name.lastIndexOf('/')+1);

						// we must filter out the qualified method names that apply to our given offshoot class
						// (there may be methods for other offshoots produced by the same component)
						for (int i = 0; i < excludedMethods.length; i++) {
							String qualExclMethodName = excludedMethods[i];
							if (qualExclMethodName.startsWith("OFFSHOOT::")) {
								int methodNamePosition = qualExclMethodName.indexOf('#') + 1;
								if (methodNamePosition > 0) {
									String interfaceName = qualExclMethodName.substring("OFFSHOOT::".length(), methodNamePosition -1);
									if (interfaceName.equals(actualInterfaceName)) {
										String methodName = qualExclMethodName.substring(methodNamePosition);
										methodNamesExcludedFromInvocationLogging.add(methodName);
									}
								}
							}
						}
					}
					else {
						// for components, we get the list of method names directly and don't have to check if they apply
						this.methodNamesExcludedFromInvocationLogging.addAll(Arrays.asList(excludedMethods));
					}

					if (logger.isLoggable(Level.FINE) && !methodNamesExcludedFromInvocationLogging.isEmpty()) {
						StringBuffer buff = new StringBuffer(100);
						buff.append("Container will not log invocations of the following methods of ");
						buff.append(isOffShoot ? "offshoot '" : "component '");
						buff.append(name).append("' :");
						for (Iterator<String> iter = methodNamesExcludedFromInvocationLogging.iterator(); iter.hasNext();) {
							String methodName = iter.next();
							buff.append(methodName);
							if (iter.hasNext()) {
								buff.append(", ");
							}
						}
						logger.fine(buff.toString());
					}

				}
				catch (Exception ex) {
					// logging a warning is enough (error will just result in unwanted future logging and thus does not warrant a failure here).
					logger.log(Level.WARNING, "failed to exclude certain methods of '" + name + 
							"' from future automatic invocation logging.", ex);
				}
			}
		}

		@Override
		public InterceptionHandler createInterceptionHandler() {
			return new ComponentInterceptionHandler(name, isOffShoot, logger, methodNamesExcludedFromInvocationLogging);
		}
	}

}
