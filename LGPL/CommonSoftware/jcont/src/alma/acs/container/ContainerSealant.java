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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.DATA_CONVERSION;
import org.omg.CORBA.UserException;

import alma.acs.component.dynwrapper.DynWrapperException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.util.StopWatch;

/**
 * Seals the container to make it a "tight container", as opposed to an "open container".
 * There's one sealant instance per component instance. The sealant is placed between the CORBA POATie 
 * on the one side, and the component implementation class or the component interface translator
 * (if present) on the other side. The sealant therefore intercepts all functional calls made
 * to the component.
 * <p>
 * This sealant class is not only used for components, but also for offshoots from that component,
 * if they follow the tie-approach. 
 * <p>
 * Todo: let listeners register, and notify them of any intercepted call; 
 * they may either veto down the invocation, or simply trace the call.
 * Any logging, security checks etc. should be done by such listeners in the future.
 * 
 * @author hsommer
 */
public class ContainerSealant implements InvocationHandler
{
	private final String m_name;
	private final Object m_component;
	private final Logger m_logger;
	private final ClassLoader componentContextCL;    
	private final Set<String> methodNamesExcludedFromInvocationLogging;
    
	private ContainerSealant(Object componentImpl, String name, 
								Logger logger, 
								ClassLoader componentContextCL ) 
	{
		m_component = componentImpl;
		m_name = name;
		m_logger = logger;
        this.componentContextCL = componentContextCL; 
        
        methodNamesExcludedFromInvocationLogging = new HashSet<String>(2);
	}


	/**
     * Creates a ContainerSealant and uses it as the invocation handler for the returned dynamic proxy
     * which implements <code>corbaInterface</code>.
     * 
	 * @param corbaInterface  the interface that the created sealant will implement;
	 *                         this should be the component's xxxOperations interface.
	 * @param component  the component implementation class, or any translator class
	 *                    in front of the component implementation.
	 *                    <code>componentImpl</code> must implement <code>corbaInterface</code>
	 *                    so that the sealant can forward calls to the component.
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
	static Object createContainerSealant(Class corbaInterface, Object component, String name, boolean isOffShoot,
            Logger logger, ClassLoader componentContextCL, String[] methodNamesExcludedFromInvocationLogging )
			throws ContainerException
	{
		if (!corbaInterface.isInstance(component))
		{
			throw new ContainerException("sealant factory: component " + component.getClass().getName() 
			+ " must implement the sealant interface " + corbaInterface.getClass().getName());
		}
		
		ContainerSealant invHandler = new ContainerSealant(component, name, logger, componentContextCL);
		try {
			invHandler.setMethodsExcludedFromInvocationLogging(methodNamesExcludedFromInvocationLogging, isOffShoot);
		} catch (Exception ex) {
			// logging a warning is enough (error will just result in unwanted future logging and thus does not warrant a failure here).
			logger.log(Level.WARNING, "failed to exclude certain methods of '" + name + 
						"' from future automatic invocation logging.", ex);
		}
		
		Object proxy = Proxy.newProxyInstance(corbaInterface.getClassLoader(),
															new Class[] { corbaInterface },
															invHandler);
		return proxy;	
	}	
			
    
	/**
	 * Receives functional calls to the component. 
     * <ol>
	 * <li>Intercepts them to produce logs before and after the call.
     * <li>Sets the component classloader as the current thread's context classloader
     *     so that components can use <code>Thread.currentThread().getContextClassLoader()</code>. <br>
	 * </ol>
	 * todo: add security and other container features here, especially check the component state!!!
	 * <p> 
	 * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
	 */
	public Object invoke(Object proxy, Method method, Object[] args)
		throws Throwable
	{
		StopWatch methodInvWatch = new StopWatch(m_logger);

		String qualMethodName = m_name + "#" + method.getName();
		if (! isExcludedFromInvocationLogging(method.getName())) {
			m_logger.log(AcsLogLevel.DEBUG, "intercepted a call to '" + qualMethodName + "'.");	    
		}
		
		Object retObj = null;
		try
		{
            ClassLoader oldContCL = Thread.currentThread().getContextClassLoader();
            try {
                Thread.currentThread().setContextClassLoader(componentContextCL);
                // call to the component implementation or the transparent-xml layer above it
                retObj = method.invoke(m_component, args);
            } finally {
                Thread.currentThread().setContextClassLoader(oldContCL);                                
            }
			
			if (! isExcludedFromInvocationLogging(method.getName())) {
				m_logger.log(AcsLogLevel.DEBUG, "returning from " + qualMethodName + " after " + 
					methodInvWatch.getLapTimeMillis() + " ms.");
			}
		}
		catch (Throwable thr)
		{
			Throwable realThr = unwindThrowableHierarchy(thr);
			
			if (realThr instanceof UserException)
			{
				// should have been declared in IDL, but better check!
				boolean declared = false;
				Class[] declaredExceptions = method.getExceptionTypes();
				for (int i = 0; i < declaredExceptions.length; i++) {
					if (declaredExceptions[i] == realThr.getClass()) {
						declared = true;
						break;
					}
				}
				String msg = ( declared 
								? "checked exception " 
								: "unchecked user exception (should have been declared in IDL!) " );
				msg += "was thrown in functional method '" + qualMethodName + "':";
				Level logLevel = ( declared ? AcsLogLevel.DEBUG : Level.WARNING );
				m_logger.log(logLevel, msg, realThr);
				throw realThr;				
			} 
//			// TODO perhaps support ACS error handling  
//			else if (realThr instanceof AcsJException) {...}			
			else if (realThr instanceof DynWrapperException)
			{
				String msg = "the dynamic xml entity translator failed with the functional method '" + 
				qualMethodName + "': ";
				m_logger.log(Level.SEVERE, msg, realThr);
				// we slightly extend the foreseen usage of this CORBA system exception, which is
				// "raised if an ORB cannot convert the representation of data 
				//  as marshaled into its native representation or vice-versa"
				throw new DATA_CONVERSION(msg + realThr.toString());
			}
			else
			{
				m_logger.log(Level.WARNING, "unexpected exception was thrown in functional method '" + 
					qualMethodName + "': ", realThr);
				throw realThr;				
			}
		}
		
		return retObj;
	}

		
	/**
     * Checks if an invoked method is known to be excluded from logging.
     * This is always true for {@link ACSComponentOperations#componentState() componentState},
     * and for all methods that are set for no-logging.
     */
    private boolean isExcludedFromInvocationLogging(String methodName) {
        return (methodNamesExcludedFromInvocationLogging.contains(methodName));
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
	 * Disables automatic invocation logging for certain component or offshoot methods.
	 * <p>
	 * The method {@link ACSComponentOperations#componentState() componentState} gets always excluded,
	 * independently of <code>excludedMethods</code>.
	 * It is a hard-coded special case.
	 * <p>
	 * For components, the method name must be given directly. 
	 * For offshoots, method names must be qualified, see comment for  
	 * {@link ComponentHelper#_getComponentMethodsExcludedFromInvocationLogging() ComponentHelper}
	 * where component developers can specify these strings.
	 * <p>
	 * TODO: check if calls to JBaci properties should be also excluded hard-coded.
	 * At the moment the component helper class must specify them explicitly, 
	 * e.g. "OFFSHOOT::alma.ACS.ROstringSeq#get_sync". 
	 * This is not intuitive because JBaci properties are not visibly offshoots for the user, 
	 * this is only an ACS implementation choice. 
	 * <p> 
	 * @param excludedMethods the names of methods to exclude from automatic logging. 
	 * @param isOffshoot true if the sealed object is an offshoot, as opposed to a component.
	 * @see ComponentHelper#getComponentMethodsExcludedFromInvocationLogging()
	 */
	void setMethodsExcludedFromInvocationLogging(String[] excludedMethods, boolean isOffshoot) {
        // see comment for method 'isExcludedFromInvocationLogging'
		methodNamesExcludedFromInvocationLogging.add("componentState");		

		if (excludedMethods == null) {
			return;
		}
		
		if (isOffshoot) {
			// strip off component name from qualified offshoot name
			String actualInterfaceName = m_name.substring(m_name.lastIndexOf('/')+1);
			
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
		if (m_logger.isLoggable(Level.FINE) && !methodNamesExcludedFromInvocationLogging.isEmpty()) {
			StringBuffer buff = new StringBuffer(100);
			buff.append("Container will not log invocations of the following methods of ");
			buff.append(isOffshoot ? "offshoot '" : "component '");
			buff.append(m_name).append("' :");
			for (Iterator iter = methodNamesExcludedFromInvocationLogging.iterator(); iter.hasNext();) {				
				String methodName = (String) iter.next();
				buff.append(methodName);
				if (iter.hasNext()) {
					buff.append(", ");
				}
			}
			m_logger.fine(buff.toString());
		}
	}

}
