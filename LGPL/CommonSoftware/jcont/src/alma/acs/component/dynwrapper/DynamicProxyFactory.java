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
package alma.acs.component.dynwrapper;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.container.ComponentHelper;
import alma.acs.util.StopWatch;


/**
 * @author hsommer Nov 22, 2002 4:48:25 PM
 * $Id$
 */
public class DynamicProxyFactory
{
	private static DynamicProxyFactory s_instance;
	
	/**
	 * Singleton accessor.
	 * The logger must be provided because this class is used both inside the container as well
	 * as outside in tests and applications that are clients to container/components.
	 * The latter cases might not want to use central ACS logging.
	 *  
	 * @param logger
	 * @return DynamicProxyFactory
	 */
	public static DynamicProxyFactory getDynamicProxyFactory(Logger logger)
	{
		if (s_instance == null)
		{
			s_instance = new DynamicProxyFactory(logger);
		}
		return s_instance;
	}
	
	private Logger m_logger = null;
	
	private DynamicProxyFactory(Logger logger)
	{
		m_logger = logger;
	}
	
	
	/**
	 * Creates a proxy object for a corba stub for a component. 
	 * The proxy object will delegate calls to the corba stub, and will translate parameters as necessary.
	 *   
	 * @param componentInterface  the component interface
	 * @param corbaStub (should be a Corba object, but this is currently not enforced)
	 * @return Object
	 * @throws DynWrapperException
	 */
	public <T, F> T createClientProxy(Class<T> componentInterface, 
											F corbaStub, 
											Class<F> corbaOperationsIF)
			throws DynWrapperException
	{
		T proxy = null;
		
		try {
			ComponentInvocationHandler handler = 
				new ComponentInvocationHandler(corbaStub, corbaOperationsIF, m_logger);

			// TODO: use the result
			checkMethodMatching(componentInterface, handler);

			proxy = componentInterface.cast(Proxy.newProxyInstance(componentInterface.getClassLoader(),
																new Class[] { componentInterface },
																handler));
		}
		catch (DynWrapperException e) {
			throw e;
		}		
		catch (RuntimeException e2) {
			String msg = "failed to create a binding-class aware client proxy implementing " + componentInterface.getName();
			m_logger.log(Level.SEVERE, msg, e2);
			throw new DynWrapperException(msg, e2);
		}
		return proxy;
	}
	
	
	
	/**
	 * Creates a proxy object for a component implementation. 
	 * The proxy object will delegate calls to the component, and will translate parameters as necessary.
	 *
	 * @param corbaIF  the IDL-generated xxOperations interface.
	 * @param componentImpl  the component implementation class that implements <code>componentIF</code>. 
	 *                        This may be identical with <code>corbaIF</code>, or could be 
	 *                        some other similar interface, e.g. one that uses XML binding classes.
	 * @param componentIF  the interface of the component to which calls will be delegated.
	 * @return the proxy object that implements <code>corbaIF</code> and delegates to <code>componentImpl</code>.
	 * @throws DynWrapperException
	 */
	public Object createServerProxy(Class<?> corbaIF, 
											Object componentImpl, 
											Class<?> componentIF)
			throws DynWrapperException
	{
		ComponentInvocationHandler handler = 
			new ComponentInvocationHandler(componentImpl, componentIF, m_logger);

		checkMethodMatching(corbaIF, handler);

		Object proxy = Proxy.newProxyInstance(corbaIF.getClassLoader(),
															new Class[] { corbaIF },
															handler);
		return proxy;
	}
	
	
	
	/**
	 * Checks if all methods in <code>facade</code> can be mapped to a corresponding method 
	 * in <code>delegate</code> using the various mappers for argument translation.
	 * <p>
	 * TODO: make this check optional (property etc.) since it's not needed for stable
	 * and already verified interfaces. Perhaps move from runtime to buildtime?
	 * 
	 * @param facade  the interface that is presented to the client 
	 * 			(serverside: the container; clientside: the client application).
	 * @param invHandler  the invocation handler that can delegate the facade method calls
	 * 			(serverside: to the component implementation; clientside: to the CORBA stub).
	 * @return  true if all parameters can be mapped automatically both ways.
	 *           false if at least one pair of parameters requires manual mapping.
	 * @throws DynWrapperException if <code>delegate</code> does not provide 
	 *          a peer method for at least one method in <code>facade</code>.
	 *          Currently two methods are considered peers if they have the same name and the same
	 *          number of parameters. 
	 */	
	@SuppressWarnings("unchecked")
	private boolean checkMethodMatching(Class facade, ComponentInvocationHandler invHandler)
		throws DynWrapperException
	{
		StopWatch methodMatchCheckTimer = new StopWatch(m_logger); 
		
		boolean autonomousMapping = true;		
		
		Method[] facadeMethods = facade.getMethods();
		for (int i = 0; i < facadeMethods.length; i++)
		{	
			Method delegateMethod = invHandler.findDelegateMethod(facadeMethods[i]);
			Class[] facadeParams = facadeMethods[i].getParameterTypes();
			Class[] delegParams = delegateMethod.getParameterTypes();
			// number of parameters must be equal
			if (facadeParams.length != delegParams.length)
			{
				String msg = "unmatching numbers of parameters in method " + delegateMethod.getName();
				throw new DynWrapperException(msg);
			}
			// check calling parameters
			for (int pIndex = 0; pIndex < facadeParams.length; pIndex++)
			{
				// todo: use in/inout/out info from IR for more specific check; currently assumes worst case 'inout'
				if (!invHandler.canTranslate(facadeParams[pIndex], delegParams[pIndex]) 
				    || !invHandler.canTranslate(delegParams[pIndex], facadeParams[pIndex]) )
				{
					String msg = "unable to map automatically between parameter type '" + facadeParams[pIndex].getName() +
									 "' and '" + delegParams[pIndex].getName() + "' in method '" + 
									 facadeMethods[i].getName() + "'. This functionality must therefore be provided " +
									 " by the component's associated " + ComponentHelper.class.getName() + " class.";
					m_logger.info(msg);
					autonomousMapping = false;
				}
			}
			// check return value
			Class delegateRet = delegateMethod.getReturnType();
			Class facadeRet = facadeMethods[i].getReturnType();
			if (!invHandler.canTranslate(delegateRet, facadeRet))
			{
				String msg = "unable to map automatically from return type '" + delegateRet.getName() +
								 "' to type '" + facadeRet.getName() + "' in method '" + 
								 facadeMethods[i].getName() + "'.";
				m_logger.info(msg);
				autonomousMapping = false;
			}
		}
		
		methodMatchCheckTimer.logLapTime("verify automatic translation for methods in " + facade.getName());
		
		return autonomousMapping;
	}
	
}
