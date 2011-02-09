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

import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.PortableServer.Servant;

import alma.ACS.ACSComponentOperations;
import alma.JavaContainerError.wrappers.AcsJJavaComponentHelperEx;
import alma.acs.component.ComponentLifecycle;
import alma.acs.component.dynwrapper.DynamicProxyFactory;
import alma.acs.logging.ClientLogManager;
import alma.maciErrType.wrappers.AcsJComponentCreationEx;



/**
 * Base class for component helper classes that must be provided 
 * by the component developers (for example by copying the template file that gets
 * generated with the <code>COMPONENT_HELPERS=on</code> Makefile setting).
 * <p>
 * The method implementations in this class perform checks on the data returned 
 * from the subclass methods (framework-style design). In that sense, <code>ComponentHelper</code>
 * is part of the container, whereas its subclass belongs to the component.
 * <p>
 * The methods that return a <code>Class</code>
 * object should be implemented like <code>return XxxPOATie.class;</code> so that the absence
 * of this class can be discovered at compile time. 
 * (Therefore don't use <code>classForName</code> which evades compile time checking.) 
 * 
 * @author hsommer
 */
public abstract class ComponentHelper
{
	private ComponentLifecycle m_componentImpl;

	// must be private since any subclass should get a different logger ("forComponent")
	private final Logger m_containerLogger;
	
	protected String componentInstanceName; 
	
	
	/**
     * Subclasses must override and call this constructor ("<code>super(containerLogger)</code>").
	 * @param containerLogger logger used by this base class.
	 */
	public ComponentHelper(Logger containerLogger)
	{
		m_containerLogger = containerLogger;
	}
	
	/**
	 * Allows the container to set the component instance name.
	 * This name is used for the component logger and may also be useful in other ways.
	 * <p>
	 * Note that in a better design this name should be given in the constructor,
	 * but was added here as a separate method to avoid modifying existing component helper classes.   
	 * @param name the component instance name (CURL).
	 * @since ACS 5.0 
	 */
	final void setComponentInstanceName(String name) {
		componentInstanceName = name;
	}
	
	/**
	 * Gets the component logger. 
	 * @return the logger to be used by subclasses if they have to log anything.
	 */
	protected final Logger getComponentLogger() {
		String loggerName = ( componentInstanceName != null ? componentInstanceName : "ComponentHelper" );
		return ClientLogManager.getAcsLogManager().getLoggerForComponent(loggerName);
	}
	
	
	/**
	 * Gets the component implementation. 
	 * Must be the same object that also implements the functional interface
	 * obtained from <code>getInternalInterface</code>. 
	 * 
	 * @return  The component implementation class that implements <code>ComponentLifecycle</code>
	 *           and the functional interface.
 	 * @throws AcsJJavaComponentHelperEx  if the component implementation construction failed or 
 	 *                              if the component does not implement its declared functional interface. 
 	 */
	protected final synchronized ComponentLifecycle getComponentImpl() throws AcsJComponentCreationEx, AcsJJavaComponentHelperEx
	{
		if (m_componentImpl== null)
		{
			Class<?> internalIF = null;
			try {
				m_componentImpl = _createComponentImpl();
				internalIF = getInternalInterface();
			} 
			catch (AcsJComponentCreationEx ex) {
				throw ex;
			}
			catch (AcsJJavaComponentHelperEx ex) {
				throw ex;
			}
			catch (Throwable thr) {
				// the ex type is slightly misleading if an overloaded 'getInternalInterface' throws something other than 
				// the declared AcsJJavaComponentHelperEx
				throw new AcsJComponentCreationEx(thr);
			}
			
			if (m_componentImpl == null) {
				AcsJComponentCreationEx ex = new AcsJComponentCreationEx();
				ex.setReason("_createComponentImpl() returned null.");
				throw ex;
			}

			if (!internalIF.isInstance(m_componentImpl)) {
				AcsJJavaComponentHelperEx ex = new AcsJJavaComponentHelperEx();
				ex.setContextInfo("component impl class '" + m_componentImpl.getClass().getName() + 
						"' does not implement the specified functional IF " + internalIF.getName());
				throw ex;
			}
			else if (!ACSComponentOperations.class.isInstance(m_componentImpl))
			{
				AcsJJavaComponentHelperEx ex = new AcsJJavaComponentHelperEx();
				ex.setContextInfo("component impl class '" + m_componentImpl.getClass().getName() + 
						"' does not implement the mandatory IF " + ACSComponentOperations.class.getName() +
						". Check the IDL interface definition, and add ': ACS::ACSComponent'.");
				throw ex;
			}			
			m_containerLogger.finer("component '" + componentInstanceName + "' (class '" + m_componentImpl.getClass().getName() + "') instantiated.");
		}
		return m_componentImpl;
	}		
	
	/**
	 * Method _createComponentImpl to be provided by subclasses.
	 * Must return the same object that also implements the functional component interface.
	 * 
	 * @return ComponentLifecycle  The component impl class that implements the ComponentLifecycle interface.
	 * @throws AcsJComponentCreationEx  if the component implementation class could not be instantiated
	 */
	abstract protected ComponentLifecycle _createComponentImpl() throws AcsJComponentCreationEx;
	
	
	/**
	 * Gets the <code>Class</code> object for the POA tie skeleton class.
	 * The POA tie class is generated by the IDL compiler and must have
	 * a constructor that takes the operations interface as its only parameter.
	 * 
	 * @return the tie class.
	 */
	final Class<? extends Servant> getPOATieClass() throws AcsJJavaComponentHelperEx
	{	
		Class<? extends Servant> poaTieClass = null;
		try {
			poaTieClass = _getPOATieClass();
		} 
		catch (Throwable thr) {
			AcsJJavaComponentHelperEx ex = new AcsJJavaComponentHelperEx(thr);
			ex.setContextInfo("failed to obtain the POATie class from component helper.");
			throw ex;
		}
		if (poaTieClass == null) {
			AcsJJavaComponentHelperEx ex = new AcsJJavaComponentHelperEx();
			ex.setContextInfo("received null as the POATie class from the component helper.");
			throw ex;
		}
		
		// check inheritance stuff (Servant should be checked by compiler under JDK 1.5, but operations IF is unknown at ACS compile time)
		if (!Servant.class.isAssignableFrom(poaTieClass)) {
			AcsJJavaComponentHelperEx ex = new AcsJJavaComponentHelperEx();
			ex.setContextInfo("received invalid POATie class that is not a subclass of org.omg.PortableServer.Servant");
			throw ex;
		}
		if (!getOperationsInterface().isAssignableFrom(poaTieClass)) {
			AcsJJavaComponentHelperEx ex = new AcsJJavaComponentHelperEx();
			ex.setContextInfo("POATie class '" + poaTieClass + "' does not implement the declared operations interface '" + getOperationsInterface().getName() + "'.");
			throw ex;
		}
		return poaTieClass;
	}
	
	/**
	 * This method must be provided by the component helper class.
	 * It must return the Class of the IDL-generated POA-Tie Servant, as in <br>
	 * <code>return DummyComponentPOATie.class;</code>. 
	 */
	protected abstract Class<? extends Servant> _getPOATieClass();
	
	
	/**
	 * Gets the xxOperations interface as generated by the IDL compiler by calling {@link #_getOperationsInterface()}.
	 * @throws AcsJJavaComponentHelperEx  if the subclass methdod throws something or returns <code>null</code>. 
	 */
	public final Class<? extends ACSComponentOperations> getOperationsInterface() throws AcsJJavaComponentHelperEx
	{
		Class<? extends ACSComponentOperations> opIF = null;
		try {
			opIF = _getOperationsInterface();
		}
		catch (Throwable thr) {
			AcsJJavaComponentHelperEx ex = new AcsJJavaComponentHelperEx(thr);
			ex.setContextInfo("failed to retrieve 'operations' interface from component helper");
			throw ex;
		}
		if (opIF == null) {
			AcsJJavaComponentHelperEx ex = new AcsJJavaComponentHelperEx();
			ex.setContextInfo("received null as the 'operations' IF class from the component helper.");
			throw ex;
		}
		return opIF;
	}
	
	
	/**
	 * Gets the xxOperations interface as generated by the IDL compiler.
	 * 
	 * This method must be provided by the component helper class.
	 * 
	 * @return  the <code>Class</code> object associated with the operations interface.
	 */
	protected abstract Class<? extends ACSComponentOperations> _getOperationsInterface();
	

	/**
	 * Gets the component's implemented functional IF.
	 * For instance, the method signatures can be the same as in the operations interface,
	 * except that the internal interface uses xml binding classes
	 * where the IDL-generated operations interface only contains stringified XML.
	 * <p>
	 * To be overridden by the component helper class only if the InternalInterface differs from
	 * the OperationsInterface.
	 * 
	 * @return Class  the Java Class of the internal interface. 
	 */
	protected Class<?> getInternalInterface() throws AcsJJavaComponentHelperEx {
		return getOperationsInterface();
	}	

	
	/**
	 * Method getInterfaceTranslator. Called by the container framework.
	 * @return Object
	 * @throws AcsJJavaComponentHelperEx
	 */
	final Object getInterfaceTranslator()
			throws AcsJJavaComponentHelperEx
	{
		Object interfaceTranslator = null;
		
		// the dynamic proxy is the default interface translator
		Object defaultInterfaceTranslator = null;
		try {
			defaultInterfaceTranslator =
				DynamicProxyFactory.getDynamicProxyFactory(m_containerLogger).createServerProxy(
					getOperationsInterface(), m_componentImpl, getInternalInterface());
		}
		catch (Throwable thr) {
			throw new AcsJJavaComponentHelperEx(thr);
		}
		
		try {
			interfaceTranslator = _getInterfaceTranslator(defaultInterfaceTranslator);
		} 
		catch (Throwable thr) {
			AcsJJavaComponentHelperEx ex = new AcsJJavaComponentHelperEx(thr);
			ex.setContextInfo("failed to obtain the custom interface translator.");
			throw ex;
		}
		if (interfaceTranslator == null) {
			// use default translator
			interfaceTranslator = defaultInterfaceTranslator;
		}
		return interfaceTranslator;
	}


	/**
	 * To be overridden by subclass only if it wants its own interface translator to be used.
	 * <p>
	 * The returned interface translator must implement the component's operations interface,
	 * and must take care of translating <code>in</code>, <code>inout</code> parameters, 
	 * forwarding the call to the respective method in the component implementation,
	 * and then translating <code>out</code>, <code>inout</code> parameters, and the return value.
	 * <p>
	 * It is foreseen that the necessary translations for most methods will be done 
	 * automatically by the dynamic proxy classes. Some methods may require manual translation,
	 * e.g. if an expression that involves xml entity classes is too complex for the 
	 * dynamic proxy. 
	 * <br> 
	 * On the other end of the spectrum, a manual implementation may
	 * choose to <emph>not</emph> unmarshal a marshalled binding object that 
	 * it receives as an <code>in</code> parameter, because the binding object
	 * will only be routed through to another component, so first unmarshalling 
	 * and later marshalling it would be an unnecessary performance loss. 
	 * In that case, the manual interface translator could call an additional
	 * method in the component implementation which expects the marshalled xml
	 * rather than the binding object.
	 * <p>
	 * To facilitate the implementation of a manual interface translator, 
	 * the <code>defaultInterfaceTranslator</code> is provided; it is a dynamic
	 * proxy object that implements the component's operations interface and
	 * forwards all calls to the component implementation created in
	 * <code>_createComponentImpl</code>, performing type translations in between.
	 * The methods that should be handled automatically can then be directly 
	 * delegated to that proxy object, and only those methods that require
	 * special care need to be implemented by hand.
	 * 
	 * @param defaultInterfaceTranslator  the default translator that the custon translator may 
	 * 										use to delegate some or all method invocations to.
	 * @return  the custom translator, or null if the default translator should be used.
	 */
	protected Object _getInterfaceTranslator(Object defaultInterfaceTranslator) throws AcsJJavaComponentHelperEx {
		return null;
	}
	
	
    /**
     * @see #_getComponentMethodsExcludedFromInvocationLogging
     */
    final String[] getComponentMethodsExcludedFromInvocationLogging() {
    	String[] ret = null;
		try {
			ret = _getComponentMethodsExcludedFromInvocationLogging();
		} 
		catch (Exception ex) {
			m_containerLogger.log(Level.WARNING, "failed to obtain the list of component methods to exclude from automatic logging.", ex);
		}
		return ret;
    }
    
    /**
     * Tells the container to not automatically log calls to certain component interface methods.
     * This method may be overridden by helper subclasses, since the default behavior is 
     * for the container to log all component method invocations except <code>ACSComponentOperations#componentState()</code>. 
     * <p>
     * Dealing with OffShoots: offshoots that follow the Corba Tie-approach enjoy automatic method invocation logging
     * just like components do. In the rare event that you explicitly activate offshoots from your component,
     * and want to suppress automatic logging for certain methods, return <code>String</code>(s) in the 
     * following format: <code>OFFSHOOT::&lt;offshootinterfacename&gt;#&lt;methodname&gt;</code>.
     * Example for ALMA archive: "OFFSHOOT::Operational#retrieveFragment".
     * <p>
     * Note that returning <code>String</code>s is fine (compared with more sophisticated <code>Method</code> objects) 
     * because the functional component interface methods cannot have parameter polymorphism thanks to IDL limitations.
     * Thus there is no ambiguity in the method names.
     * 
     * @return names of component methods for which the call-intercepting container should not log anything.
     *                     Can be null. 
     * @since ACS 5.0
     */
    protected String[] _getComponentMethodsExcludedFromInvocationLogging() {
    	return null;
    }
    
    /**
     * <strong>Do not overwrite this method unless you absolutely need to 
     * (currently only for the archive logger component!)</strong>
     * <p>
     * If <code>true</code> then the ORB logger will not send any log messages to the central (remote) Log service
     * after activation of this component, regardless of any kind (env var, CDB, dynamic) log level settings. 
     * Local stdout logging by the ORB is not affected.
     * Currently the ORB log suppression is not reversible: even when the component which requested this 
     * is unloaded, the ORB logger will not resume sending remote log messages. 
     * This may change in the future though.
     * <p>
     * Note that <code>getComponentMethodsExcludedFromInvocationLogging</code> can switch off automatic logging
     * done by the container on behalf of the component. However, the ORB does not use a logger-per-component 
     * concept, and thus can't be told selectively to not log anything for a particualar component.
     * The entire process (container and all components) are affected by suppressing the ORB's remote logging.  
     * <p>
     * This method addresses the special problem of an infrastructural component that receives log messages 
     * from the Log service and writes them to the archive. 
     * Any message logged by the ORB on the receiver side would go back to the Log service and be received 
     * by the infrastructural component, with positive feedback leading to an explosion of log messages. 
     * Therefore we can't rely on log level settings and must categorically rule out such feedback loops.
     * <p>
     * @since ACS 7.0 when JacORB logs get sent to the Log service by default.  
     * @return
     */
    protected boolean requiresOrbCentralLogSuppression() {
    	return false;
    }
}
