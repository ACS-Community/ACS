/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package acs.benchmark.util;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import si.ijs.maci.ComponentSpec;

import alma.ACS.ACSComponentOperations;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentLifecycle;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;

/**
 * A reflection-based convenience class that retrieves components either from the container services
 * or from its own component cache. 
 * <p>
 * Features:
 * <ul>
 *   <li>It restricts the interface to the IDL-derived "xxxOperations" class in order to allow 
 *       unit testing with mock components or actual component impl classes, without requiring container/corba support.
 *   <li>Hides the Corba helper class that performs the "narrow". Together with the above point, corba is then completely hidden.
 *   <li>(TODO) It optionally installs a transparent client-side interceptor that detects runtime problems.
 *   <li>(TODO) Configurable timeout disconnect from components, with transparent reconnect.
 * </ul>
 * @TODO: To identify containers/hosts, use collocated dummy target components instead of the deprecated
 *        {@link ContainerServices#getDynamicComponent(ComponentSpec, boolean)}.
 */

public class ComponentAccessUtil {

	protected final Logger logger;
	protected final ContainerServices contSrv;

	/**
	 * key = component instance name, value = component reference
	 */
	protected final Map<String, ACSComponentOperations> compName2Comp;
	
	public ComponentAccessUtil(ContainerServices contSrv) {
		this.contSrv = contSrv;
		this.logger = contSrv.getLogger();
		this.compName2Comp = new HashMap<String, ACSComponentOperations>();
	}

	/**
	 * Returns the cached reference or the component retrieved from the container services
	 * @param <T> The IDL-derived component interface
	 * @param compSpec
	 * @param idlOpInterface
	 * @return
	 * @throws AcsJContainerServicesEx
	 */
	public synchronized <T extends ACSComponentOperations> T getDynamicComponent(ComponentSpec compSpec, Class<T> idlOpInterface) 
		throws AcsJContainerServicesEx {
		
		T comp = null;
		boolean foundInCache = false;
		
		synchronized (compName2Comp) {
			// try the cache first
			comp = idlOpInterface.cast(compName2Comp.get(compSpec.component_name));
			
			// if not from cache, then from container services
			if (comp == null) {
				comp = getDynamicComponentFromContainerServices(compSpec, idlOpInterface);
				compName2Comp.put(compSpec.component_name, comp);
			}
			else {
				foundInCache = true;
			}
		}
		logger.log(AcsLogLevel.DEBUG, "Retrieved component '" + compSpec.component_name + "' from " 
				+ (foundInCache ? "cache." : "container services.") );
		return comp;
	}
	
	
	/**
	 * Gets the component via {@link #contSrv}, without using the cache.
	 * Can be overridden for tests, to bypass the container services.
	 * @throws AcsJContainerServicesEx 
	 */
	protected <T extends ACSComponentOperations> T getDynamicComponentFromContainerServices(ComponentSpec compSpec, Class<T> idlOpInterface) 
		throws AcsJContainerServicesEx {
	
		// infer the corba helper class
		Class<?> corbaHelperClass = null;
		try {
			int classBaseNameEnd = idlOpInterface.getName().lastIndexOf("Operations");
			String classBaseName = idlOpInterface.getName().substring(0, classBaseNameEnd);
			corbaHelperClass = Class.forName(classBaseName + "Helper");
		} catch (Exception ex) {
			String msg = "Failed to find Corba Helper class matching " + idlOpInterface.getName();
			logger.log(Level.FINE, msg, ex);
			throw new IllegalArgumentException(msg, ex); // TODO throw better ex
		} 
			
		org.omg.CORBA.Object compRaw = contSrv.getDynamicComponent(compSpec, false);
		
		// narrow the component reference
		Object comp = null;
		try {
			Method narrowMethod = corbaHelperClass.getMethod("narrow", org.omg.CORBA.Object.class);
			comp = narrowMethod.invoke(null, compRaw);
		} catch (Exception ex) {
			String msg = "Failed to Corba-narrow component " + compSpec.component_name;
			logger.log(Level.FINE, msg, ex);
			throw new IllegalArgumentException(msg, ex); // TODO throw better ex
		} 
		
		if (!idlOpInterface.isInstance(comp)) {
			String msg = "Narrowed component " + compSpec.component_name + " is not of type " + idlOpInterface;
			logger.log(Level.FINE, msg);
			throw new IllegalArgumentException(msg); // TODO throw better ex
		}
		return idlOpInterface.cast(comp);
	}
	
	
	public List<ACSComponentOperations> getCachedComponents() {
		synchronized (compName2Comp) {
			return new ArrayList<ACSComponentOperations>(compName2Comp.values());
		}
	}
	
	/**
	 * Should be called when a component is no longer needed. 
	 */
	public synchronized void releaseComponent(String compName) {
		// release the component with the container services
		contSrv.releaseComponent(compName, null);
		
		// remove its reference from the cache
		synchronized (compName2Comp) {
			compName2Comp.remove(compName);
		}
	}
	
	/**
	 * Should be called when no component are needed any more. 
	 */
	public synchronized void releaseAllComponents() {
		synchronized (compName2Comp) {
			List<String> compNames = new ArrayList<String>(compName2Comp.keySet()); // to avoid ConcurrentModificationException
			for (String compName : compNames) {
				releaseComponent(compName);
			}
		}
	}

	public void logInfo()
	{
		int nelem = compName2Comp.size();
		String message = "Components in the list: " + nelem + " (";
		List<String> compNames = new ArrayList<String>(compName2Comp.keySet()); // to avoid ConcurrentModificationException
		for (String compName : compNames) {
			message = message + compName + " ";
		}
		logger.info(message + ")");
	}
	

	/**
	 * Convenience method for subclasses created by unit tests, 
	 * that override {@link #getComponentFromContainerServices(String, Class)}
	 * and need to create and initialize component implementation classes locally inside the test.
	 */
	protected <T, U extends ACSComponentOperations & ComponentLifecycle> 
			T initCompImpl(U compImpl, Class<T> idlOpInterface) 
			throws ComponentLifecycleException {
		compImpl.initialize(contSrv);
		compImpl.execute();
		return idlOpInterface.cast(compImpl);
	}

}
