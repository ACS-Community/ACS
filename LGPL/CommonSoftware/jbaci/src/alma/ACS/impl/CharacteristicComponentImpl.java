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

package alma.ACS.impl;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import org.omg.CORBA.Any;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.CosPropertyService.PropertySet;
import org.omg.PortableServer.Servant;

import com.cosylab.CDB.DAL;

import alma.ACS.CharacteristicComponentDesc;
import alma.ACS.CharacteristicComponentOperations;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.OffShootOperations;
import alma.ACS.Property;
import alma.ACS.PropertyDesc;
import alma.ACS.PropertyHelper;
import alma.ACS.PropertyOperations;
import alma.ACS.jbaci.PrioritizedExecutor;
import alma.ACS.jbaci.PrioritizedRunnable;
import alma.ACS.jbaci.PrioritizedRunnableComparator;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

/**
 * Implementation of <code>alma.ACS.CharacteristicComponentImpl</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @author <a href="mailto:cmaureirATinf.utfsm.cl">Cristian Maureira</a>
 * @version $id$
 */
public class CharacteristicComponentImpl extends ComponentImplBase
	implements CharacteristicComponentOperations, PrioritizedExecutor {

	/**
	 * CharacteristicModel implementation (delegate).
	 */
	protected CharacteristicModelImpl characteristicModelImpl;

	/**
	 * CharacteristicComponent descriptor (lazy initialization).
	 */
	private CharacteristicComponentDesc characteristicComponentDesc;

	/**
	 * List of all component properties (needed on component destruction).
	 */
	protected Map<PropertyOperations, Servant> properties;

	/**
	 * Component thread pool.
	 */
	private ThreadPoolExecutor threadPool;

	/**
	 * Number of requests in thread pool (guarantees order of execution).
	 */
	private static final int MAX_REQUESTS = 100;

	/**
	 * Number of threads in thread pool.
	 */
	private static final int MAX_POOL_THREADS = 10;
	

	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */

	
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException {
		super.initialize(containerServices);


		try
		{
			DAL dal = m_containerServices.getCDB();

			// create characteristic model
			// TODO think of error handling; why creating model per instance...
			characteristicModelImpl = new CharacteristicModelImpl("alma/" + m_instanceName, dal);
		} catch (AcsJContainerServicesEx ce)
		{
			throw new ComponentLifecycleException("Failed to create characteristic model.", ce);
		}

		// create properties list
		properties = new HashMap<PropertyOperations, Servant>();
	}

	/**
	 * @see alma.acs.component.ComponentLifecycle#cleanUp()
	 */
	public void cleanUp() throws AcsJComponentCleanUpEx {
		// shutdown thread-pool
		if (threadPool != null)
		{
			// initiate shutdown
			threadPool.shutdown();
			
			boolean nicePoolShutdown = false;
			// first be kind and wait up to 3 seconds to terminate
			try {
				nicePoolShutdown = threadPool.awaitTermination(3, TimeUnit.SECONDS);
			} catch (InterruptedException e) {
				// noop
			}
			boolean cleanPoolShutdown = nicePoolShutdown;
			if (!nicePoolShutdown) {
				// no more "mister-nice-guy", terminate all
				threadPool.shutdownNow();
				try {
					cleanPoolShutdown = threadPool.awaitTermination(1, TimeUnit.SECONDS);
				} catch (InterruptedException e) {
					// noop
				}
			}
			String msg = "jbaci thread pool shutdown ";
			if (!cleanPoolShutdown) {
				msg += "failed.";
			}
			else if (!nicePoolShutdown) {
				msg += "succeeded, but had to terminate running threads.";
			}
			else {
				msg += "succeeded.";
			}
			m_logger.log(AcsLogLevel.DELOUSE, msg);
		}

		// destroy all properties
		if (properties.size() != 0)
		{
			PropertyImpl[] propertyArray = null;
			synchronized (properties)
			{
				propertyArray = new PropertyImpl[properties.size()];
				properties.keySet().toArray(propertyArray);
			}
			for (int i = 0; i < propertyArray.length; i++)
			{
				try
				{
					propertyArray[i].destroy();
				}
				catch (Throwable th)
				{
					// TODO or even log here
					m_logger.log(Level.WARNING, "jBaci::CharacteristicComponentImpl::cleanUp - Error while destroying the properties.");
					th.printStackTrace();
				}
			}
		}
		super.cleanUp();
	}

	/**
	 * Get component container services.
	 * @return component container services.
	 */
	public ContainerServices getComponentContainerServices()
	{
		return m_containerServices;
	}
	
	/**
	 * Execute action. 
	 * If the maximum pool size or queue size is bounded,
	 * then it is possible for incoming execute requests to block. 
	 * <code>BACIExecutor</code> uses default 'Run' blocking policy: 
	 * The thread making the execute request runs the task itself. This policy helps guard against lockup. 
	 * @param action	action to execute.
	 * @return <code>true</code> on success.
	 */
	public boolean execute(PrioritizedRunnable action)
	{
		try
		{
			if (threadPool == null)
			{
				// TODO make PriorityBlockingQueue bounded!!! (to MAX_REQUESTS)
				// TODO should I use PooledExecutorWithWaitInNewThreadWhenBlocked...?
				threadPool = new ThreadPoolExecutor(1, MAX_POOL_THREADS, 1, TimeUnit.MINUTES,
										new PriorityBlockingQueue<Runnable>(MAX_REQUESTS, new PrioritizedRunnableComparator<Runnable>()),
										m_containerServices.getThreadFactory());
			}
			
			threadPool.execute(action);
			return true;	
		}
		catch (Throwable th)
		{
			return false;
		}
	}

	/**
	 * Register property on this component (and optionally CORBA activate).
	 * Registration is needed for property destruction on component destruction.
	 * @param propertyImpl		property implementation.
	 * @param propertyServant	property CORBA servant (e.g. Rx<type>POATie class). If <code>null</code> property will
	 * 							be treated as non-CORBA property and no CORBA activation will be done.
	 * @return CORBA activated property reference, <code>null</code> if <code>propertyServant == null</code>.
	 */
	public <T extends Servant & OffShootOperations> Property registerProperty(PropertyOperations propertyImpl, T propertyServant) {

		// allow activation if already in property list...
		Property property = null;
		if (propertyServant != null)
		{
			try
			{
				// TODO pesistent, user ID activation
				property = PropertyHelper.narrow(
					getComponentContainerServices().activateOffShoot(propertyServant)
				);
				
				// set reference to itself
				if (propertyImpl instanceof PropertyReferenceHolder)
					((PropertyReferenceHolder)propertyImpl).setPropertyRef(property);
			}
			catch (Throwable th)
			{
				// TODO log
				m_logger.log(Level.WARNING, "jBaci::CharacteristicComponentImpl::registerProperty - No resources to register the new property.");
				throw new NO_RESOURCES(th.getMessage());
			}
		}

		// add to list
		synchronized (properties)
		{
			if (!properties.containsKey(propertyImpl))
				properties.put(propertyImpl, propertyServant);
		}
		
		return property;
		
	}

	/**
	 * Unregister property on this component (and optionally CORBA deactivate).
	 * Should be called by <code>PropertyImpl.destroy()</code> method.
	 * @param propertyImpl	property implementation.
	 */
	public void unregisterProperty(PropertyOperations propertyImpl) {
		
		Servant propertyServant = null;
		
		// remove from list
		synchronized (properties)
		{
			propertyServant = properties.remove(propertyImpl);
		}
		
		// deativate CORBA monitor servant
		if (propertyServant != null)
		{
			try
			{
				getComponentContainerServices().deactivateOffShoot(propertyServant);
			}
			catch (Throwable th)
			{
				m_logger.log(Level.WARNING, "jBaci::CharacteristicComponentImpl::unregisterProperty - Error when trying to deactivate a property.");
				th.printStackTrace();
			}
		}
	}

	/*********************** [ CharacteristicComponent ] ***********************/

	/**
	 * NOTE: <code>characteristic_component_ref</code> member of <code>CharacteristicComponentDesc</code> is always set to <code>null</code>.
	 * @see alma.ACS.CharacteristicComponentOperations#descriptor()
	 */
	public CharacteristicComponentDesc descriptor() {
		
		if (characteristicComponentDesc == null)
		{
			PropertyDesc[] propertyDescriptors = null;
			synchronized (properties)
			{
				int i = 0;
				propertyDescriptors = new PropertyDesc[properties.size()];
				Iterator<PropertyOperations> iter = properties.keySet().iterator();
				while (iter.hasNext())
					propertyDescriptors[i++] = ((PropertyImpl)iter.next()).getPropertyDescriptor(); 
			}
						
			// TODO CORBA reference to this component to be set
			characteristicComponentDesc = new CharacteristicComponentDesc(null,
						m_instanceName,
						propertyDescriptors,
					    get_all_characteristics());

		}
		
		return characteristicComponentDesc; 
	}

	/*********************** [ CharacteristicModel ] ***********************/

	/**
	 * @see alma.ACS.CharacteristicModelOperations#get_characteristic_by_name(java.lang.String)
	 */
	public Any get_characteristic_by_name(String name)
		throws NoSuchCharacteristic {
		//for create the Any
		characteristicModelImpl.lendContainerServices(m_containerServices);
		Any ret = characteristicModelImpl.get_characteristic_by_name(name);
		if(ret!=null)
		return characteristicModelImpl.get_characteristic_by_name(name);
		else 
			throw new NoSuchCharacteristic();
	}

	/**
	 * @see alma.ACS.CharacteristicModelOperations#find_characteristic(java.lang.String)
	 */
	public String[] find_characteristic(String wildcard) {
		
		return characteristicModelImpl.find_characteristic(wildcard);

	}

	/**
	 * @see alma.ACS.CharacteristicModelOperations#get_all_characteristics()
	 */
	public PropertySet get_all_characteristics() {
		characteristicModelImpl.lendContainerServices(m_containerServices);
		return characteristicModelImpl.get_all_characteristics();
	}

}
