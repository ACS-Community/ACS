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

import org.omg.CORBA.Any;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.CosPropertyService.PropertySet;
import org.omg.PortableServer.Servant;

import com.cosylab.CDB.DAL;

import alma.ACS.CharacteristicComponentDesc;
import alma.ACS.CharacteristicComponentOperations;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.Property;
import alma.ACS.PropertyDesc;
import alma.ACS.PropertyHelper;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

/**
 * Implementation of <code>alma.ACS.CharacteristicComponentImpl</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class CharacteristicComponentImpl extends ComponentImplBase
	implements CharacteristicComponentOperations {

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
	protected Map properties;

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
		properties = new HashMap();
	}

	/**
	 * @see alma.acs.component.ComponentLifecycle#cleanUp()
	 */
	public void cleanUp() {
		super.cleanUp();

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
					th.printStackTrace();
				}
			}
		}
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
	 * Register property on this component (and optionally CORBA activate).
	 * Registration is needed for property destruction on component destruction.
	 * @param propertyImpl		property implementation.
	 * @param propertyServant	property CORBA servant (e.g. Rx<type>POATie class). If <code>null</code> property will
	 * 							be threated as non-CORBA property and no CORBA activation will be done.
	 * @return CORBA activated property reference, <code>null</code> if <code>propertyServant == null</code>.
	 */
	public Property registerProperty(PropertyImpl propertyImpl, Servant propertyServant) {

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
			}
			catch (Throwable th)
			{
				// TODO log
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
	public void unregisterProperty(PropertyImpl propertyImpl) {
		
		Servant propertyServant = null;
		
		// remove from list
		synchronized (properties)
		{
			propertyServant = (Servant)properties.remove(propertyImpl);
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
				// TODO log
				th.printStackTrace();
			}
		}
	}

	/*********************** [ CharacteristicComponent ] ***********************/

	/**
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
				Iterator iter = properties.keySet().iterator();
				while (iter.hasNext())
					propertyDescriptors[i] = ((PropertyImpl)iter.next()).getPropertyDescriptor(); 
			}
						
			// TODO CORBA reference to this component to be set, PropertySet
			characteristicComponentDesc =
				new CharacteristicComponentDesc(null,
				   							    m_instanceName,
												propertyDescriptors,
											    null);
		}
		
		return characteristicComponentDesc; 
	}

	/*********************** [ CharacteristicModel ] ***********************/

	/**
	 * @see alma.ACS.CharacteristicModelOperations#get_characteristic_by_name(java.lang.String)
	 */
	public Any get_characteristic_by_name(String name)
		throws NoSuchCharacteristic {
		return characteristicModelImpl.get_characteristic_by_name(name);
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
		return characteristicModelImpl.get_all_characteristics();
	}

}
