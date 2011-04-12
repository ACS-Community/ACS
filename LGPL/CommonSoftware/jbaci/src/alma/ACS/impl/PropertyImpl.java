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

import org.omg.CORBA.Any;
import org.omg.CosPropertyService.PropertySet;

import com.cosylab.CDB.DAL;

import alma.ACS.NoSuchCharacteristic;
import alma.ACS.Property;
import alma.ACS.PropertyDesc;
import alma.ACS.PropertyOperations;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;

/**
 * Implementation of <code>alma.ACS.Property</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @version $id$
 */
public class PropertyImpl implements PropertyOperations, PropertyReferenceHolder {

	/**
	 * Property name.
	 */
	protected String name;
	
	/**
	 * Property parent component.
	 */
	protected CharacteristicComponentImpl parentComponent;
	
	/**
	 * CharacteristicModel implementation (delegate).
	 */
	protected CharacteristicModelImpl characteristicModelImpl;

	/**
	 * Property descriptor (lazy initialization).
	 */
	private PropertyDesc propertyDesc;

	/**
	 * Constructor.
	 * @param name				property name, non-<code>null</code>.
	 * @param parentComponent	parent component, non-<code>null</code>.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public PropertyImpl(String name, CharacteristicComponentImpl parentComponent) 
		throws PropertyInitializationFailed {

		if (name == null)
			throw new NullPointerException("name == null");

		if (parentComponent == null)
			throw new NullPointerException("parentComponent == null");

		this.name = name;
		this.parentComponent = parentComponent;
	
		// TODO it would be nice to have componentCharacteristicsModel.createChildModel(name)
		try
		{
			DAL dal = parentComponent.getComponentContainerServices().getCDB();

			// create characteristic model
			// TODO think of error handling; why crating model per instance, pass DAL reference...
			//characteristicModelImpl = new CharacteristicModelImpl("alma/" + parentComponent.name() + "/" + name, dal);
			characteristicModelImpl = new CharacteristicModelImpl("alma/" + parentComponent.name(), dal);
			// TODO remove this ugly crap
			characteristicModelImpl.setFieldPrefix(name+"/");
		} catch (AcsJContainerServicesEx ce)
		{
			throw new PropertyInitializationFailed("Failed to create characteristic model.", ce);
		}

		// read characteristics
		// not called here, but in CommonPropertyImpl (needed fro enum property - propertyType must be known)
		//readCharacteristics();
	}

	/**
	 * Read property characteristics.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public void readCharacteristics()
		throws PropertyInitializationFailed {
		 // noop
	}
	
	/**
	 * Destroy property.
	 */
	public void destroy()
	{
		// unregister (and deactivate) from parent component
		parentComponent.unregisterProperty(this);
	}
	
	/**
	 * Get property parent component.
	 * @return	property parent component.
	 */
	public CharacteristicComponentImpl getParentComponent() {
		return parentComponent;
	}

	/*********************** [ Property ] ***********************/

	/**
	 * @see alma.ACS.PropertyOperations#name()
	 */
	public String name() {
		return name;
	}

	/**
	 * @see alma.ACS.PropertyOperations#characteristic_component_name()
	 */
	public String characteristic_component_name() {
		return parentComponent.name();
	}

	/*********************** [ CharacteristicModel ] ***********************/

	/**
	 * @see alma.ACS.CharacteristicModelOperations#get_characteristic_by_name(java.lang.String)
	 */
	public Any get_characteristic_by_name(String name)
		throws NoSuchCharacteristic {
		//the same thing has to be added here, maybe I can use the parentComponent 
		//method directly (so i don't have to repeat this line again)
		characteristicModelImpl.lendContainerServices(parentComponent.getComponentContainerServices());
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
		characteristicModelImpl.lendContainerServices(parentComponent.getComponentContainerServices());
		return characteristicModelImpl.get_all_characteristics();
	}

	/**********************************************************/
	/*********************** [ Helper ] ***********************/
	/**********************************************************/
	
	/**
	 * Get property descriptor.
	 * @see alma.ACS.PropertyDesc
	 */
	public synchronized PropertyDesc getPropertyDescriptor()
	{
		if (propertyDesc == null)
		{
			propertyDesc =	new PropertyDesc(propertyRef, parentComponent.name() + ":" + name, get_all_characteristics());
		}
		
		return propertyDesc; 
	}

	protected Property propertyRef;
	
	public synchronized Property getPropertyRef() {
		return propertyRef;
	}

	public synchronized void setPropertyRef(Property ref) {
		propertyRef = ref;
	}
	
}
