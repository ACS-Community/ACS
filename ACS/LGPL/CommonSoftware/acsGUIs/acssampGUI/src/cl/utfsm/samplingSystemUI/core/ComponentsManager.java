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
/**
 *	@author Julio Araya (jaray[at]alumnos.inf.utfsm.cl) 
 *	@author Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/

package cl.utfsm.samplingSystemUI.core;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.omg.CORBA.InterfaceDef;
import org.omg.CORBA.InterfaceDefHelper;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Repository;
import org.omg.CORBA.RepositoryHelper;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription;
import org.omg.CORBA.TypeCodePackage.BadKind;

import alma.acs.component.ComponentDescriptor;
import alma.acs.container.ContainerServices;

/**
* Class to manage the Components within a Container, checks for properties, existance of a given Component and existance of a given Porperty within a managed Component.
*/
public class ComponentsManager{
	public static final String CORBALOC = System.getProperty("ACS.repository");
	public static final String IDL_PROPERTY = "IDL:alma/ACS/Property:1.0";

	private Repository rep = null;
	private ORB orb = null;
	private ContainerServices cServices = null;

	/**
	 * Retrieves an array of strings containing the components names in a container.
	 * @return array of strings.
	 **/
	public String[] getComponentsName(){
		String[] components = new String[0];
		try{
			components = cServices.findComponents(null,null);
		}catch(Exception e){/*do nothing*/}
		return components;
	}

	/**
	* Gets a list of Container Services and connects to the ACS repository.
	* @param cServices List of Container services 
	**/
	public ComponentsManager(ContainerServices cServices) throws IllegalStateException{
		this.cServices = cServices;
		Properties props = System.getProperties();
		orb = org.omg.CORBA.ORB.init(new String[0], props);
		// resolve ACS Repository
		org.omg.CORBA.Object repRef = null;
		try {
			repRef = orb.string_to_object(CORBALOC);
		} catch (Exception e) {
			throw new IllegalStateException("Cannot access orb initial reference 'InterfaceRepository'.",e);
		}	
		rep = RepositoryHelper.narrow(repRef);//cast to the Repository class
	}

	/**
	 * Verifies if a component exists by name in the container.
	 * @param componentName string with the component name.
	 * @return boolean, true if the component exists or false otherwise.
	 **/
	public boolean componentExists(String componentName){
		String[] components = getComponentsName();
		for(int i = 0 ; i < components.length ; i++){
			if(components[i].compareTo(componentName) == 0)
				return true;
		}
		return false;
	}

	/**
	 * Gets a list of properties name from a given component.
	 * @param componentName string with the component name.
	 * @return array of strings.
	 * FIXME: does it have to be case sensitive??
	 * FIXME: Must handle the exception.
	 **/
	public List<String> getComponentProperties(String componentName){

		List<String> tmp = new ArrayList<String>();
		if(componentExists(componentName)){
			try{	
				ComponentDescriptor desc = cServices.getComponentDescriptor(componentName);
				InterfaceDef ifdef = InterfaceDefHelper.narrow(rep.lookup_id(desc.getType()));
				if( ifdef == null )
					return null;
				// begin
				/*
				FullInterfaceDescription ifdes = ifdef.describe_interface();
                for(int i = 0 ; i < ifdes.attributes.length ; i++){
                        TypeCode tc = ifdes.attributes[i].type;
                        if(tc.kind() != TCKind.tk_objref)
                        	continue;
                        InterfaceDef tcdef = InterfaceDefHelper.narrow(rep.lookup_id(tc.id()));
                        if(tc.kind() == TCKind.tk_objref && tcdef.is_a(IDL_PROPERTY))
                                tmp.add(ifdes.attributes[i].name);
                }
                //end
                */
				tmp = getComponentProperties(ifdef);
			}catch(Exception e){
				e.printStackTrace();
				}
		}

		return tmp;
	}

	/**
	 * Recursive function to get the list of all the ACS properties
	 * for a given IDL type. It scans the current interface, and then
	 * it sub-scans the inherited types
	 * 
	 * @param ifdef The IDL interface defintion
	 * @return The list of properties for the given IDL definition, including the parents' properties
	 * @throws BadKind
	 */
	private List<String> getComponentProperties(InterfaceDef ifdef) throws BadKind {

		List<String> tmp = new ArrayList<String>();
		FullInterfaceDescription ifdes = ifdef.describe_interface();

		// Look-up the properties of the current interface
		
        for(int i = 0 ; i < ifdes.attributes.length ; i++){
        	TypeCode tc = ifdes.attributes[i].type;
            if(tc.kind() != TCKind.tk_objref) // ojo
            	continue;
            InterfaceDef tcdef = InterfaceDefHelper.narrow(rep.lookup_id(tc.id()));
            if(tc.kind() == TCKind.tk_objref && tcdef.is_a(IDL_PROPERTY))
                    tmp.add(ifdes.attributes[i].name);
        }
		// Lookup the children properties
		// TODO: Loop only over CharacteristicComponent interfaces
		InterfaceDef [] ifaces = ifdef.base_interfaces();
		for(int i = 0; i!= ifaces.length; i++){
			tmp.addAll(getComponentProperties(ifaces[i]));
		}
		return tmp;
	}	

	/**
	 * Verify if a property exists by name for a given component name.
	 * @param componentName string with the component name
	 * @param propertName string with the property name
	 * @return boolean true if the component have the property
	 */
	public boolean propertyExists(String componentName, String propertyName){
		List<String> tmp = getComponentProperties(componentName);
		String properties[] = new String[tmp.size()];
		tmp.toArray(properties);
		if(properties.length==0)
			return false;
		for(int i = 0 ; i < properties.length ; i++)
			if(properties[i].compareTo(propertyName) == 0)
				return true;
		return false;
	}

}
