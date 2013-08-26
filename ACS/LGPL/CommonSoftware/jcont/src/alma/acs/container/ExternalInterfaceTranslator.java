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
 * 
 */
package alma.acs.container;

/**
 * Interface used to distinguish a user-provided interface translator for XML-binded components which uses
 * for certain operations the default interface translator. This interface allows the ContainerServices to
 * retrieve the default interface translator created by the AcsContainer, and thus inject the offshoot
 * implementation/CORBA-object mappings into the component's interface translator.
 * 
 * @author rtobar
 * @since ACS 9.0
 */
public interface ExternalInterfaceTranslator {

	public void setDefaultInterfaceTranslator(Object defaultInterfaceTranslator);

	public Object getDefaultInterfaceTranslator();

}