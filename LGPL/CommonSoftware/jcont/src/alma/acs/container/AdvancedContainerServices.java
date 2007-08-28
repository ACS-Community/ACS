/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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

import org.omg.CORBA.ORB;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;

/**
 * This class defines the more exotic methods from the container services interface,
 * which have been refactored out of <code>ContainerServices</code> into this separate interface.
 * <p>
 * The idea is
 * <ol> 
 * <li> to keep the standard <code>ContainerServices</code> interface lean,
 * <li> to discourage using these methods when not really needed, because they may violate architectural principles,
 * <li> perhaps later to require a flag in the CDB for a component to be allowed to access these methods.
 * </ol> 
 * @author hsommer
 * created May 2, 2005 5:46:23 PM
 */
public interface AdvancedContainerServices {
	
	/**
	 * Provides explicit acces to the normally invisible ORB, for components that fulfill infrastructural tasks.
	 * <p>
	 * <b>Normal subsystem components must not use this method!</b> If they feel they should get access to the ORB, 
	 * either ACS is missing a feature which should be reported, or there is a misunderstanding in how to develop software for Alma.
	 * @return the ORB that connects the container and its components with other processes.
	 */
	public ORB getORB();
	
    /**
     * Encapsulates {@link org.omg.CORBA.ORB#object_to_string(org.omg.CORBA.Object)}.
     * @param objRef the corba stub 
     * @return standardized string representation of <code>objRef</code>. 
     */
    public String corbaObjectToString(org.omg.CORBA.Object objRef);

    /**
     * Encapsulates {@link org.omg.CORBA.ORB#string_to_object(String)}.
     * @param strObjRef
     * @return org.omg.CORBA.Object
     */
    public org.omg.CORBA.Object corbaObjectFromString(String strObjRef);

    /**
     * Returns a reference to a new CORBA Any. Int Java the only way to do 
     * this is through the ORB itself (i.e., the create_any method).
     * <p>
     * The notification channel module provides a convenience class for dealing with CORBA Anys,
     * see {@link @see alma.acs.nc.AnyAide}.
     * 
     * @return org.omg.CORBA.Any
     */
    public org.omg.CORBA.Any getAny();

}
