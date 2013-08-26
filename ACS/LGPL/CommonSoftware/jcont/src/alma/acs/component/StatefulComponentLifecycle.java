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
package alma.acs.component;

import java.util.Map;

/**
 * Interface to allow a component to declare that it is stateful (client-specific state or other) 
 * and can only be restarted (without surprising effects for its clients) 
 * if its state is handled by the container using the methods of this interface.
 * <p>
 * About references to components and callback objects: 
 * Baci monitors will be stored and reconnected automatically by the container.
 * Component references should be stored not as Corba IORs, but as qualified names 
 * which allow to request the real reference again from the manager. 
 * We will have to disucss how dependencies among a group of restarted/restored components can be handled this way.
 * Also for restarting a component, the manager should not release the components that this component is a client of.
 * Perhaps we should have a separate method to get and set component references or other Corba references.
 *  
 * @author hsommer
 * @since ACS 7.0.1
 * @deprecated  Early construction -- do not use this class yet, methods may change. 
 */
public interface StatefulComponentLifecycle extends ComponentLifecycle
{
	Map<String, String> getStateAsNameValueData();
	void setStateAsNameValueData(Map<String, String> nameValueStateData);
	
	String getStateAsXmlString();
	void setStateAsXmlString(String xmlStateData);	
}
