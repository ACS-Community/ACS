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

import java.util.List;
import java.util.Vector;

import si.ijs.maci.ComponentInfo;

/**
 * Wraps the {@link si.ijs.maci.ComponentInfo} struct for use by components.
 * <p>
 * Hides some fields. The idea is to not encourage components to use information 
 * about where some other components are running, in which language, etc.
 * If necessary, further fields might become accessible.
 * 
 * @author hsommer
 * created Oct 30, 2003 6:50:46 PM
 * @see ComponentInfo
 */
public class ComponentDescriptor 
{
	private ComponentInfo m_componentInfo;
	
	public ComponentDescriptor(ComponentInfo compInfo)
	{
		m_componentInfo = compInfo;
	}
	
	/**
	 * @return
	 */
	public java.lang.String getName()
	{
		return m_componentInfo.name;
	}

	/**
	 * @return
	 */
	public org.omg.CORBA.Object getComponent()
	{
		return m_componentInfo.reference;
	}

	/**
	 * @return
	 */
	public java.lang.String getType()
	{
		return m_componentInfo.type;
	}

	/**
	 * @return
	 */
	public java.lang.String[] getInterfaces()
	{
		return m_componentInfo.interfaces;
	}

	public String toString()
	{
		return "ComponentDescriptor { name='" + getName() + "' type='" + getType() + "' }" ;
	}
	
	
	public static List<ComponentDescriptor> fromComponentInfoArray (ComponentInfo[] compInfos) {
		List<ComponentDescriptor> compDescs = new Vector<ComponentDescriptor>(compInfos.length);
		for (ComponentInfo compInfo : compInfos) {
			compDescs.add(new ComponentDescriptor(compInfo));
		}
		return compDescs;
	}
}
