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

import si.ijs.maci.COMPONENT_SPEC_ANY;
import si.ijs.maci.ComponentSpec;

/**
 * Wraps the {@link si.ijs.maci.ComponentSpec} struct for use by components.
 * <p>
 * Hides some fields. The idea is to not encourage components to use information 
 * about where some other components are running, in which language, etc.
 * If necessary, further fields might become accessible.
 * 
 * @author hsommer
 * created Nov 13, 2003
 * @see ComponentSpec
 */
public class ComponentQueryDescriptor 
{
	/**
	 * An alias for {@link si.ijs.maci.COMPONENT_SPEC_ANY#value}, currently "*".
	 * Used as a wildcard to state that any value for a field is fine.
	 */
	public static final String ANY = COMPONENT_SPEC_ANY.value; 
	
	private String m_compName;	
	private String m_compType;
	
//  these fields we want to hide for now...	
//	private String m_compImplCode;	
//	private String m_containerName;	

	public ComponentQueryDescriptor()
	{
		m_compName = ANY;
		m_compType = ANY;
	}
	
	public ComponentQueryDescriptor(String compName, String compType)
	{
		m_compName = compName;
		m_compType = compType;
	}
	
	public String getComponentName()
	{
		return m_compName;		
	}
	
	public void setComponentName(String compName)
	{
		m_compName = compName;		
	}
	
	public String getComponentType()
	{
		return m_compType;		
	}
	
	public void setComponentType(String compType)
	{
		m_compType = compType;		
	}
	

	/**
	 * To be used by the container to get the <code>ComponentSpec</code>
	 * that will be sent to the manager over CORBA.
	 * <p>
	 * <code>null</code> values will be converted automatically into {@link #ANY}.
	 * 
	 * @return filled with name and type from this class, ANY for code and container name.
	 */
	public ComponentSpec toComponentSpec()
	{
		ComponentSpec corbaSpec = new ComponentSpec();

		corbaSpec.component_name = (m_compName != null ? m_compName : ANY);
		corbaSpec.component_type = (m_compType != null ? m_compType : ANY);
		corbaSpec.component_code = ANY;
		corbaSpec.container_name = ANY;

		return corbaSpec;
	}

	public String toString()
	{
		return "ComponentQueryDescriptor { name='" + getComponentName() + "' type='" + getComponentType() + "' }";
	}
}
