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
package alma.acs.container.corba;

import java.util.Properties;

import alma.acs.util.CmdLineRegisteredOption;


/**
 * Created on 17-Oct-2002 10:55:42
 * @author hsommer
 * $Id$
 */
public class JavaIDLConfigurator extends OrbConfigurator
{

	/**
	 * @see alma.acs.container.corba.OrbConfigurator#getProperties()
	 */
	protected Properties _getProperties()
	{
		return null;
	}

	/**
	 * @see alma.acs.container.corba.OrbConfigurator#_declareOptions()
	 */
	protected CmdLineRegisteredOption[] _declareOptions()
	{
		return null;
	}

	/**
	 * @see alma.acs.container.corba.OrbConfigurator#getORBClass()
	 */
	public String getORBClass() 
	{
		return null; // will be defaulted to "com.sun.corba.se.internal.Interceptors.PIORB"
	}

	/**
	 * @see alma.acs.container.corba.OrbConfigurator#getORBSingleton()
	 */
	public String getORBSingleton() 
	{
		return null; // will be defaulted to "com.sun.corba.se.internal.corba.ORBSingleton"
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.corba.OrbConfigurator#getPortPropertyName()
	 */
	public String getPortPropertyName()
	{
		return "com.sun.CORBA.POA.ORBPersistentServerPort";
	}

}
