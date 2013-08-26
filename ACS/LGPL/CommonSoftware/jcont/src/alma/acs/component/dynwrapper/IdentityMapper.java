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
package alma.acs.component.dynwrapper;

import java.util.logging.Logger;

/**
 * @author hsommer Dec 4, 2002 4:03:56 PM
 * $Id$
 */
public class IdentityMapper extends TypeMapper
{

	/**
	 * Constructor for IdentityMapper.
	 * @param delegate
	 */
	public IdentityMapper(Object delegate, Logger logger)
	{
		super(delegate, logger);
	}

	/**
	 * @see alma.acs.component.dynwrapper.TypeMapper#canTranslate(java.lang.Class, java.lang.Class, alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	public boolean canTranslate(
		Class<?> oldObjClass,
		Class<?> newObjClass,
		ComponentInvocationHandler invHandler)
	{
		if (oldObjClass.getName().equals(newObjClass.getName()))
		{
			if (m_verbose)
			{
				m_logger.finest("same types: " + oldObjClass.getName());
			}
			return true;
		}
		//m_logger.finest("different types: " + oldObjClass.getName() + ", " + newObjClass.getName());
		return false;
	}

	/**
	 * @see alma.acs.component.dynwrapper.TypeMapper#translate(java.lang.Object, java.lang.Object, java.lang.Class, alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	@SuppressWarnings("unchecked")
	public Object translate(
		Object oldObject,
		Object newObjectTemplate,
		Class newObjectClass,
		ComponentInvocationHandler invHandler)
		throws DynWrapperException
	{		
		return oldObject;
	}

	
}
