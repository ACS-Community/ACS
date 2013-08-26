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
package alma.acs.entityutil;

import java.lang.reflect.Method;
import java.util.logging.Logger;

import alma.entities.commonentity.EntityT;

/**
 * Takes the binding object that represents an (xml) entity object,
 * and finds the child element of type {@link alma.entities.commonentity.EntityT}. 
 * This element contains administrational information like id, schema version etc. and
 * is required in the xml schema in order for the object to be an ACS entity object.
 * <p>
 * @author hsommer May 6, 2003 5:31:11 PM
 */
public class EntityTFinder
{
	private Logger m_logger;
	private boolean m_verbose = false;

	public EntityTFinder(Logger logger)
	{
		m_logger = logger;
	}

	/**
	 * Only finds the method that can return the <code>EntityT</code> object.
	 * Uses introspection.
	 * <p>
	 * Generally, the users are expected to not call this method, but use 
	 * {@link #extractEntityT(Object)} instead.
	 *  
	 * @param entityClass  the class of the entity object (e.g. alma.xmljbind.test.schedblock.SchedBlock.class)
	 * @return  the method of entityClass which returns the EntityT object.
	 * @throws EntityException  if no such method can be found.
	 */
	public Method getEntityTMethod(Class entityClass)
			throws EntityException
	{
		Method entityTMethod = null;
		try
		{
			Method[] methods = entityClass.getDeclaredMethods();
			for (int i = 0; i < methods.length; i++)
			{
				Class retType = methods[i].getReturnType();
				if (EntityT.class.isAssignableFrom(retType))
				{
					entityTMethod = methods[i];
					break;
				}
			}						
		}
		catch (Exception ex)
		{
			String msg = "failed to extract getter method for child object EntityT from '" + entityClass.getName() + "'.";
			throw new EntityException(msg, ex);
		}

		if (entityTMethod == null)
		{
			String msg = "Class '" + entityClass.getName() + "' does not have getter method for child object EntityT.";
			throw new EntityException(msg);
		}
		
		if (m_verbose)
		{
			m_logger.finer("entity method '" + entityTMethod.getName() + "' found in class '" + entityClass.getName() + "'.");
		}
		
		return entityTMethod;
	}
	

	/**
	 * Extracts the <code>EntityT</code> child of <code>entityObj</code>.
	 * Implementation uses {@link #getEntityTMethod}.
	 * 
	 * @param entityObj  the entity object, e.g. an <code>alma.xmljbind.test.schedblock.SchedBlock</code>.
	 * @return  the administrational data of type EntityT, possibly <code>null</code>.
	 * @throws EntityException  if there is no method returning an <code>EntityT</code> assignable
	 * 							object, or if the invocation of that method fails. 
	 */
	public EntityT extractEntityT(Object entityObj)
			throws EntityException
	{
		Method entityTMethod = getEntityTMethod(entityObj.getClass());
		EntityT entityT = null;
		try
		{
			entityT = (EntityT) entityTMethod.invoke(entityObj, (Object[]) null);
		}
		catch (Exception e)
		{
			String msg = "failed to retrieve EntityT child object from binding class '" + entityObj.getClass() + ".";
			throw new EntityException(msg);
		}		
		
		if (entityT != null)
		{
			if (m_verbose)
			{
				m_logger.finer("EntityT with id='"  + entityT.getEntityId() + "' extracted from object of Class '" + 
										entityObj.getClass().getName() + "'.");
			}
		}
		else
		{
			m_logger.finer("Method " + entityObj.getClass().getName() + "#" + entityTMethod.getName() + 
							" returned null as the " + entityTMethod.getReturnType().getName() + " object");
		}
		return entityT;
	}

	public void setVerbose(boolean verbose)
	{
		m_verbose = verbose;
	}
}
