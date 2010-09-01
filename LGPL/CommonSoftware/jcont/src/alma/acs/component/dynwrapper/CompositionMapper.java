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

import java.lang.reflect.Field;
import java.util.logging.Logger;


/**
 * Maps between composite objects, delegating the translation of values
 * recursively if necessary. 
 * 
 * @author heiko
 */
public class CompositionMapper extends TypeMapper
{
	/**
	 * @see alma.acs.component.dynwrapper.TypeMapper#TypeMapper(java.lang.Object, Logger)
	 */
	public CompositionMapper(Object delegate, Logger logger)
	{
		super(delegate, logger);
	}

	/**
	 * @see alma.acs.component.dynwrapper.TypeMapper#canTranslate(java.lang.Class, 
	 * java.lang.Class, alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	@SuppressWarnings("unchecked")
	public boolean canTranslate(
		Class oldObjClass,
		Class newObjClass,
		ComponentInvocationHandler invHandler)
	{
		boolean canTranslate = true;

		Field oldField = null;
		String fieldMismatchMsg = null;
		try
		{
			// iterate over all public members of oldObjClass and check if they can be translated
			// into a corresponding member of newObjClass
			Field[] oldFields = oldObjClass.getFields();
			for (int i = 0; i < oldFields.length; i++)
			{
				oldField = oldFields[i];
				Field newField = newObjClass.getField(oldField.getName());
				// recursion
				boolean canTranslateField = invHandler.canTranslate(oldField.getType(), newField.getType());
				if (!canTranslateField)
				{
					canTranslate = false;
					break;
				}
			}			
		}
		catch (NoSuchFieldException e)
		{
			canTranslate = false;
			fieldMismatchMsg = "Member field '" + oldField.getName() +  
				"' not found in " + newObjClass.getName() + ".";
		}

		if (m_verbose)
		{
			String msg = "can " + (canTranslate ? "" : "not ") + 
				"translate from class '" + oldObjClass.getName() + "' to class '" 
				+ newObjClass.getName() + "'. ";
			if (fieldMismatchMsg != null)
			{
				msg += fieldMismatchMsg;
			}
			m_logger.finer(msg);
		}
				
		return canTranslate;		
	}


	/*
	 * TODO	
	 * Translates the object in the <code>value</code> field of <code>oldHolder</code> using some other mapper,
	 * and sets it as the <code>value</code> of <code>newHolderTemplate</code>.
	 *  
	 * @see alma.acs.component.dynwrapper.TypeMapper#translate(java.lang.Object, 
	 * java.lang.Object, java.lang.Class, alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	public <T> Object translate(
		Object oldObject,
		T newObjectTemplate,
		Class<T> newObjectClass,
		ComponentInvocationHandler invHandler)
		throws DynWrapperException
	{
		if (m_verbose)
		{
			m_logger.finer("will try to translate from '" + oldObject.getClass().getName()  + "' to '" + 
							newObjectClass.getName() + "'.");
		}
		if (newObjectTemplate == null)
		{
			try
			{
				newObjectTemplate = newObjectClass.newInstance();
			}
			catch (Exception e)
			{
				throw new DynWrapperException("failed to create composition class " + newObjectClass.getName());
			}
		}

// not true, e.g. if it's a return		
		
		try
		{
			Field[] oldFields = oldObject.getClass().getFields();
			for (int i = 0; i < oldFields.length; i++)
			{
				Field oldValueField = oldFields[i];
				Object oldValue = oldValueField.get(oldObject);
				Field newValueField = newObjectClass.getField(oldValueField.getName());
				Object newValueTemplate = newValueField.get(newObjectTemplate);
				// recursion
				Object newValue = null;
				if (oldValue == null)
				{
					m_logger.warning("Field " + oldObject.getClass().getName() + "." + 
							oldValueField.getName() + " is null. If this corresponds to a CORBA struct, the ORB will not transmit this data.");
				}
				else
				{
					newValue = invHandler.translate(oldValue, newValueTemplate, newValueField.getType());
				}
				newValueField.set(newObjectTemplate, newValue);
			}			
		}
		catch (Exception e)
		{
			throw new DynWrapperException(e);
		}

		if (m_verbose)
		{
			m_logger.finer("successfully translated from '" + oldObject.getClass().getName()  + "' to '" + 
							newObjectTemplate.getClass().getName() + "'.");
		}
		
		return newObjectTemplate;
	}

}
