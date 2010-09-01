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
 * Maps between CORBA <code>Holder</code> classes that are used for 
 * <code>out</code>/<code>inout</code> parameters.
 * Delegates the mapping of the contained <code>value</code> field to some other mapper. 
 * 
 * @author heiko
 */
public class HolderMapper extends TypeMapper
{
	/**
	 * @see alma.acs.component.dynwrapper.TypeMapper#TypeMapper(java.lang.Object, Logger)
	 */
	public HolderMapper(Object delegate, Logger logger)
	{
		super(delegate, logger);
	}

	/**
	 * @see alma.acs.component.dynwrapper.TypeMapper#canTranslate(java.lang.Class, 
	 * java.lang.Class, alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	public boolean canTranslate(
		Class<?> oldObjClass,
		Class<?> newObjClass,
		ComponentInvocationHandler invHandler)
	{
		boolean canTranslate = false;

		try
		{
			// the OMG IDL-Java mapping spec mandates the ending "Holder" 
			if (oldObjClass.getName().endsWith("Holder") && 
				newObjClass.getName().endsWith("Holder") )
			{
				// public member 'value' also mandated by spec
				Field oldValue = oldObjClass.getDeclaredField("value");
				Field newValue = newObjClass.getDeclaredField("value");
				// recursion
				canTranslate = invHandler.canTranslate(oldValue.getType(), newValue.getType());
			}
		}
		catch (NoSuchFieldException e)
		{
			m_logger.fine("Holder class without field 'value' encountered.");
			// well, just leave canTranslate==false
		}

		if (m_verbose)
		{
			String msg = "can " + (canTranslate ? "" : "not ") + "translate from class '" + oldObjClass.getName()  + 
						"' to class '" + newObjClass.getName() + "'.";
			m_logger.finer(msg);
		}		
		return canTranslate;
	}


	/**
	 * Translates the object in the <code>value</code> field of <code>oldHolder</code> using some other mapper,
	 * and sets it as the <code>value</code> of <code>newHolderTemplate</code>.
	 *  
	 * @see alma.acs.component.dynwrapper.TypeMapper#translate(java.lang.Object, 
	 * java.lang.Object, java.lang.Class, alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	@SuppressWarnings("unchecked")
	public Object translate(
		Object oldHolder,
		Object newHolderTemplate,
		Class newObjectClass,
		ComponentInvocationHandler invHandler)
		throws DynWrapperException
	{
		if (newHolderTemplate == null)
		{
// TODO check this out			
//			if (newHolderTemplate == null)
//			{
//				throw new DynWrapperException("translating " + oldHolder.getClass().getName() + 
//					": new holder object of type " + newObjectClass.getName() + " must not be null.");
//			}
			try
			{
				newHolderTemplate = newObjectClass.newInstance();
			}
			catch (Exception e)
			{
				throw new DynWrapperException("failed to create holder class " + newObjectClass.getName());
			}
		}
		
		try
		{
			Field oldValueField = oldHolder.getClass().getDeclaredField("value");
			Field newValueField = newObjectClass.getDeclaredField("value");
			Object oldValue = oldValueField.get(oldHolder);
			
			//recursion
			Object newValue = invHandler.translate(oldValue, null, newValueField.getType());
			
			newValueField.set(newHolderTemplate, newValue);
			return newHolderTemplate;
		}
		catch (Exception e)
		{
			throw new DynWrapperException(e);
		}
	}

}
