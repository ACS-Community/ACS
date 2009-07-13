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

import java.lang.reflect.Array;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
/**
 * Maps between one-dimensional arrays of different types.
 * Delegates the mapping of the array values to some other mapper.
 *  
 * @author hsommer Jan 3, 2003 4:00:22 PM
 */
public class ArrayMapper extends TypeMapper
{

	/**
	 * Constructor for ArrayMapper.
	 * @param delegate
	 */
	public ArrayMapper(Object delegate, Logger logger)
	{
		super(delegate, logger);
	}

	/**
	 * @see alma.acs.component.dynwrapper.TypeMapper#canTranslate(java.lang.Class, java.lang.Class, 
	 * alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	public boolean canTranslate(
		Class<?> oldObjClass,
		Class<?> newObjClass,
		ComponentInvocationHandler invHandler)
	{
		boolean canTranslate = false;

		try
		{
			if (oldObjClass.isArray() && newObjClass.isArray())
			{
				// check if dimensions match
				int dim1 = oldObjClass.getName().lastIndexOf('[') + 1;
				int dim2 = newObjClass.getName().lastIndexOf('[') + 1;
				if (dim1 == 1 && dim2 == 1)
				{
					// check if types match
					Class<?> oldCompType = oldObjClass.getComponentType();
					Class<?> newCompType = newObjClass.getComponentType();

					// recursion
					canTranslate = invHandler.canTranslate(oldCompType, newCompType);
				}
			}
		}
		catch (Exception e)
		{
			m_logger.log(Level.FINE, "failed to check array translation" ,e);
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
	 * Translates <code>oldArray</code> into a new array of type <code>newArrayClass</code>
	 * with the same length.
	 * The array elements are translated using some other mapper class.
	 *  
	 * @see alma.acs.component.dynwrapper.TypeMapper#translate(java.lang.Object, 
	 * java.lang.Object, java.lang.Class, alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	public <T> Object translate(
		Object oldArray,
		T newObjectTemplate,
		Class<T> newArrayClass,
		ComponentInvocationHandler invHandler)
		throws DynWrapperException
	{
		int length = Array.getLength(oldArray);
		Class<?> newType = newArrayClass.getComponentType();
		Object newArray = Array.newInstance(newType, length);
		
		for (int i = 0; i < length; i++)
		{
			Object oldValue = Array.get(oldArray, i);
			
			// recursion
			Object newValue = invHandler.translate(oldValue, null, newType);
			
			Array.set(newArray, i, newValue);
		}
		
		return newArray;
	}

}
