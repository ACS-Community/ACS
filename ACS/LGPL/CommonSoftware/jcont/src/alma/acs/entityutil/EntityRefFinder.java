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
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Enumeration;

import alma.acs.util.StopWatch;
import alma.entities.commonentity.EntityRefT;

/**
 * Takes an entity object (binding class) and traverses the tree to find
 * references ({@link EntityRefT} nodes) to other entity objects.
 * <p>
 * Currently specific to XML binding classes produced by the Castor framework;
 * should be not too hard though to adapt to others (mainly enumeration stuff)
 * <p>
 * Assumes that it's a tree, not a graph 
 * (therefore no checks whether a node has been visited already). 
 * On a graph this could lead to an infinite loop.
 * <p>
 * Possible optimization would be to compute on demand a list of Class objects for
 * which we know that they can't have EntityRefT children, and to  
 * subsequently stop the recursion there.
 * 
 * @author hsommer Apr 24, 2003 2:05:43 PM
 */
public class EntityRefFinder
{

	private boolean m_debug;

	/**
	 * 
	 */
	public EntityRefFinder()
	{
		this(false);
	}

	public EntityRefFinder(boolean debug)
	{
		super();
		m_debug = debug;
	}


	public EntityRefT[] findEntityReferences(Object rootEntityObject)
		throws EntityException 
	{
		ArrayList<EntityRefT> entityRefs = new ArrayList<EntityRefT>();
		
		StopWatch stopw = new StopWatch(null);
				
		recursiveFindEntityReferences(rootEntityObject, entityRefs);
		
		if (m_debug)
		{
			System.out.println("findEntityReferences took " + stopw.getLapTimeMillis() + " ms.");
		}
				
		return ( entityRefs.toArray(new EntityRefT[0]) );
	}
	
	
	private void recursiveFindEntityReferences(Object obj, ArrayList<EntityRefT> entityRefs)
		throws EntityException
	{
		try
		{
			// dead end cases
			if (obj == null)
			{
				return;
			}
			Class objClass = obj.getClass();
			if (obj instanceof Class || 
				objClass.isPrimitive() || objClass.isArray() ||
				objClass.getPackage().getName().equals("java.lang") )
			{
				return;
			}
			
			if (m_debug)
			{
				System.out.println(obj.getClass().getName());
			}
			
			// the good recursion stopper case
			if (obj instanceof EntityRefT)
			{
				entityRefs.add((EntityRefT)obj);
				return;
			}
			
			// recursion for child objects
			Method[] methods = obj.getClass().getMethods();
			
			for (int i = 0; i < methods.length; i++)
			{
				if (methods[i].getName().startsWith("get") &&
				    methods[i].getParameterTypes().length == 0)
				{
					Object retObj = methods[i].invoke(obj, (Object[]) null);
					recursiveFindEntityReferences(retObj, entityRefs);
				}
				else if (methods[i].getName().startsWith("enumerate") &&
				          methods[i].getReturnType().isAssignableFrom(Enumeration.class) && 
				          methods[i].getParameterTypes().length == 0 &&
				          !Modifier.isStatic(methods[i].getModifiers())  // Castor enum classes have a static enumerate() method...
				        )
				{
					Enumeration retObjEnum = (Enumeration) methods[i].invoke(obj, (Object[]) null);
					while (retObjEnum.hasMoreElements())
					{
						Object retObj = retObjEnum.nextElement();
						recursiveFindEntityReferences(retObj, entityRefs);
					}
				}
			}
		}
		catch (EntityException ex)
		{
			// to avoid repeated wrapping when flying up the recursion stack
			throw ex;
		}
		catch (Throwable thr)
		{
			throw new EntityException("failed to find entity references ", thr);
		}
	}
}
