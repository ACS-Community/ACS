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
package alma.acs.genfw;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import de.bmiag.genfw.meta.ElementSet;
import de.bmiag.genfw.meta.core.ModelElement;

/**
 * Only accepts <code>ModelElement</code>s that have a unique name.
 * The behaviour should be the same as with using a normal <code>ElementSet</code> and 
 * having <code>equals()</code> methods based on the element name in all stored 
 * <code>ModelElement</code> subtypes.
 *  
 * @author hsommer
 * created Mar 3, 2004 9:16:29 AM
 */
public class UniqueNameModelElementSet extends ElementSet
{
	private Map m_nameMap = new HashMap();
	
	
	/**
	 * 
	 */
	public UniqueNameModelElementSet() {
		super(ModelElement.class);
	}

	/**
	 * @param initialValues
	 */
	public UniqueNameModelElementSet(ElementSet initialValues) {
		this();
		add(initialValues);
	}


	/**
	 * @see de.bmiag.genfw.meta.ElementSet#add(java.lang.Object)
	 */
	public void add(Object element) {
	    if (element == null || element instanceof ElementSet || !(element instanceof ModelElement)) {
	      super.add(element);
	    } 
	    else {
	    	String name = ((ModelElement) element).Name().Name();
	    	if (name != null && !m_nameMap.containsKey(name)) {
	    		m_nameMap.put(name, element);
	    		super.add(element);
	    	}
	    }
	}

	/**
	 * @see de.bmiag.genfw.meta.ElementSet#contains(java.lang.Object)
	 */
	public boolean contains(Object element) {
		boolean containsElem = true;
		if (!(element instanceof ModelElement)) {
			containsElem = false;
		}
		else if (element instanceof ElementSet) {
			for (Iterator iter = ((ElementSet)element).iterator(); iter.hasNext();) {
				Object elemObj = iter.next();
				if (!this.contains(elemObj)) {
					containsElem = false;
					break;
				}
			}
		}
		
		return containsElem;
	}
}
