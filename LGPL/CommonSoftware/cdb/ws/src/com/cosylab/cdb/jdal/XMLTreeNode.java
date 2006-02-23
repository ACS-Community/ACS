/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * @author dragan
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
package com.cosylab.cdb.jdal;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Iterator;

public class XMLTreeNode {
	public static final String ARRAY_MARKER = "_Array_";
	
	XMLTreeNode m_parent; // parent in access tree
	LinkedHashMap m_subNodesMap;
	LinkedHashMap m_fieldMap;
	String m_name;
	boolean m_isArray;

	XMLTreeNode(XMLTreeNode pParent) {
		m_parent = pParent;
		m_subNodesMap = new LinkedHashMap();
		m_fieldMap = new LinkedHashMap();
		m_isArray = false;
	}
	public String getAttributeValues() {
		StringBuffer retVal = new StringBuffer(128);
		Iterator i = m_fieldMap.keySet().iterator();
		while (i.hasNext()) {
			String key = (String) i.next();
			String value = (String) m_fieldMap.get(key);
			retVal.append(value);
			if( i.hasNext() )
				retVal.append(',');
		}
		return retVal.toString();
	}
	public String getAttributeNames() {
		String key;
		StringBuffer retVal = new StringBuffer(128);
		Iterator i = m_fieldMap.keySet().iterator();
		while (i.hasNext()) {
			key = (String) i.next();
			// remove xml specific attributes
			if(key.startsWith(" xmlns") || key.startsWith("xmlns") || key.equals("space"))
				continue; // dont put unused info in this listing
			retVal.append(key);
			if( i.hasNext() )
				retVal.append(',');
		}
		
		
		// TODO !!! remove this cause this adds XML nested nodes, which are not attributes)
		
		if (retVal.length()>0 && m_subNodesMap.keySet().size()>0)
			retVal.append(',');

        // and elements names too
        i = m_subNodesMap.keySet().iterator();
        while (i.hasNext()) {
                key = (String) i.next();
    			retVal.append(key);
    			if( i.hasNext() )
    				retVal.append(",");
        }
        
		return retVal.toString();
	}
	/**
	 * @return LinkedHashMap
	 */
	public LinkedHashMap getFieldMap() {
		return m_fieldMap;
	}

	/**
	 * @return HashMap
	 */
	public HashMap getNodesMap() {
		return m_subNodesMap;
	}

	/**
	 * @return String
	 */
	public String getName() {
		return m_name;
	}
	
	/**
	 * Retruns true if this node is the root node for the array.
	 * Array is an user definied sequence of complex types that have every element named '_'.
	 * 
	 * @return boolean
	 */
	public boolean isArray() {
		return m_subNodesMap.get(ARRAY_MARKER) != null;
	}

}
