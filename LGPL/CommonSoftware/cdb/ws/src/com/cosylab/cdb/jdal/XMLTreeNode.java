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
	public static final String ARRAY_TYPE = "_ArrayNode_";
	public static final String DYNAMIC_TYPE = "_DynamicNode_";
	public static final String MAP_TYPE = "_MapNode_";
	public static final String NORMAL_TYPE = "_NormalNode_";
	
	XMLTreeNode m_parent; // parent in access tree
	LinkedHashMap m_subNodesMap;
	LinkedHashMap m_fieldMap;
	String m_name;
	String m_nameSpace;
	String m_type;

	XMLTreeNode(XMLTreeNode pParent) {
		m_parent = pParent;
		m_subNodesMap = new LinkedHashMap();
		m_fieldMap = new LinkedHashMap();
		m_nameSpace = "";
		m_type = NORMAL_TYPE;
	}

	/*
	 * Returns true if this node is the type Map.
	 * Map is an user definied sequence of complex types that have every element named '_'.
	 * To be a map, also the element must have a attribute called 'Name' 
	 *
	 * @return boolean
	 */
	boolean isMapNode(){
		return m_type.equals(MAP_TYPE);
	}
	/*
	 * Returns true if this node is the type Array.
	 * Array is an user definied sequence of simple type (long, string..) that have every 
         * element named '_'.
	 * 
	 * @return boolean
	 */
	boolean isArrayNode(){
		return m_type.equals(ARRAY_TYPE);
	}
	boolean isDynamicNode(){
		return m_type.equals(DYNAMIC_TYPE);
	}
	boolean isNormalNode(){
		return m_type.equals(NORMAL_TYPE);
	}
	void setDynamicNode(){
		m_type = DYNAMIC_TYPE;
	}
	void setMapNode(){
		m_type = MAP_TYPE;
	}
	void setArrayNode(){
		m_type = ARRAY_TYPE;
	}
	void setNormalNode(){
		m_type = NORMAL_TYPE;
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

	public String toString(boolean withMapNames){
		return toString(0, withMapNames);
	}
	
	public String toString(int indent, boolean withMapNames){
		StringBuffer s = new StringBuffer(256);
		for(int i = 0; i < indent; i++) {
			s.append("\t");
		}
		s.append("<"+m_nameSpace);
		if(!m_nameSpace.equals(""))
			s.append(":");	
		if(isArrayNode() || isDynamicNode() || (!withMapNames && isMapNode())) s.append("_"); 
		else s.append(m_name);
		
		//attributes
		Iterator i = m_fieldMap.keySet().iterator(); 
		while (i.hasNext()) {
			String key = (String) i.next();
			String value = (String) m_fieldMap.get(key);
			s.append(" "+ key + "=\""+value+"\"");
		}
		//subNodes
		i = m_subNodesMap.keySet().iterator(); 
		if(!i.hasNext() || isArrayNode()) s.append(" />");
		else{
		    s.append(">"); 
		    while (i.hasNext()) {
			s.append("\n");
			String key = (String) i.next();
			XMLTreeNode node = (XMLTreeNode) m_subNodesMap.get(key);
			s.append(node.toString(indent+1,withMapNames));
		    }
		    s.append("\n");
		    for(int j = 0; j < indent; j++) {
			s.append("\t");
		    }
		    s.append("</"+m_nameSpace);
		    if(!m_nameSpace.equals(""))
			s.append(":");	
		    if(isDynamicNode() || (!withMapNames && isMapNode())) s.append("_"); 
		    else s.append(m_name);
		    s.append(">");
		}
		return s.toString();
	}

}
