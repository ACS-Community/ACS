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
import java.util.Iterator;
import java.util.LinkedHashMap;

public class XMLTreeNode {
	public static final String ARRAY_TYPE = "_ArrayNode_";
	public static final String DYNAMIC_TYPE = "_DynamicNode_";
	public static final String MAP_TYPE = "_MapNode_";
	public static final String NORMAL_TYPE = "_NormalNode_";
	
	XMLTreeNode m_parent; // parent in access tree
	LinkedHashMap<String, XMLTreeNode> m_subNodesMap;	// elements incl. hierarchy elements
	LinkedHashMap<String, XMLTreeNode> m_elementsMap;	// XML elements only 
	LinkedHashMap<String, String> m_fieldMap;
	String m_name;
	String m_nameSpace;
	String m_type;

	XMLTreeNode(XMLTreeNode pParent) {
		m_parent = pParent;
		m_subNodesMap = new LinkedHashMap<String, XMLTreeNode>();
		m_fieldMap = new LinkedHashMap<String, String>();
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
	
	public void markNodesAsElements()
	{
		// ignore remarkings, they are wrong
		if (m_elementsMap == null)
			m_elementsMap = new LinkedHashMap<String, XMLTreeNode>(m_subNodesMap);
	}

	public String getAttributeNames() {
		String key;
		StringBuffer retVal = new StringBuffer(128);
		Iterator<String> i = m_fieldMap.keySet().iterator();
		while (i.hasNext()) {
			key = i.next();
			// remove xml specific attributes
			if(key.startsWith(" xmlns") || key.startsWith("xmlns") || key.startsWith("xsi:") || key.equals("space"))
				continue; // dont put unused info in this listing
			retVal.append(key);
			if (i.hasNext())
				retVal.append(',');
		}

		return retVal.toString();
	}

	public String getElementNames() {
		// not defined, all the subNodes are actually XML elements
		if (m_elementsMap == null)
			return getNodeNames();
		
		String key;
		StringBuffer retVal = new StringBuffer(128);
		Iterator<String> i = m_elementsMap.keySet().iterator();
		while (i.hasNext()) {
			key = i.next();
			retVal.append(key);
			if (i.hasNext())
				retVal.append(",");
		}
		
		return retVal.toString();
	}
	
	public String getSubNodeNames() {
		// not defined, all the subNodes are actually XML elements
		if (m_elementsMap == null || m_subNodesMap.size() == 0 || m_elementsMap.size() == m_subNodesMap.size())
			return "";

		// list of subnodes
		LinkedHashMap<String, XMLTreeNode> diff = new LinkedHashMap<String, XMLTreeNode>(m_subNodesMap);
		for (Object key : m_elementsMap.keySet())
			diff.remove(key);
		
		String key;
		StringBuffer retVal = new StringBuffer(128);
		Iterator<String> i = diff.keySet().iterator();
		while (i.hasNext()) {
			key = i.next();
			retVal.append(key);
			if (i.hasNext())
				retVal.append(",");
		}
		
		return retVal.toString();
	}

	// return (elements + subnodes)
	public String getNodeNames() {
		String key;
		StringBuffer retVal = new StringBuffer(128);
		Iterator<String> i = m_subNodesMap.keySet().iterator();
		while (i.hasNext()) {
			key = i.next();
			retVal.append(key);
			if (i.hasNext())
				retVal.append(",");
		}
		
		return retVal.toString();
	}

	// return attributes + (elements + subnodes)
	public String getAttributeAndNodeNames() {
		final String retVal = getAttributeNames();
		final String elementNames = getNodeNames(); 
		if (elementNames.length()==0)
			return retVal;
		else if (retVal.length()>0)
			return retVal + "," + elementNames;
		else
			return elementNames;
	}

	/**
	 * @return LinkedHashMap
	 */
	public LinkedHashMap<String, String> getFieldMap() {
		return m_fieldMap;
	}

	/**
	 * @return HashMap
	 */
	public HashMap<String, XMLTreeNode> getNodesMap() {
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
		Iterator<String> i = m_fieldMap.keySet().iterator(); 
		while (i.hasNext()) {
			String key = i.next();
			String value = m_fieldMap.get(key);
			s.append(" "+ key + "=\""+value+"\"");
		}
		//subNodes
		i = m_subNodesMap.keySet().iterator(); 
		if(!i.hasNext() || isArrayNode()) s.append(" />");
		else{
		    s.append(">"); 
		    while (i.hasNext()) {
			s.append("\n");
			String key = i.next();
			XMLTreeNode node = m_subNodesMap.get(key);
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
