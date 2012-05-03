package com.cosylab.cdb.jdal;

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
 *
 * @author dragan
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */

import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import alma.cdbErrType.wrappers.AcsJCDBRecordDoesNotExistEx;

public class XMLHandler extends DefaultHandler {
	private static boolean m_toString;
	private final Logger logger;
	//
	public XMLTreeNode m_rootNode = null;
	protected XMLTreeNode m_parent;
//	private StringBuffer m_arrayContent = new StringBuffer(64);
	private int elementID = 0;
	
	//
	// if we are already write down xml header
	private boolean headerEmited = false;
	// used in hierarchy build when we don't want to close element before we emit all its subnodes
	private boolean autoCloseStartingElement = true;
	private boolean firstElement = true;
	// elements as they are in xmls i.e. 'TestPowerSupplyACS'
	private ArrayList startElements = null;
	// logical names as found in hierarchy i.e. 'TEST_PS_1'
	private ArrayList elementNames = null;
	private String firstElementName = null;
	// will be used to store xmlns prefixes
	private ArrayList prefixes = new ArrayList();
	// should we mark arrays
	private int markArrays = 0;

	// public fields for content and eventually error
	public String m_errorString = null;
	public StringBuffer m_xmlString = new StringBuffer(256);
	
	// ctor
	public XMLHandler(boolean toString, Logger logger) {
		this.logger = logger;
		m_toString = toString;
		
	}

	public void startDocument() throws SAXException {
		if (m_toString && !headerEmited) {
			m_xmlString.append("<?xml version='1.0' encoding='ISO-8859-1'?>\n");
			headerEmited = true;
		}
		firstElement = true;
	}
	/** Start element. */
	public void startElement(String uri, String local, String raw, Attributes attrs)
		throws SAXException {
		boolean inArray = false;
	
		if(!autoCloseStartingElement && firstElement) {
			if(startElements == null) {
				startElements = new ArrayList();
				elementNames = new ArrayList();
			}
			startElements.add(raw);
			
			if(firstElementName == null || raw.equals("fault-family"))	// alarm system fix to allow "component#property"
				firstElementName = raw;
			elementNames.add(firstElementName);
			raw = firstElementName;
			firstElementName = null;
			
			firstElement = false;
		}

		if (!m_toString) {
			XMLTreeNode pNode;
			if (m_rootNode == null) {
				m_rootNode = new XMLTreeNode(null);
				m_rootNode.m_name = raw;
				m_parent = m_rootNode;
			}
			else{
				pNode = new XMLTreeNode(m_parent);
				if (attrs.getLength() > 0 && (raw.equals("_") || raw.endsWith(":_"))) {
					// in this case we will replace the element name with Name name, otherwise its first attribute is taken
					if(raw.indexOf(':') >= 0) pNode.m_nameSpace = raw.substring(0,raw.indexOf(':'));
					raw = attrs.getValue("Name");
					if (raw == null){
					    inArray = true; // special case when array is found
					    raw = attrs.getValue(0);
					    if( markArrays > 0 ) { // ad an array marker
						pNode.setArrayNode();
					    /*	if(markArrays == 2) { // add node to the array - used by diff check where we must distinguish between original array elements and merged ones 
							XMLTreeNode arrNode = (XMLTreeNode)m_parent.m_subNodesMap.get(XMLTreeNode.ARRAY_MARKER); 
							arrNode.m_subNodesMap.put(String.valueOf(arrNode.m_subNodesMap.size()), pNode);
					    	}
*/
					    }
					}else{
					    if( markArrays > 0 ){ 
						if(raw.equals("*")) pNode.setDynamicNode();
						else pNode.setMapNode();
					    }
					}
				}

				String mapKey;
				if (m_parent.m_subNodesMap.get(raw) != null) {
					elementID++;
					pNode.m_name = raw;
					mapKey = raw + elementID;
				}
				else {
					pNode.m_name = mapKey = raw;
				}
				m_parent.m_subNodesMap.put(mapKey, pNode);
				m_parent = pNode;
			}
		}
		else{
		    if(elementNames == null) elementNames = new ArrayList();
			m_xmlString.append('<');
			if (attrs.getLength() > 0 && (raw.equals("_") || raw.endsWith(":_"))) {
			    // in this case we will replace the element name with Name name
			    //String mapName = attrs.getValue("Name");
			    //if(mapName != null && !mapName.equals("*")){
			//	m_xmlString.append(mapName);
			//	elementNames.add(mapName); 
//			    }else{
m_xmlString.append(raw); elementNames.add(raw);//}
			}else{
				m_xmlString.append(raw);
				elementNames.add(raw);
			}
		}
		// add prefixes if any only if we are wiritng XML otherwise (like in DAO) it is not needed
		if(prefixes.size() > 0) {
			for (int i = 0; i < prefixes.size(); i++) {
				if (m_toString) {
					m_xmlString.append(prefixes.get(i));
				}
				else {
					String prefix = (String)prefixes.get(i);
					String name = prefix.substring(0, prefix.indexOf('='));
					String value = prefix.substring(prefix.indexOf('=')+1);
					m_parent.m_fieldMap.put(name, value);
				}
			}
			prefixes.clear();
		}
		
		if (attrs != null) {
			for (int i = 0; i < attrs.getLength(); i++) {
				if (m_toString) {
					m_xmlString.append(' ').append(attrs.getQName(i)).append("=\"").append(attrs.getValue(i)).append("\"");
				}
				else {
					m_parent.m_fieldMap.put(attrs.getQName(i), attrs.getValue(i));
				}

			}
		}
		if (m_toString)
			m_xmlString.append('>');

	}

	/** End element. */
	public void endElement(String uri, String local, String raw) throws SAXException {
		if(!autoCloseStartingElement) {
			String elem = (String)startElements.get(startElements.size()-1);
			if(elem.equals(raw))
				return; // will close it later on explicit call
		}
		
		if (m_toString) {
			String elem = (String)elementNames.remove(elementNames.size()-1);
			m_xmlString.append("</").append(elem).append('>');
		}
		else {
		/*	if (m_parent.m_isArray) {
				String nolastDelim = m_arrayContent.substring(0,m_arrayContent.length()-1); // remove last delimiter
				m_parent.m_parent.m_fieldMap.put(m_parent.m_name, nolastDelim);
				// since we added array content as field then there is no need any more for array node  itself
				m_parent.m_parent.m_subNodesMap.remove(m_parent.m_name);
				m_arrayContent.delete(0, m_arrayContent.length());
			}
		*/	m_parent = m_parent.m_parent;
		}
	}
	public void characters(char buf[], int offset, int len) throws SAXException {
		if (m_toString) {
			// maybe we should normalize first
			m_xmlString.append(buf, offset, len);
		}
	}

	/* (non-Javadoc)
	 * @see org.xml.sax.ContentHandler#startPrefixMapping(java.lang.String, java.lang.String)
	 */
	public void startPrefixMapping(String prefix, String uri) throws SAXException {
		//if(!firstElement)
		//	return; // use just mappings for first element. Other elements also have prefixes at least from first element but they are not written inside XML so we will ignore them
		if(prefix.length()>0)
			prefixes.add(" xmlns:"+prefix+"=\""+uri+"\"");
		else
			prefixes.add(" xmlns=\""+uri+"\"");
	}

	public void error(SAXParseException e) {
		m_errorString = "Line=" + e.getLineNumber() + ", Column="+e.getColumnNumber()+": " + e.getMessage();
		logger.log(Level.FINE, "XML parser error: ", e);
	}

	/**
	 * Should we automatically close ending node
	 * @param b
	 */
	public void setAutoCloseStartingElement(boolean b) {
		autoCloseStartingElement = b;
	}

	/**
	 * 
	 */
	public void closeElement() throws SAXException {
		if(autoCloseStartingElement)
			return;

		/*String elem = (String)*/startElements.remove(startElements.size()-1);
		if(startElements.size() == 0)
			startElements.add("dummy_elem");

		String name = (String)elementNames.remove(elementNames.size()-1);
		endElement(null, null, name);
	}

	/**
	 * @param string
	 */
	public void setFirstElement(String string) {
		firstElementName = string;
	}

	/**
	 * @param markArrays
	 */
	public void setMarkArrays(int mode) {
		this.markArrays = mode;
	}
	
	public XMLHandler getChild(String curl)
		throws AcsJCDBRecordDoesNotExistEx{
	    XMLTreeNode tnFather = m_rootNode;
	    XMLTreeNode tnChild = null;
	    String strFirst = "";
	    String strLast = curl;
	    String strPast ="";
	    do{
    	    	strFirst = strLast.substring(0, strLast.indexOf('/'));
	    	strLast = strLast.substring(strLast.indexOf('/')+1);
/*Iterator i = tnFather.getNodesMap().keySet().iterator();
while(i.hasNext()){
String s = (String)i.next();
}
*/
	 	tnChild = tnFather.getNodesMap().get(strFirst);
		strPast+="/"+strFirst;
		if(tnChild == null){
		    AcsJCDBRecordDoesNotExistEx recordDoesNotExist = 
			new AcsJCDBRecordDoesNotExistEx();
		    recordDoesNotExist.setCurl(strPast);
		    throw recordDoesNotExist;
		}
		tnFather = tnChild;
	    }while(!strLast.equals(""));
	    XMLHandler a = new XMLHandler(false, logger);
	    a.m_rootNode = tnChild;
	    return a;  
	}

	public String toString(boolean withMapNames){
		if(m_toString)
			return m_xmlString.toString();
		else{
			if(m_rootNode == null) return null;
			else return m_rootNode.toString(withMapNames);
		}
	}

}
