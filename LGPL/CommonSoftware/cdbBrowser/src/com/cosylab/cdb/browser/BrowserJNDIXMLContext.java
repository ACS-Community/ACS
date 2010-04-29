/***************************************************************************
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
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
*    MA 02111-1307  USA
*/
package com.cosylab.cdb.browser;

import com.cosylab.cdb.jdal.*;
import java.io.IOException;
import java.io.StringReader;

import javax.naming.Context;
import javax.naming.Name;
import javax.naming.NameClassPair;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.NotContextException;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import alma.cdbErrType.CDBXMLErrorEx;
import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;
import java.util.*;
import java.util.logging.Logger;

/**
 * @author dvitas
 */

public class BrowserJNDIXMLContext extends BrowserJNDIContext
 {
     //is parser alrady initialised?
     static private boolean parserInited = false;
     static private SAXParserFactory factory;
     static private SAXParser saxParser;
     
     private XMLTreeNode node;
     // all hierarchical records inside this node
     private Hashtable nestedElements;
     
     static protected void initParser() {
	 if (parserInited)
	     return;
	 factory = SAXParserFactory.newInstance();
	 try {
	     saxParser = factory.newSAXParser();
	     parserInited = true;
	 } catch (ParserConfigurationException e) {
	     e.printStackTrace();
	 } catch (SAXException e) {
	     e.printStackTrace();
	 }
     }
     
     /**	
      * Selected node becomes a XMLTreeNode. This node contains the attributes (names AND values)
      * of the next CDBTree level. (throught parsing of the xml String)
      */
     public BrowserJNDIXMLContext(String name, String elements, String xml, Logger logger) {
	 super(name, elements, logger);
	 initParser();
	 
	 // parse XML string
	 XMLHandler xmlSolver = new XMLHandler(false, logger);
	 try {
	     saxParser.parse(new InputSource(new StringReader(xml)), xmlSolver);
	 } catch (SAXException e) {
	     e.printStackTrace();
	 } catch (IOException e) {
	     e.printStackTrace();
	 }
	 if (xmlSolver.m_errorString != null) {
	     String info = "XML parser error: " + xmlSolver.m_errorString;
	     AcsJCDBXMLErrorEx xmlErr = new AcsJCDBXMLErrorEx();
	     xmlErr.setErrorString(info);
	     System.err.println(info);
	 }
	 
	 // new XMLTreeNode that contains the Attibutes (names + values) of next level
	 node = xmlSolver.m_rootNode;
	 
	 //CDBLogic.createTabbedPane(name.substring(name.lastIndexOf("/") + 1), node.getFieldMap(),xml);
	 Browser.getInstance().createTabbedPane(node.getFieldMap(),xml);
	 node.getFieldMap().clear();                             
     }
     
    public BrowserJNDIXMLContext(String name, XMLTreeNode node, Logger logger) {
    	super(logger);
	 this.name = name;
	 this.node = node;
	 
	 //if(!node.getFieldMap().isEmpty())
	 //{ 
	 //CDBLogic.createTabbedPane(node.getName(), node.getFieldMap(), null);
	 Browser.getInstance().createTabbedPane(node.getFieldMap(),null);
	 node.getFieldMap().clear();                             
	 //}
     }
          
     public NamingEnumeration list(Name name) throws NamingException {
	 if (name.isEmpty()) {
	     JNDI_XMLNodesEnumeration enumeration = new JNDI_XMLNodesEnumeration(node);
		/*
		already in node.m_subNodes...
	     if (elements != null) { // do we have to care of hierarchical names
		 nestedElements = new Hashtable();
		 StringTokenizer st = new StringTokenizer(elements);
		 while (st.hasMoreTokens()) {
					String element = st.nextToken();
					if (element.indexOf(".xml") != -1)
					    continue; // skip nested xml
					nestedElements.put(element, element);
		 }
		 enumeration.nestedNames = nestedElements.keys();
	     }
		 */
	     return enumeration;
	 }
	 
	 Object target = lookup(name);
	 if (target instanceof Context) {
	     return ((Context) target).list("");
	 }
	 throw new NotContextException(name + " unable to list");
     }
     
     /*
      * method that will create the next level of the XMLTree
      */
     public Object lookup(Name name) throws NamingException {

	 String nameToLookup = name.toString();        //last selected node
	 String fullLookupName = this.name + "/" + nameToLookup;
	 int slashIndex = nameToLookup.indexOf('/');   //if '/' does not exist, then: int slashIndex  = -1
	 CDBLogic.setKey(fullLookupName);
	 
	 if( slashIndex != -1 ) { //nameToLookup is String rep. of the full path to last selected node
	     String nodeName = nameToLookup.substring(0,slashIndex); //path of the parent node
	     if (node.getNodesMap().containsKey(nodeName)) { //Returns true if map contains a mapping for the specified key.
		 XMLTreeNode nextNode = (XMLTreeNode) node.getNodesMap().get(nodeName);
		 return new BrowserJNDIXMLContext(fullLookupName, nextNode, logger).lookup(nameToLookup.substring(slashIndex+1));
	     }
	 }
	 

	 //nestedElement = hashtable
	 if (nestedElements != null && nestedElements.containsKey(nameToLookup)){ 
	     return super.lookup(name);
	 }
	

	 // node is the root XMLTreeNode
	 // gets the HashMap of its subnodes and checks if there is a mapping for the selected subnode
	 // if true -> create new XMLTreeNode with the selected subnode.
	 // The new XMLTreeNode will contain all Attributes as its childern
	 if (node.getNodesMap().containsKey(nameToLookup)) {
	     
	     XMLTreeNode nextNode = (XMLTreeNode) node.getNodesMap().get(nameToLookup);

	     //SEND NEXTNODE TO BROWSER
	     //Browser.getInstance().addXMLnode(nextNode);
	     //System.out.println(nextNode.getAttributeNames());
	     //System.out.println(nextNode.getAttributeValues());

	     return new BrowserJNDIXMLContext(fullLookupName, nextNode, logger);
	 }
	 
	 // ??
	 if (node.getFieldMap().containsKey(nameToLookup)) {
	     
	     return (String) node.getFieldMap().get(nameToLookup);
	 }
	 throw new NamingException("No name " + nameToLookup );
     }
     
     private class JNDI_XMLNodesEnumeration implements NamingEnumeration {
	 
	 // members
	 protected XMLTreeNode node;
	 protected Enumeration nestedNames = null;
	 protected Iterator nodesIter;
		protected Iterator fieldsIter;
	 
	 /**
	  * Constructor for CDBNamesList.
	  */
	 public JNDI_XMLNodesEnumeration(XMLTreeNode node) {
	     super();
	     this.node = node;
	     nodesIter = node.getNodesMap().keySet().iterator();
	     fieldsIter = node.getFieldMap().keySet().iterator();
	 }
	 
	 /**
	  * @see NamingEnumeration#next()
	  */
	 public Object next() throws NamingException {
	     //String name = (String) names.nextElement();
	     if (nestedNames != null && nestedNames.hasMoreElements())
		 return new NameClassPair(
					  (String) nestedNames.nextElement(),
					  Context.class.getName());
	     if (nodesIter.hasNext())
		 return new NameClassPair((String) nodesIter.next(), Context.class.getName());
	     if (fieldsIter.hasNext())
		 return new NameClassPair((String) fieldsIter.next(), String.class.getName());
	     return null;
	 }
	 
	 /**
	  * @see NamingEnumeration#hasMore()
	  */
	 public boolean hasMore() throws NamingException {
	     if (nestedNames != null && nestedNames.hasMoreElements())
		 return true;
	     if (nodesIter.hasNext())
		 return true;
	     if (fieldsIter.hasNext())
		 return true;
	     return false;
	 }
	 
	 /**
	  * @see NamingEnumeration#close()
	  */
	 public void close() throws NamingException {
	 }
	 
	 /**
	  * @see Enumeration#hasMoreElements()
	  */
	 public boolean hasMoreElements() {
	     try {
		 return hasMore();
	     } catch (NamingException e) {
		 return false;
	     }
	 }
	 
	 /**
	  * @see Enumeration#nextElement()
	  */
	 public Object nextElement() {
	     try {
		 return next();
	     } catch (NamingException e) {
		 throw new NoSuchElementException(e.toString());
	     }
	 }
	 
     }
}
