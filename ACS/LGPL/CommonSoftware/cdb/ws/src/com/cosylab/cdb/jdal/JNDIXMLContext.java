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
 * Created on Apr 8, 2003
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code Template
 */
package com.cosylab.cdb.jdal;

import java.io.IOException;
import java.io.StringReader;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.logging.Logger;

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

//import alma.cdbErrType.CDBXMLErrorEx;

/**
 * @author dvitas
 */

public class JNDIXMLContext extends JNDIContext {

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
	 * @param name
	 */
	public JNDIXMLContext(String name, String elements, String xml, Logger logger) {
		super(name, elements, logger);
		initParser();
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
			//XMLerror xmlErr = new XMLerror(info);
			System.err.println(info);
		}
		node = xmlSolver.m_rootNode;
	}

	public JNDIXMLContext(XMLTreeNode node, Logger logger) {
		super(logger);
		this.node = node;
	}

	/* (non-Javadoc)
	 * @see javax.naming.Context#list(javax.naming.Name)
	 */
	public NamingEnumeration list(Name name) throws NamingException {
		//System.out.println( "XML list on " + this.name + " for name " + name.toString() + "->" + elements);

		if (name.isEmpty()) {
			JNDIXMLNodesEnumeration enumeration = new JNDIXMLNodesEnumeration(node);
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

	/* (non-Javadoc)
	 * @see javax.naming.Context#lookup(javax.naming.Name)
	 */
	public Object lookup(Name name) throws NamingException {
		//System.out.println( "XML lookup on " + this.name + " for name " + name.toString() );
		String nameToLookup = name.toString();
		int slashIndex = nameToLookup.indexOf('/');
		if( slashIndex != -1 ) { // is it composite name?
			String nodeName = nameToLookup.substring(0,slashIndex);
			if (node.getNodesMap().containsKey(nodeName)) {
				XMLTreeNode nextNode = node.getNodesMap().get(nodeName);
				return new JNDIXMLContext(nextNode, logger).lookup(nameToLookup.substring(slashIndex+1));
			}
		}
		
		if (nestedElements != null && nestedElements.containsKey(nameToLookup)) {
			return super.lookup(name);
		}
		if (node.getNodesMap().containsKey(nameToLookup)) {
			XMLTreeNode nextNode = node.getNodesMap().get(nameToLookup);
			return new JNDIXMLContext(nextNode, logger);
		}
		if (node.getFieldMap().containsKey(nameToLookup)) {
			return node.getFieldMap().get(nameToLookup);
		}
		throw new NamingException("No name " + nameToLookup );
	}

	private class JNDIXMLNodesEnumeration implements NamingEnumeration {

		// members
		protected XMLTreeNode node;
		protected Enumeration nestedNames = null;
		protected Iterator<String> nodesIter;
		protected Iterator<String> fieldsIter;

		/**
		 * Constructor for CDBNamesList.
		 */
		public JNDIXMLNodesEnumeration(XMLTreeNode node) {
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
				return new NameClassPair(nodesIter.next(), Context.class.getName());
			if (fieldsIter.hasNext())
				return new NameClassPair(fieldsIter.next(), String.class.getName());
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
