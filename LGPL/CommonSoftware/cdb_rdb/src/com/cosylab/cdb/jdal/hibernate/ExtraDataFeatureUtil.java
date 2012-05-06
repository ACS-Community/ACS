/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package com.cosylab.cdb.jdal.hibernate;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.HashSet;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.cosylab.CDB.DAOOperations;

import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.WrongCDBDataTypeEx;

public class ExtraDataFeatureUtil {
	
	/**
	 * Object pool used for DocumentBuilderObjectPool.
	 * Should probably move to module jacsutil at some point, but then  
	 * with additional features like unreferencing objects
	 * above some minimum pool size, when they are stale for a while. 
	 */
	private static abstract class ObjectPool<T>
	{
		private final Queue<T> objects = new ConcurrentLinkedQueue<T>();

		public abstract T createObject();

		public T borrowObject() {
			T t;
			if ((t = objects.poll()) == null) {
				t = createObject();
			}
			return t;
		}

		/**
		 * @param object
		 */
		public void returnObject(T object) {
			this.objects.offer(object);
		}
	}
	
	/**
	 * We reuse XML parsers from this pool, to avoid thread issues reported in 
	 * http://jira.alma.cl/browse/COMP-6488 .
	 */
	private static class DocumentBuilderObjectPool extends ObjectPool<DocumentBuilder>
	{
		// always thread-safe? Currently no other such calls in the rdbCDB process though.
		final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

		@Override
		public synchronized DocumentBuilder createObject() {
			try {
				return factory.newDocumentBuilder();
			} catch (ParserConfigurationException e) {
				throw new RuntimeException(e);
			}
		}
		
	}
	
	public static final DocumentBuilderObjectPool documentBuilderObjectPool = new DocumentBuilderObjectPool();
	
	public static final Set<String> EMPTY_SET = new HashSet<String>();
	private final Logger logger;

	
	public ExtraDataFeatureUtil(Logger logger) {
		this.logger = logger;
	}
	
	/*
	private static final String removeNamespace(String name)
	{
		// only one is allowed, start from beginning (most likely to find it there)
		int pos = name.indexOf(":");
		if (pos == -1)
			return name;
		else
			return name.substring(pos + 1);
	}
	*/
	
	public static Element getExtraDataMap(String xml)
		throws ParserConfigurationException, SAXException, IOException
	{
		if (xml == null || xml.isEmpty())
			return null;
		
		DocumentBuilder builder = documentBuilderObjectPool.borrowObject();
		try
		{
			Document xmldoc = builder.parse(new InputSource(new StringReader(xml)));
			return xmldoc.getDocumentElement();
		}
		finally {
			documentBuilderObjectPool.returnObject(builder);
		}
	}

	public String getExtraDataMap(DAOOperations dao, String path, Set<String> expectedAttributes, Set<String> expectedElements)
		throws CDBFieldDoesNotExistEx, WrongCDBDataTypeEx, ParserConfigurationException, TransformerException
	{
		Document xmldoc;
		DocumentBuilder builder = documentBuilderObjectPool.borrowObject();
		try
		{
			DOMImplementation impl = builder.getDOMImplementation();
			xmldoc = impl.createDocument(null, "data", null);
		}
		finally {
			documentBuilderObjectPool.returnObject(builder);
		}

		Element root = xmldoc.getDocumentElement();

		if (getExtraDataMap(root, dao, path, expectedAttributes, expectedElements))
		{
			StringWriter stringWriter = new StringWriter();
			StreamResult streamResult = new StreamResult(stringWriter);
			TransformerFactory tf = TransformerFactory.newInstance();
			Transformer serializer = tf.newTransformer();
			serializer.setOutputProperty(OutputKeys.INDENT,"yes");
			serializer.transform(new DOMSource(xmldoc), streamResult); 		
			String ret = stringWriter.toString();
			
			// Oracle XMLTYPE attributes don't like empty XML, thus we convert it to null
			if (ret != null && ret.trim().isEmpty()) {
				ret = null;
			}

			return ret;
		}
		else {
			return null;
		}
	}
	
	protected static boolean getExtraDataMap(Element root, DAOOperations dao, String path, Set<String> expectedAttributes, Set<String> expectedElements)
		throws CDBFieldDoesNotExistEx, WrongCDBDataTypeEx
	{
		final String prefix = path == null || path.length() == 0 ? "" : path + "/";
			
		boolean changed = false;
		
		String[] attributes = dao.get_string_seq(prefix + "_attributes");
		for (String attribute : attributes)
			if (!expectedAttributes.contains(attribute))
			{
				root.setAttribute(/*removeNamespace(*/attribute/*)*/, dao.get_string(prefix + attribute));
				changed = true;
			}
		
		String[] elements = dao.get_string_seq(prefix + "_elements");
		for (String element : elements)
			if (!expectedElements.contains(element) && !element.startsWith("xsi:"))
			{
				// special case, array of numbers (array of strings not supported)
				if (Character.isDigit(element.charAt(0)))
				{
					Element parentNode = (Element)root.getParentNode();
					parentNode.removeChild(root);
					parentNode.setAttribute(/*removeNamespace(*/root.getNodeName()/*)*/, dao.get_string(path));
					changed = true;
					break;
				}
				
				// artificial name, try to get the real one
				String elementName = element;
				if (Character.isDigit(element.charAt(element.length()-1)))
				{
					try
					{
						elementName = dao.get_string(prefix + element + "/_name");
					}
					catch (Throwable th)
					{
						// TODO log debug
					}
				}

				Element newElement = root.getOwnerDocument().createElement(/*removeNamespace(*/elementName/*)*/);
				root.appendChild(newElement);
				changed = true;

				getExtraDataMap(newElement, dao, prefix + element, EMPTY_SET, EMPTY_SET);
			}
		
		return changed;
	}

}
