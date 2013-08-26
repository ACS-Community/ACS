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
package com.cosylab.cdb.jdal;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.exolab.castor.net.URIException;
import org.exolab.castor.net.URILocation;
import org.exolab.castor.net.URIResolver;
import org.exolab.castor.net.util.URILocationImpl;
import org.exolab.castor.net.util.URIResolverImpl;
import org.exolab.castor.xml.XMLException;
import org.exolab.castor.xml.schema.ComplexType;
import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.XMLType;
import org.exolab.castor.xml.schema.reader.Sax2ComponentReader;
import org.exolab.castor.xml.schema.reader.SchemaUnmarshaller;
import org.xml.sax.InputSource;
import org.xml.sax.Parser;
import org.xml.sax.SAXException;

public class XSDElementTypeResolver {
	
	protected Parser parser;
	private final Logger logger;
	DALURIResolver uriResolver;
	private final String root;
	
	public XSDElementTypeResolver(String root, Logger logger)
		throws ParserConfigurationException, SAXException
	{
		this.root = root;
		this.logger = logger;
		initializeParser();
	}

	protected void initializeParser()
		throws ParserConfigurationException, SAXException
	{
    	SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware(true);
        factory.setValidating(false);
 
        SAXParser saxParser = factory.newSAXParser();
        parser = saxParser.getParser();

        String schemas = DALImpl.getSchemas(root, logger);
        if (schemas == null) schemas = "";
       
        // do urn to file mapping for schemas
        Map<String, File> schemaName2File = new HashMap<String, File>();
        StringTokenizer tokenizer = new StringTokenizer(schemas);
        while (tokenizer.hasMoreTokens())
        {
        	String urn = tokenizer.nextToken();
        	String fileName = tokenizer.nextToken();
        	schemaName2File.put(getXSDElementName(urn) + ".xsd", new File(fileName));
        }
        
        uriResolver = new DALURIResolver(schemaName2File, logger);

	}
	
	private static class XMLNSElement
	{
		String xmlns;
		String element;
		
		public XMLNSElement(String xmlns, String element)
		{
			this.xmlns = xmlns;
			this.element = element;
		}

		public boolean equals(Object other) {
			if (other instanceof XMLNSElement)
			{
				XMLNSElement o = (XMLNSElement)other;
				return xmlns.equals(o.xmlns) && element.equals(o.element);
			}
			else
				return false;
		}

		public int hashCode() {
			return xmlns.hashCode() * 29 + element.hashCode();
		}
	}
	
	protected Map<XMLNSElement, String> cache = new HashMap<XMLNSElement, String>();
	
	public String[] getElementTypes(final String parentElementName, final String[] xmlns, final String[] elements)
	{
		if (xmlns.length != elements.length)
			throw new IllegalArgumentException("xmlns.length != elements.length");
		
		// it's up to user to
    	String[] types = new String[elements.length];
    	if (types.length == 0)
    		return types;

    	int hits = 0;
    	for (int index = 0; index < elements.length; index++)
		{
			XMLNSElement el = new XMLNSElement(xmlns[index]+parentElementName, elements[index]);
			if (cache.containsKey(el))
			{
				types[index] = cache.get(el);
				hits++;
			}
		}
    	
    	// all found in cache
    	if (hits == elements.length)
    		return types;

    	try
    	{
    		Set<String> processedNs = new HashSet<String>();
    	 	for (String ns : xmlns)
        	{
    	 		if (processedNs.contains(ns))
    	 			continue;
        	 	processedNs.add(ns);

        	 	List<String> list = new ArrayList<String>();
        	 	for (int i = 0; i < xmlns.length; i++)
        	 		if (types[i] == null && ns.equals(xmlns[i]))
        	 			list.add(elements[i]);
        	 	
        	 	if (list.size() > 0)
        	 	{
	        	 	String[] nsElements = list.toArray(new String[list.size()]);
	        	 	String[] nsTypes = internalGetElementTypes(parentElementName, ns, nsElements);
	
	        	 	// not very fast, but this is not critical here (also everything is cached)
        	 		for (int j = 0; j < elements.length; j++)
        	 			if (types[j] == null)
	        	 			for (int i = 0; i < nsElements.length; i++)
		        	 			if (nsElements[i].equals(elements[j]))
		        	 				types[j] = nsTypes[i];
        	 	}
        	}
    	}
    	catch (Throwable th)
    	{
    		th.printStackTrace();
    	}
    	
    	return types;
	}
	
	protected Map<String, Boolean> doesExtendMap = new HashMap<String, Boolean>();

	/**
	 * @TODO: Change this method to not rely on the xsd file name to match a part of the xml namespace.
	 *        For example, all xsd files could be parsed using a xerces XMLGrammarLoader 
	 *        (see http://xerces.apache.org/xerces2-j/faq-grammars.html#faq-3).
	 *        This xsd cache could then be queried here for the given namespace, 
	 *        and it could also be shared with the xml parser in {@link DALImpl}.
	 */
	public boolean doesExtend(final String xmlns, final String baseTypeName)
    	throws SAXException, IOException, URIException, XMLException
    {
		final String key = xmlns + " " + baseTypeName;
		if (doesExtendMap.containsKey(key))
			return doesExtendMap.get(key);

		final String elementName = getXSDElementName(xmlns);

		Schema schema = null;
		try {
			SchemaUnmarshaller schemaUnmarshaller = new SchemaUnmarshaller();
			schemaUnmarshaller.setURIResolver(uriResolver);
			Sax2ComponentReader handler = new Sax2ComponentReader(schemaUnmarshaller);
			parser.setDocumentHandler(handler);
			parser.setErrorHandler(handler);
			parser.parse(new InputSource(uriResolver.resolve(elementName + ".xsd", null).getReader()));
			schema = schemaUnmarshaller.getSchema();
		} catch (Exception e) {
			logger.warning("Failed to locate or parse schema '" + elementName + ".xsd', continuing with the assumption that '" 
					+ xmlns + "' does not extend '" + baseTypeName + "'.");
			doesExtendMap.put(key, false);
			return false;
		}

		String elementTypeName = null;
		
        Enumeration structures;
        for (structures = schema.getElementDecls(); structures.hasMoreElements(); )
        {
        	ElementDecl elementDecl = (ElementDecl) structures.nextElement();
        	if (elementDecl.getName().equals(elementName))
        	{
        		elementTypeName = elementDecl.getType().getName();
        		break;
        	}
        }
        
        if (elementTypeName != null)
        {
	        for (structures = schema.getComplexTypes(); structures.hasMoreElements(); )
	        {
	        	ComplexType complexType = (ComplexType) structures.nextElement();
	        	XMLType baseType = complexType.getBaseType();
	        	if (baseType != null)
	        	{
	        		if (baseType.getName().equals(baseTypeName))
	        		{
	        			doesExtendMap.put(key, true);
	        			return true;
	        		}
	        		else
	        		{
	        			boolean recursive = doesExtend("urn:schemas-cosylab-com:" + baseType.getName() + ":1.0", baseTypeName);
	        			if (recursive)
	        			{
		        			doesExtendMap.put(key, true);
	        				return true;
	        			}
	        		}
	        	}
	        }
	    }
        
		doesExtendMap.put(key, false);
        return false;
 	}

    protected String[] internalGetElementTypes(String parentElementName, final String xmlns, final String[] elements)
	throws SAXException, IOException, URIException, XMLException
{
	String[] types = new String[elements.length];
	if (types.length == 0)
		return types;

	final String elementName = getXSDElementName(xmlns);

    SchemaUnmarshaller schemaUnmarshaller = new SchemaUnmarshaller();
    schemaUnmarshaller.setURIResolver(uriResolver);
	Sax2ComponentReader handler = new Sax2ComponentReader(schemaUnmarshaller);
	parser.setDocumentHandler(handler);
	parser.setErrorHandler(handler);
	parser.parse(new InputSource(uriResolver.resolve(elementName + ".xsd", null).getReader()));
	Schema schema = schemaUnmarshaller.getSchema();

	String elementTypeName = null;
	
    Enumeration structures;
    for (structures = schema.getElementDecls(); structures.hasMoreElements(); )
    {
    	ElementDecl elementDecl = (ElementDecl) structures.nextElement();
    	if (elementDecl.getName().equals(elementName))
    	{
    		elementTypeName = elementDecl.getType().getName();
    		break;
    	}
    }
    
    // fallback: use parentName
	if (elementTypeName == null && parentElementName != null)
        for (structures = schema.getElementDecls(); structures.hasMoreElements(); )
        {
        	ElementDecl elementDecl = (ElementDecl) structures.nextElement();
        	if (elementDecl.getName().equals(parentElementName))
        	{
        		elementTypeName = elementDecl.getType().getName();
        		break;
        	}
        }
     
    
    if (elementTypeName != null)
    {
        for (structures = schema.getComplexTypes(); structures.hasMoreElements(); )
        {
        	ComplexType complexType = (ComplexType) structures.nextElement();
        	if (complexType.getName().equals(elementTypeName))
        	{
        		for (int i = 0; i < elements.length; i++)
        		{
        			ElementDecl element = complexType.getElementDecl(elements[i]);
        			if (element != null)
        			{
        				XMLType type = element.getType();
        				types[i] = type.getName();
        				if (types[i] == null)
        					types[i] = type.getBaseType().getName();
        			}
        			
        			// fill cache, also with null
        			cache.put(new XMLNSElement(xmlns+parentElementName, elements[i]), types[i]);
        		}
        		break;
        	}
        }
    }
    
    return types;
	}

	protected static class DALURIResolver implements URIResolver {
		private Map<String, File> schemaName2File;
		private final Logger logger;

		public DALURIResolver(Map<String, File> schemaName2File, Logger logger) {
			this.schemaName2File = schemaName2File;
			this.logger = logger;
		}

		/**
		 * @param href
		 *            the (schema) file name etc.
		 * @param documentBase
		 *            is ignored
		 */
		public URILocation resolve(String href, String documentBase) throws URIException {
			logger.finest("DALURIResolver#resolve(" + href + ", " + documentBase + ") called.");
			File schemaFile = schemaName2File.get(href);
			if (schemaFile == null) {
				int lastSlash = href.lastIndexOf('/');
				if (lastSlash != -1) {
					return resolve(href.substring(lastSlash + 1), documentBase);
				}
				else {
					String msg = "schema file '" + href + "' not found!";
					logger.warning(msg);
					throw new URIException(msg);
				}
			}

			URILocation ret = new URILocationImpl(schemaFile.getAbsolutePath());
			logger.finest("DALURIResolver#resolve(" + href + ", " + documentBase + ") returning " + ret.getAbsoluteURI());
			return ret;
		}

		/**
		 * Returns location for URN.
		 * 
		 * @see URIResolverImpl#resolveURN(String)
		 */
		public URILocation resolveURN(String urn) throws URIException {
			return resolve(getXSDElementName(urn) + ".xsd", null);
		}
	}

	/**
	 * @param xmlns
	 * @return
	 */
	private static final String getXSDElementName(final String xmlns) {
		final int endPos = xmlns.lastIndexOf(':');
		String elementName = xmlns.substring(0, endPos);
		final int pos = elementName.lastIndexOf(':');
		elementName = elementName.substring(pos+1);
		return elementName;
	} 
	
//	public static void main(String[] args) throws Throwable {
//		XSDElementTypeResolver etr = new XSDElementTypeResolver();
//		System.out.println(etr.doesExtend("urn:schemas-cosylab-com:LO2Base:1.0", "ControlDevice"));
//		/*
//		String[] types;
//		types = etr.getElementTypes(null, new String[] {"urn:schemas-cosylab-com:RampedPowerSupply:1.0"}, new String[] { "current" });
//		for (String type : types)
//			System.out.println(type);
//		*/
//	}

	/**
	 * @return the uriResolver
	 */
	public DALURIResolver getUriResolver() {
		return uriResolver;
	}

}
