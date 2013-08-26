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
 */
/*
 * Created on Feb 6, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.cdb.jdal;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.ext.LexicalHandler;

import com.cosylab.CDB.DAOPOATie;
import com.cosylab.CDB.WDAO;
import com.cosylab.CDB.WDAOHelper;

import alma.acs.logging.AcsLogLevel;
import alma.cdbErrType.CDBExceptionEx;
import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.CDBRecordAlreadyExistsEx;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBRecordIsReadOnlyEx;
import alma.cdbErrType.CDBXMLErrorEx;
import alma.cdbErrType.wrappers.AcsJCDBExceptionEx;
import alma.cdbErrType.wrappers.AcsJCDBFieldDoesNotExistEx;
import alma.cdbErrType.wrappers.AcsJCDBRecordAlreadyExistsEx;
import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;

/**
 * Implementation of Writable Data Access Layer (WDAL) interface.   Enables
 * adding, removing and modifying nodes in ConfigurationDataBase (CDB). Nodes
 * are xml files organized in directories so that directory has a xml file
 * named exactly as directory. For example node /alma/LAMP1 is valid CDB node
 * if there is file /alma/LAMP1/LAMP1.xml.   It is implemented as wraper
 * around DAL.
 *
 * @author dvitas
 */
public class WDALImpl extends WDALBaseImpl
{
	private POA poa = null;
	private HashMap<String, WDAO> wdaoMap = new HashMap<String, WDAO>();
	/**
	 * Constructor as it is for DAL
	 *
	 * @param args
	 * @param orb
	 * @param poa
	 */
	public WDALImpl(String[] args, ORB orb, POA poa, Logger logger)
	{
		super(args, orb, poa, logger);
		this.poa = poa;
	}

	DALImpl getDALImplDelegate() {
		return dalImpl;
	}
	
	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDALOperations#get_WDAO_servant(java.lang.String)
	 */
	public WDAO get_WDAO_Servant(String curl)
		throws CDBXMLErrorEx
	{
		dalImpl.totalDALInvocationCounter.incrementAndGet();

		// if we already have WDAO
		synchronized(wdaoMap) {
			if(wdaoMap.containsKey(curl)) {
				return wdaoMap.get(curl);
			}
		}

		try {
			// use dao from delegate (logically: base) implementation so we can change objects already active i.e. if Manager creates DAO /MACI/Managers/Manager
			// later we instantiate a WDAO of the same curl and change values that Manager looks at 
			org.omg.PortableServer.Servant daoServant = poa.reference_to_servant(dalImpl.get_DAO_Servant(curl));
			DAOImpl daoImpl = (DAOImpl) ((DAOPOATie)daoServant)._delegate();

			WDAOImpl wdaoImpl = new WDAOImpl(this, curl, daoImpl, poa, logger);

			// create object id
			String name = "WDAO-" + curl;
			byte[] id = name.getBytes();

			// activate object
			poa.activate_object_with_id(id, wdaoImpl);

			WDAO href = WDAOHelper.narrow(poa.servant_to_reference(wdaoImpl));

			// map reference
			wdaoMap.put(curl, href);

			return href;
		} catch(Throwable t) {
			String info = "WDAL::get_WDAO_Servant " + t;
			CDBXMLErrorEx xmlErr = new CDBXMLErrorEx();
			logger.log(AcsLogLevel.NOTICE, info);
			throw xmlErr;
		}
	}

	/**
	 * Adds a new node specified by curl to the CDB initially filed with
	 *
	 * @param curl uri for the CDB node
	 * @param xml
	 *
	 * @throws CDBRecordAlreadyExistsEx
	 * @throws CDBXMLErrorEx
	 * @throws CDBExceptionEx
	 */
	public void add_node(String curl, String xml)
		throws CDBRecordAlreadyExistsEx, CDBXMLErrorEx, CDBExceptionEx
	{
		dalImpl.totalDALInvocationCounter.incrementAndGet();

		logger.log(AcsLogLevel.INFO, "add_node " + curl);

		// check if node is already there
		if(nodeExists(curl)) {
			logger.log(AcsLogLevel.NOTICE, "Record already exists: " + curl);
			AcsJCDBRecordAlreadyExistsEx e2 = new AcsJCDBRecordAlreadyExistsEx();
			e2.setCurl(curl);
			e2.log(logger);
			throw e2.toCDBRecordAlreadyExistsEx();
		}

		// check that suplied xml is valid 
		try{
			validateXML(xml);
		}catch(AcsJCDBXMLErrorEx e){
			e.log(logger);
			throw e.toCDBXMLErrorEx();
		}
		// recreate dir structure and put data content 
		getNodeFile(curl).getParentFile().mkdirs();
		try{
		// write content
		writeXmlData(curl, xml);
		}catch(AcsJCDBExceptionEx e){
			throw e.toCDBExceptionEx();
		}
	}

	/**
	 * Removes node identified by curl by deleting its file and directory if
	 * empty.
	 *
	 * @param curl uri for the CDB node
	 *
	 * @throws CDBRecordDoesNotExistEx
	 * @throws CDBRecordIsReadOnlyEx
	 */
	public void remove_node(String curl)
		throws CDBRecordDoesNotExistEx, CDBRecordIsReadOnlyEx
	{
		dalImpl.totalDALInvocationCounter.incrementAndGet();

		logger.log(AcsLogLevel.INFO, "remove_node " + curl);

		// check if node exists
		if(!nodeExists(curl)) {
			logger.log(AcsLogLevel.NOTICE, "Record does not exist: " + curl);
			throw new CDBRecordDoesNotExistEx();
		}

		// if so delete nodes file
		File xmlFile = getNodeFile(curl);
		boolean deleted = xmlFile.delete();

		// if we can't delete it assume that it is read only
		if(!deleted) {
			throw new CDBRecordIsReadOnlyEx();
		}

		// also delete directory node but don't care if it is not empty and we actually didn't delete it  
		xmlFile.getParentFile().delete();

		// let other new the node gone
		clear_cache(curl);
	}

	/**
	 * Change content of a node identified by curl so given xml is scanned for
	 * differences which are applied. This function can be invoked with full
	 * expanded version of the existing xml with some changes or it can be
	 * invoked by small xml with only changes to be applied. For example to
	 * change parameter 'Timeot' in Manager we can pass as xml
	 * <pre>
	 * <code>
	 *     <?xml version="1.0" encoding="ISO-8859-1"?>
	 *  <Manager Timeout="50.0"/>
	 * </code>
	 * </pre>
	 * and new value will be saved in the xml file.
	 *
	 * @param curl uri for the CDB node
	 * @param xml
	 *
	 * @throws CDBRecordDoesNotExistEx
	 * @throws CDBFieldDoesNotExistEx
	 * @throws CDBRecordIsReadOnlyEx
	 * @throws CDBXMLErrorEx
	 * @throws CDBExceptionEx
	 */
	public void set_DAO(String curl, String xml)
		throws CDBRecordDoesNotExistEx, CDBFieldDoesNotExistEx, CDBRecordIsReadOnlyEx, 
			CDBXMLErrorEx, CDBExceptionEx
	{
		dalImpl.totalDALInvocationCounter.incrementAndGet();

		logger.log(AcsLogLevel.INFO, "set_DAO " + curl);

		// check if node exists
		if(!nodeExists(curl)) {
			logger.log(AcsLogLevel.NOTICE, "Record does not exist: " + curl);
			throw new CDBRecordDoesNotExistEx();
		}

		File xmlFile = getNodeFile(curl);

		if(!xmlFile.canWrite()) {
			throw new CDBRecordIsReadOnlyEx();
		}

		// read given xml and iterate through its content and check if something was changed
		DAOImpl daoImp = null;
		XMLHandler daoXMLSolver = null;

		// get content of the given xml string using parser without any shemas and validation
		// since given xml string come from a client that have no shemas and it is full expanded version
		// of existing xml or it is smal composed xml of few properties
		XMLHandler xmlSolver = new XMLHandler(false, logger);
		// TODO markArrays == 2 impl. is a mess... I think lot of code could be removed!
		//xmlSolver.setMarkArrays(2);
		parseXML(xml, xmlSolver);

		// get original xml that we will use to compare
		xml = dalImpl.get_DAO(curl);
		daoXMLSolver = new XMLHandler(false, logger);
		parseXML(xml, daoXMLSolver);
		daoImp = new DAOImpl(curl, daoXMLSolver.m_rootNode, poa, logger);

		// iterater throuth given xml and put changed attributes in map
		LinkedHashMap map = new LinkedHashMap();
		try{
			checkforChanges("", xmlSolver.m_rootNode, map, daoImp);
			saveChanges(curl, map);
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}catch(AcsJCDBXMLErrorEx e){
			throw e.toCDBXMLErrorEx();
		}catch(AcsJCDBExceptionEx e){
			throw e.toCDBExceptionEx();
		}
	}

	private void parseXML(String xml, XMLHandler xmlSolver)
		throws CDBXMLErrorEx
	{
		try {
			SAXParserFactory factory = SAXParserFactory.newInstance();
			SAXParser saxParser = factory.newSAXParser();
			saxParser.parse(new InputSource(new StringReader(xml)), xmlSolver);

			if(xmlSolver.m_errorString != null) {
				String info = "XML parser error: " + xmlSolver.m_errorString;
				CDBXMLErrorEx xmlErr = new CDBXMLErrorEx();
				logger.log(AcsLogLevel.NOTICE, info);
				throw xmlErr;
			}
		} catch(Throwable t) {
			String info = "SAXException " + t;
			CDBXMLErrorEx xmlErr = new CDBXMLErrorEx();
			logger.log(AcsLogLevel.NOTICE, info);
			throw xmlErr;
		}
	}

	/**
	 * Writes given xml to disk overwriting eventualy existing file
	 *
	 * @param curl uri for the CDB node
	 * @param xmlData xml to write
	 *
	 * @throws CDBExceptionEx general CDB exception if something goes wrong while
	 *         saving
	 */
	private synchronized void writeXmlData(String curl, String xmlData)
		throws AcsJCDBExceptionEx
	{
		File xmlFile = getNodeFile(curl);

		try {
			xmlFile.createNewFile();

			FileWriter fw = new FileWriter(xmlFile);
			fw.write(xmlData);
			fw.close();
		} catch(IOException e) {
			e.printStackTrace();
			
			AcsJCDBExceptionEx e2 = new AcsJCDBExceptionEx(e);
			//e2.setCurl(curl);
			throw e2;
			//throw new CDBExceptionEx("Exception while writing node " + curl
			  //  + " : " + e);
		}

		// let other new about change
		clear_cache(curl);
	}

	/**
	 * Recursively scans nodes and check every property with current xml 
	 *
	 * @param name
	 * @param node
	 * @param map
	 * @param dao
	 *
	 * @throws AcsJCDBFieldDoesNotExistEx
	 * @throws AcsJCDBXMLErrorEx
	 */
	private void checkforChanges(String name, XMLTreeNode node, Map map,
	    DAOImpl dao) throws AcsJCDBFieldDoesNotExistEx, AcsJCDBXMLErrorEx
	{
		String propertyName;
		String currentValue;
		String value;

		// if this node represents an array then add its contents into map
		// this will be the case when we cheking full expanded version of a XML
		//if(node.isArrayNode()) {
		//	XMLTreeNode arrNode = (XMLTreeNode)node.m_subNodesMap.get(XMLTreeNode.ARRAY_MARKER);

		for(Iterator iter = node.m_subNodesMap.keySet().iterator(); iter.hasNext();) {
			String key = (String)iter.next();
			XMLTreeNode childNode = (XMLTreeNode)node.m_subNodesMap.get(key);
			if(childNode.isMapNode()){
				for(Iterator iterator = childNode.m_fieldMap.keySet().iterator();iterator.hasNext();) {
				String childKey = (String)iterator.next();
					map.put(XMLTreeNode.MAP_TYPE + "/" + key + "/"
					    + childKey, childNode.m_fieldMap.get(childKey));
				}
				node.m_subNodesMap.clear();
			}
		}

		// node attributes i.e 'CommandLine' in node 'Manager'
		for(Iterator iter = node.m_fieldMap.keySet().iterator();
		    iter.hasNext();) {
			String key = (String)iter.next();
			propertyName = name + "/" + key;

			try {
				currentValue = dao.get_field_data(propertyName);
			} catch(Exception e) {
				
				// TODO additional elements in maps will cause an exception... they are not supported
				// TODO also if an element is removed, this will not be detected
				
				e.printStackTrace();

				AcsJCDBXMLErrorEx xmlErr = new AcsJCDBXMLErrorEx(e);
				throw xmlErr;
			}

			value = (String)node.m_fieldMap.get(key);

			if(!value.equals(currentValue)) {
				map.put(propertyName, value);
			}
		}

		// subnodes for this node i.e. 'current' for 'TEST_PS_1'
		for(Iterator iter = node.m_subNodesMap.keySet().iterator();
		    iter.hasNext();) {
			String key = (String)iter.next();
			checkforChanges(name + "/" + key,
			    (XMLTreeNode)node.m_subNodesMap.get(key), map, dao);
		}
	}

	/**
	 * Returns true if node specified with curl exists
	 *
	 * @param curl uri of the CDB node
	 *
	 * @return true if node exists false otherwise
	 */
	public boolean nodeExists(String curl)
	{
		return getNodeFile(curl).exists();
	}

	/**
	 * Returns File object for given curl
	 *
	 * @param curl uri of the CDB node
	 *
	 * @return File object for CDB node
	 */
	public File getNodeFile(String curl)
	{
		String xmlPath = dalImpl.getRecordPath(curl);
		logger.log(AcsLogLevel.DEBUG,"getNodeFile("+curl+") = "+ xmlPath);
		return new File(xmlPath);
	}

	/**
	 * Save changes given by map to the node identified by curl
	 *
	 * @param curl
	 * @param propertyMap
	 *
	 * @throws CDBXMLErrorEx
	 * @throws CDBExceptionEx
	 * @throws CDBFieldDoesNotExistEx
	 */
	public void saveChanges(String curl, Map propertyMap)
		throws AcsJCDBXMLErrorEx, AcsJCDBExceptionEx, AcsJCDBFieldDoesNotExistEx
	{
		if(!nodeExists(curl)) {
			return; // we can't save merged curl
		}

		WriteXMLHandler xmlHandler = null;

		try {
			// create plain parser 
			SAXParserFactory factory = SAXParserFactory.newInstance();
			SAXParser saxParser = factory.newSAXParser();

			xmlHandler = new WriteXMLHandler(propertyMap);

			Object lexicalParser = saxParser.getProperty(
				    "http://xml.org/sax/properties/lexical-handler");
			saxParser.setProperty("http://xml.org/sax/properties/lexical-handler",
			    xmlHandler);

		        logger.log(AcsLogLevel.DEBUG,"saveChanges('"+curl+"'): - Parsing");  
			saxParser.parse(getNodeFile(curl), xmlHandler);
		} catch(Exception e) {
			e.printStackTrace();
			
			AcsJCDBXMLErrorEx e2 = new AcsJCDBXMLErrorEx(e);
			e2.setCurl(curl);
			//throw new CDBXMLErrorEx(e.toString());
			throw e2;
		}

		StringWriter sw = new StringWriter();
		xmlHandler.writeXML(sw);
	//	try{	
			// now check that everything conforms to the schema
			validateXML(sw.toString());
			// ok it is safe now to write it to disk
			writeXmlData(curl, sw.toString());
	//	}catch(AcsJCDBXMLErrorEx e){
		//	throw e;
		//}catch(AcsJCDBException e){
		//	thro
		//}
	}

	/**
	 * Check that everything conforms to the schema in given xml. This check
	 * will be done by parser used in DAL.
	 *
	 * @param xml
	 *
	 * @throws AcsJCDBXMLErrorEx
	 */
	public void validateXML(String xml) throws AcsJCDBXMLErrorEx
	{
		try {
			XMLHandler dalSolver = new XMLHandler(true, logger);
			SAXParser saxParser = dalImpl.getSaxParser();
			saxParser.parse(new InputSource(new StringReader(xml)), dalSolver);

			if(dalSolver.m_errorString != null) {
				logger.log(AcsLogLevel.NOTICE, dalSolver.m_errorString);
				AcsJCDBXMLErrorEx e2 = new AcsJCDBXMLErrorEx();
				e2.setErrorString(dalSolver.m_errorString);	
				throw e2;
			}
		} catch(AcsJCDBXMLErrorEx e) {
			throw e;
		} catch(Throwable t) {
			t.printStackTrace();

			AcsJCDBXMLErrorEx e2 = new AcsJCDBXMLErrorEx(t);
			throw e2;
			//CDBXMLErrorEx xmlErr = new CDBXMLErrorEx(t.toString());
			//throw xmlErr;
		}
	}

	/**
	 * Extended handler that keep track of arrays inside nodes and can write
	 * xml to the given writer after changes are appliead.
	 */
	private class WriteXMLHandler extends XMLHandler implements LexicalHandler
	{
		private Writer writer = null;
		private Map propertyMap = null;
		private String existingComment = null;
		private HashMap arrayMap = null;

		/**
		 * Creates a new WriteXMLHandler object.
		 *
		 * @param propertyMap map of changed properties
		 */
		public WriteXMLHandler(Map propertyMap)
		{
			super(false, logger);
			this.propertyMap = propertyMap;
			setMarkArrays(1); // just make a holder
		}

		/**
		 * Writes content of xml to the specified writer
		 *
		 * @param writer writer to write to
		 *
		 * @throws CDBFieldDoesNotExistEx
		 * @throws CDBExceptionEx
		 */
		public void writeXML(Writer writer)
			throws AcsJCDBFieldDoesNotExistEx, AcsJCDBExceptionEx
		{
			this.writer = writer;

			// applychanges from given map
			for(Iterator iter = propertyMap.keySet().iterator();
			    iter.hasNext();) {
				String propertyName = (String)iter.next();
				String value = (String)propertyMap.get(propertyName);
				setField(propertyName, value, false);
			}

			// NOTE: be sure that this does not override map elements !!!
			// handle arrays
			if(arrayMap != null) {
				for(Iterator iter = arrayMap.keySet().iterator();
				    iter.hasNext();) {
					String key = (String)iter.next();
					setField(key, null, true);
				}
			}
			try {
				write("<?xml version='1.0' encoding='ISO-8859-1'?>\n");
				writeComment();
				writeNode(m_rootNode, 0);
			} catch(IOException e) {
				e.printStackTrace();
				throw new AcsJCDBExceptionEx(e);
			}
		}

		private void write(String string) throws IOException
		{
			writer.write(string);
		}

		private void writeComment() throws IOException
		{
			boolean historyExists = false;

			if(existingComment == null) {
				existingComment = "";
			} else {
				historyExists = existingComment.indexOf("- History:") != -1;
			}

			if(!historyExists) {
				existingComment += "\n   - History:\n";
			}

			existingComment += "   -   " + new java.util.Date()
			+ " modified by jDAL\n";

			write("<!--" + existingComment + "-->\n");
		}

		private void setField(String strFieldName, String value, boolean asArray)
			throws AcsJCDBFieldDoesNotExistEx
		{
			XMLTreeNode pNode = m_rootNode;
			StringTokenizer st = new StringTokenizer(strFieldName, "/");
			String fieldName = st.nextToken();

			while(st.hasMoreTokens()) {
				if(pNode.m_subNodesMap.get(fieldName) == null
				    && strFieldName.startsWith(XMLTreeNode.MAP_TYPE)) {
					XMLTreeNode newNode = new XMLTreeNode(pNode);
					newNode.m_name = fieldName;
					pNode.m_subNodesMap.put(fieldName, newNode);
				}

				pNode = (XMLTreeNode)pNode.m_subNodesMap.get(fieldName);

				if(pNode == null) { // is this attempt to modify merged xml?
					AcsJCDBFieldDoesNotExistEx e2 = new AcsJCDBFieldDoesNotExistEx();
					e2.setFieldName(strFieldName);
					throw e2;
				}

				fieldName = st.nextToken();
			}

			if(asArray) {
				if(value == null) {
					value = (String)pNode.m_fieldMap.get(fieldName);
				}

				/*if(pNode.isArray()) { // the hole node is an array so change back its names

					XMLTreeNode arrNode = (XMLTreeNode)pNode.m_subNodesMap.get(XMLTreeNode.ARRAY_MARKER);
				//	pNode.m_subNodesMap.remove(XMLTreeNode.ARRAY_MARKER); // remove it in any case to avoid its writing back to the XML

					if(arrNode.m_subNodesMap.size() > 0) {
						pNode.m_subNodesMap.clear();
						pNode.m_subNodesMap.putAll(arrNode.m_subNodesMap);
					}

					for(Iterator iter = pNode.m_subNodesMap.keySet().iterator();
					    iter.hasNext();) {
						String key = (String)iter.next();
						XMLTreeNode childNode = (XMLTreeNode)pNode.m_subNodesMap
							.get(key);
						childNode.m_name = "_";
					}
				} else {
					pNode.m_fieldMap.remove(fieldName);

					XMLTreeNode newNode = new XMLTreeNode(pNode);
					newNode.m_name = fieldName;
					newNode.m_isArray = true;
					newNode.m_fieldMap.put(fieldName, value);
					pNode.m_subNodesMap.put(fieldName, newNode);
				}*/
			} else {
				pNode.m_fieldMap.put(fieldName, value);
			}
		}

		private void writeIndent(int indent) throws IOException
		{
			for(int i = 0; i < indent; i++) {
				write("\t");
			}
		}

		private void writeNode(XMLTreeNode node, int indent)
			throws IOException
		{
			write(node.toString(false));	
	/*			writeIndent(indent);
			
			// is map element?
			if(node.m_parent != null &&
			   node.m_parent.m_subNodesMap.containsKey(XMLTreeNode.ARRAY_MARKER))
				if(node.m_nameSpace.equals(""))
					write("<_");
				else
					write("<"+node.m_nameSpace+":_");
			else
				write("<" + node.m_name);

			if(node.m_isArray) { // this is the array of primitive types like defined in CDB.xsd

				String arrayType = (String)arrayMap.get(node.m_name);
				String value = (String)node.m_fieldMap.get(node.m_name);
				StringTokenizer st = new StringTokenizer(value, ",");
				write(">\n");

				if(!st.hasMoreTokens()) { // in case of empty array they must provide at least one empty element to be conformant with schema
					writeIndent(indent + 1);
					write("<cdb:_ " + arrayType + "=\"\"/>\n");
				}

				while(st.hasMoreTokens()) {
					writeIndent(indent + 1);
					write("<cdb:_ " + arrayType + "=\"" + st.nextToken()
					    + "\"/>\n");
				}

				writeIndent(indent);
			} else { // this nod is plain node with its attributes and eventually child elements

				for(Iterator iter = node.m_fieldMap.keySet().iterator();
				    iter.hasNext();) {
					String key = (String)iter.next();
					write(" " + key + "=\"" + node.m_fieldMap.get(key) + "\"");
				}

				// just close element
				if (node.m_subNodesMap.size() == 0) {
					write(" />\n");
					return;
				}

				write(">");
			}

			if(node.m_subNodesMap.size() > 0) {
				write("\n");
			}

			// subnodes for this node i.e. 'current' for 'TEST_PS_1'
			for(Iterator iter = node.m_subNodesMap.keySet().iterator();
			    iter.hasNext();) {
				String key = (String)iter.next();
				// skip array marker (for maps)
				if (!XMLTreeNode.ARRAY_MARKER.equals(key))
					writeNode((XMLTreeNode)node.m_subNodesMap.get(key), indent + 1);
			}

			if(node.m_subNodesMap.size() > 0) {
				writeIndent(indent);
			}

			write("</" + node.m_name + ">\n");
		*/
		}

		private void putArrayMap(String key, String value)
		{
			if(arrayMap == null) {
				arrayMap = new HashMap();
			}

			if(!arrayMap.containsKey(key)) {
				arrayMap.put(key, value);
			}
		}

		/* (non-Javadoc)
		 * @see org.xml.sax.ContentHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
		 */
		public void startElement(String uri, String localName, String qName,
		    Attributes attributes) throws SAXException
		{
			super.startElement(uri, localName, qName, attributes);

			// if we are in array procesing just rememebr array name and type
			if(m_parent.isArrayNode())
			//if(m_parent.m_parent != null && m_parent.m_parent.m_isArray) {
				putArrayMap(m_parent.m_name, attributes.getQName(0));
				//putArrayMap(m_parent.m_parent.m_name, attributes.getQName(0));
			//}
			
			/*	
			// if node names are '_' we change its name to the first attribute so we must keep track of that
			if(qName.equals("_") && m_parent.m_parent != null) {
			    String nameString = attributes.getValue("Name");
			    // get first
			    if (nameString == null)
			        nameString = attributes.getQName(0);
				putArrayMap(m_parent.m_parent.m_name, nameString);
				//putArrayMap(m_parent.m_parent.m_name, attributes.getQName(0));
			}
			*/
		}

		/* (non-Javadoc)
		 * @see org.xml.sax.ext.LexicalHandler#comment(char[], int, int)
		 */
		public void comment(char[] ch, int start, int length)
			throws SAXException
		{
			existingComment = new String(ch, start, length);
		}

		/* (non-Javadoc)
		 * @see org.xml.sax.ext.LexicalHandler#endCDATA()
		 */
		public void endCDATA() throws SAXException
		{
		}

		/* (non-Javadoc)
		 * @see org.xml.sax.ext.LexicalHandler#endDTD()
		 */
		public void endDTD() throws SAXException
		{
		}

		/* (non-Javadoc)
		 * @see org.xml.sax.ext.LexicalHandler#endEntity(java.lang.String)
		 */
		public void endEntity(String name) throws SAXException
		{
		}

		/* (non-Javadoc)
		 * @see org.xml.sax.ext.LexicalHandler#startCDATA()
		 */
		public void startCDATA() throws SAXException
		{
		}

		/* (non-Javadoc)
		 * @see org.xml.sax.ext.LexicalHandler#startDTD(java.lang.String, java.lang.String, java.lang.String)
		 */
		public void startDTD(String name, String publicId, String systemId)
			throws SAXException
		{
		}

		/* (non-Javadoc)
		 * @see org.xml.sax.ext.LexicalHandler#startEntity(java.lang.String)
		 */
		public void startEntity(String name) throws SAXException
		{
		}
	}
	
}

/* __oOo__ */
