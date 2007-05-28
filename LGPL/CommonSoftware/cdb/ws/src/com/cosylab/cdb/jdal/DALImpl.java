package com.cosylab.cdb.jdal;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.xml.sax.SAXException;

import com.cosylab.CDB.*;
import com.cosylab.util.FileHelper;

import alma.cdbErrType.CDBXMLErrorEx;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;
import alma.cdbErrType.wrappers.AcsJCDBRecordDoesNotExistEx;

import alma.acs.logging.ClientLogManager;
import alma.acs.logging.AcsLogLevel;

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

public class DALImpl extends JDALPOA implements Recoverer {
	/** Schema validation feature id (http://apache.org/xml/features/validation/schema). */
	protected static final String SCHEMA_VALIDATION_FEATURE_ID =
		"http://apache.org/xml/features/validation/schema";

	/** Property for setting the mapping of URIs to XML schemas. */
	protected static final String EXTERNAL_SCHEMA_LOCATION_PROPERTY_ID =
		"http://apache.org/xml/properties/schema/external-schemaLocation";

	private ORB orb;
	private POA poa;
	SAXParserFactory factory;
	SAXParser saxParser;
	static String m_root;
	private HashMap daoMap = new HashMap();

	// clean cache implementation
	private HashMap listenedCurls = new HashMap();
	File listenersStorageFile = null;
	Random idPool = new Random();
	private HashMap regListeners = new HashMap();
	private boolean recoveryRead = true;
	private DALNode rootNode = null; // used for node lisitng 
	Logger m_logger;

	DALImpl(String args[], ORB orb_val, POA poa_val) {
		orb = orb_val;
		poa = poa_val;
	  	m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("CDB::DALImpl", true);	
		m_root = "CDB";

		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-root")) {
				if (i < args.length - 1) {
					m_root = args[++i] + "/CDB";
				}
			}
			if (args[i].equals("-n")) {
				recoveryRead = false;
			}
		}

		File rootF = new File(m_root);
		m_root = rootF.getAbsolutePath() + '/';
		if (Integer.getInteger("ACS.logstdout", 4) < 4)
		    {
			m_logger.log(AcsLogLevel.INFO, "DAL root is: " + m_root);
		    }
		loadFactory();
	}
	public void loadFactory(){
		factory = SAXParserFactory.newInstance();
		try {
			try {
				factory.setNamespaceAware(true);
				factory.setValidating(true);
				factory.setFeature(SCHEMA_VALIDATION_FEATURE_ID, true);
			} catch (IllegalArgumentException x) {
				// This can happen if the parser does not support JAXP 1.2
				m_logger.log(AcsLogLevel.NOTICE, "Check to see if parser conforms to JAXP 1.2 spec.");
				m_logger.log(AcsLogLevel.NOTICE, "Error is:" + x);
				System.exit(1);
			}
			// Create the parser
			saxParser = factory.newSAXParser();
			// find out all schemas
			String allURIs = getSchemas();
			if (Integer.getInteger("ACS.logstdout", 4) < 4)
			    {
				m_logger.log(AcsLogLevel.INFO, "- created parser "+saxParser.getClass().getName()+" with schema location: '" + allURIs +"'");//msc:added
			    }
			if (allURIs != null) {
				saxParser.setProperty(EXTERNAL_SCHEMA_LOCATION_PROPERTY_ID, allURIs);
			} else {
				m_logger.log(AcsLogLevel.NOTICE,"Schema files: NO SCHEMAS!");
			}

		} catch (Throwable t) {
			t.printStackTrace();
		}
	}

	/**
	 * Recovery related implementation.
	 * Load list of listeners from the recovery file and notifies them to clear cache.
	 * NOTICE: This method should be called when DAL POA is alrady initialized and active.
	 * NOTE: Method execution depends on <code>recoveryRead</code> variable.
	 */
	public void recoverClients()
	{
		if( recoveryRead ) {
			// load listeners and notify them
			loadListeners();
			new Thread (){
				public void run() {
					clear_cache_all();
				}
			}.start();
		}
	 }

	/**
	 * returns string of URIs separated by ' ' for all schema files in root/schemas and
	 * directories list given by ACS.cdbpath environment variable
	 */
	private String getSchemas() {
		String fileName, filePath;

		// we will use only first fileName we found in search path
		LinkedHashMap filePathMap = new LinkedHashMap();

		// first get schemas from root/schemas directory
		getFiles(m_root + "schemas", filePathMap);

	  int dbg_nRoot = filePathMap.size();
	  if (Integer.getInteger("ACS.logstdout", 4) < 4)
	      {	  
		m_logger.log(AcsLogLevel.INFO, "- found "+dbg_nRoot+" xsd-files in <-root>");//msc:added
	      }
		// then all given path if any
		String pathList = System.getProperty("ACS.cdbpath");
		if (pathList != null) {
			StringTokenizer st = new StringTokenizer(pathList, File.pathSeparator);//msc:fixed
			while (st.hasMoreTokens()) {
				filePath = st.nextToken();
			getFiles(filePath, filePathMap);
			}
		}
	  if (Integer.getInteger("ACS.logstdout", 4) < 4)
	      {
		m_logger.log(AcsLogLevel.INFO,"- found "+(filePathMap.size()-dbg_nRoot)+" xsd-files in <ACS.cdbpath>");//msc:added  
	      }
		// from file list build return string
		Iterator i = filePathMap.keySet().iterator();
		if (!i.hasNext())
			return null; // no schemas found

		StringBuffer allURIs = new StringBuffer(1024);
		while (i.hasNext()) {
			fileName = (String) i.next();
			filePath = (String) filePathMap.get(fileName);
         
			// trim extension
			fileName = fileName.substring(0, fileName.length() - 4);
			// compose with spaces
			allURIs.append("urn:schemas-cosylab-com:").append(fileName).append(":1.0 ").
					append(filePath).append(' ');
		}
		return allURIs.toString();
	}

	/**
	 * Adds all *.xsd files from filePath directory 
	 * if file already exists in map it is skipped
	 */
	private void getFiles(String filePath, LinkedHashMap map) {
		String fileName;
		File base = new File(filePath);
		File[] basefiles = base.listFiles(new Filter());
		if (basefiles == null)
			return;
		for (int i = 0; i < basefiles.length; i++) {
			fileName = basefiles[i].getName();
			if (map.get(fileName) != null)
				continue; // we already found the file
			map.put(fileName, basefiles[i].getPath());
		}
	}

	/**
	 * Filter which selects all xsd files in the root/schemas directory.
	 */
	private class Filter implements FilenameFilter {
		public Filter() {
		}
		public boolean accept(File arg0, String arg1) {
			if (arg1.endsWith(".xsd"))
				return true;
			else
				return false;
		}
	}

	public String getRecordPath(String curl) {
		String objectName, path;

		int slashPos = curl.lastIndexOf('/', curl.length() - 1);
		if (slashPos == -1)
			objectName = curl;
		else
			objectName = curl.substring(slashPos);
		// path is root + curl; Containers/Container -> ./DALData/Containers/Container/Container.xml
		path = m_root + curl + "/" + objectName + ".xml";
		return path;
	}
	
	public void parseNode(DALNode node, XMLHandler xmlSolver) throws SAXException, IOException, AcsJCDBXMLErrorEx {
		DALNode[] childs = node.getChilds();
		DALNode curlNode = node.getCurlNode();
		if(curlNode != null) {
			String xmlPath = getRecordPath(node.getCurl());
			File xmlFile = new File(xmlPath);
			xmlSolver.setFirstElement(node.name);
			saxParser.parse(xmlFile, xmlSolver);
		
			if (xmlSolver.m_errorString != null) {
				String info = "XML parser error: " + xmlSolver.m_errorString;
				AcsJCDBXMLErrorEx cdbxmlErr = new AcsJCDBXMLErrorEx();
				cdbxmlErr.setFilename(xmlPath);
				cdbxmlErr.setNodename(node.name);
				cdbxmlErr.setCurl(node.getCurl());
				cdbxmlErr.setErrorString(info);
				m_logger.log(AcsLogLevel.NOTICE,info); 
				throw cdbxmlErr;
			}
		}
		else {
			xmlSolver.startDocument();
			xmlSolver.startElement(null, null, node.name, new org.xml.sax.helpers.AttributesImpl());
		}
		// and childs if exist
		for (int i = 0; i < childs.length; i++) {
			parseNode(childs[i], xmlSolver);
		}
		xmlSolver.closeElement();
	}

	/**
	 * Returns a xml constructed of all records below given curl
	 * 
	 * @param curl
	 * @param toString
	 * @return
	 * @throws AcsJCDBRecordDoesNotExistEx
	 * @throws AcsJCDBXMLErrorEx
	 */
	public XMLHandler loadRecords(String curl, boolean toString)
		throws AcsJCDBRecordDoesNotExistEx, AcsJCDBXMLErrorEx{

		// create hierarchy of all nodes if it is not created yet 
		if( rootNode == null) {
			//no Error throwed
			rootNode = DALNode.getRoot(m_root);
		}

		//no Error throwed
		String strFileCurl = curl;
		String strNodeCurl = "";
		DALNode curlNode = null;
		while(strFileCurl != null){
		    curlNode = rootNode.findNode(strFileCurl);
		    if(curlNode == null) {
			if(strFileCurl.lastIndexOf('/') >0){
			    strNodeCurl = strFileCurl.substring(strFileCurl.lastIndexOf('/')+1) + "/"+ strNodeCurl; 
		    	    strFileCurl = strFileCurl.substring(0,strFileCurl.lastIndexOf('/'));
		    	}else strFileCurl = null;
		    }else break;
		}
		
		m_logger.log(AcsLogLevel.DEBUG, "loadRecords(curl="+curl+"), strFileCurl="+strFileCurl+", strNodeCurl="+ strNodeCurl);
	    	if(strFileCurl == null) {
		    m_logger.log(AcsLogLevel.NOTICE,"Record does not exists: " + curl);
		    AcsJCDBRecordDoesNotExistEx recordDoesNotExist = 
		        new AcsJCDBRecordDoesNotExistEx();
		    recordDoesNotExist.setCurl(curl);
		    throw recordDoesNotExist; 
	        }
		//no Error throwed
		if(curlNode.isSimple()){
			m_logger.log(AcsLogLevel.DEBUG, "loadRecords(curl="+curl+"), curlNode is Simple");
			if(curl.equals(strFileCurl)){
			     return loadRecord(strFileCurl, toString);
			}else{
			    XMLHandler xmlSolver = loadRecord(strFileCurl, false);
			    try{
			    return xmlSolver.getChild(strNodeCurl);
			    }catch(AcsJCDBRecordDoesNotExistEx e){
			    	e.setCurl(strFileCurl+e.getCurl());
			    	throw e;
			    }
			}
		}
		m_logger.log(AcsLogLevel.DEBUG, "loadRecords(curl="+curl+"), curlNode is Complex");
		XMLHandler xmlSolver;
		try {
			//xmlSolver.startDocument();
			//xmlSolver.startElement(null,null,"curl",new org.xml.sax.helpers.AttributesImpl);
 			if(curl.equals(strFileCurl)){
			    xmlSolver = new XMLHandler(toString);
			    xmlSolver.setAutoCloseStartingElement(false);
			    parseNode(curlNode, xmlSolver);
			//xmlSolver.closeElement();
			     return xmlSolver;
			}else{
			    //here we must to return the node inside the xmlSolver with curl= strNodeCurl		
			    xmlSolver = new XMLHandler(false);
		 	    xmlSolver.setMarkArrays(1);
			    xmlSolver.setAutoCloseStartingElement(false);
			    parseNode(curlNode, xmlSolver);
			    return xmlSolver.getChild(strNodeCurl);
			}
		} catch (AcsJCDBXMLErrorEx e) {
			e.setCurl(curl);
			throw e;
		} catch (AcsJCDBRecordDoesNotExistEx e){
			e.setCurl(strFileCurl+e.getCurl());
			throw e;
		} catch (Throwable t) {
			String info = "SAXException " + t;
		        //CDBXMLError xmlErr = new CDBXMLError(info);	
			AcsJCDBXMLErrorEx cdbxmlErr = new AcsJCDBXMLErrorEx(t);
			cdbxmlErr.setErrorString(info);
			m_logger.log(AcsLogLevel.INFO,info); 
			throw cdbxmlErr;
		}
	}

	private XMLHandler loadRecord(String curl, boolean toString)
		throws AcsJCDBRecordDoesNotExistEx, AcsJCDBXMLErrorEx {
	    return loadRecord(curl, toString, false);
	}
	
	private XMLHandler loadRecord(String curl, boolean toString, boolean silent)
		throws AcsJCDBRecordDoesNotExistEx, AcsJCDBXMLErrorEx {
		String xmlPath = getRecordPath(curl);
		File xmlFile = new File(xmlPath);
		if (!xmlFile.exists()) {
			if (!silent) m_logger.log(AcsLogLevel.NOTICE,"Record does not exists: " + curl); 
			AcsJCDBRecordDoesNotExistEx recordDoesNotExist = 
			    new AcsJCDBRecordDoesNotExistEx();
			recordDoesNotExist.setCurl(curl);
			throw recordDoesNotExist; 
		}

		XMLHandler xmlSolver = new XMLHandler(toString);
		xmlSolver.setMarkArrays(1);
		try {
			m_logger.log(AcsLogLevel.DEBUG, "Parsing xmlFile="+xmlFile);
			saxParser.parse(xmlFile, xmlSolver);
			if (xmlSolver.m_errorString != null) {
				String info = "XML parser error: " + xmlSolver.m_errorString;
				if (!silent) m_logger.log(AcsLogLevel.NOTICE, info);
				
				//CDBXMLError xmlErr = new CDBXMLError(info);
				AcsJCDBXMLErrorEx cdbxmlErr = new AcsJCDBXMLErrorEx();
				cdbxmlErr.setFilename(xmlPath);
				cdbxmlErr.setCurl(curl);
				cdbxmlErr.setErrorString(info);
				throw cdbxmlErr;

			}
			return xmlSolver;
		} catch (AcsJCDBXMLErrorEx cdbxmlErr) {
			throw cdbxmlErr;
		} catch (Throwable t) {
			String info = "SAXException " + t;
			if (!silent) m_logger.log(AcsLogLevel.NOTICE, info);
			//CDBXMLError xmlErr = new CDBXMLError(info);
			AcsJCDBXMLErrorEx cdbxmlErr = new AcsJCDBXMLErrorEx(t);
			cdbxmlErr.setCurl(curl);
			cdbxmlErr.setErrorString(info);
			throw cdbxmlErr;
		}
	}

	/**
	 * returns full expanded XML string 
	 */
	public synchronized String get_DAO(String curl) throws CDBRecordDoesNotExistEx, CDBXMLErrorEx {
		try{
		    if(curl.lastIndexOf('/') == curl.length()-1)
			curl = curl.substring(0,curl.length()-1);
		    XMLHandler xmlSolver = loadRecords(curl, true);
		if (xmlSolver == null)
			return null;
		
		m_logger.log(AcsLogLevel.INFO, "Returning XML record for: " + curl);
		return xmlSolver.toString(false);
		}catch(AcsJCDBXMLErrorEx e){
			String smsg = "XML Error \tCURL='" + e.getCurl()+"'\n\t\tFilename='"+e.getFilename()+"'\n\t\tNodename='"+e.getNodename()+"'\n\t\tMSG='"+e.getErrorString()+"'";
			m_logger.log(AcsLogLevel.NOTICE, smsg, e);	
			throw e.toCDBXMLErrorEx();
		}catch(AcsJCDBRecordDoesNotExistEx e){
			throw e.toCDBRecordDoesNotExistEx();
		}
	}

	/**
	 * create DAO servant with requested XML
	 */
	public synchronized DAO get_DAO_Servant(String curl) throws CDBRecordDoesNotExistEx, CDBXMLErrorEx {

		// make sure CURL->DAO mapping is surjective function, remove leading slash
		if (curl.length() > 1 && curl.charAt(0) == '/')
			curl = curl.substring(1);

		// make sure there are no identical DAOs created
		synchronized (daoMap) {
			if (daoMap.containsKey(curl))
				return (DAO) daoMap.get(curl);
		}

		// do the hardwork here
		XMLHandler xmlSolver;

		try{
			xmlSolver  = loadRecords(curl, false);
		}catch(AcsJCDBXMLErrorEx e){
			String smsg = "XML Error \tCURL='" + e.getCurl()+"'\n\t\tFilename='"+e.getFilename()+"'\n\t\tNodename='"+e.getNodename()+"'\n\t\tMSG='"+e.getErrorString()+"'";
			m_logger.log(AcsLogLevel.NOTICE, smsg, e);	
			throw e.toCDBXMLErrorEx();
		}catch(AcsJCDBRecordDoesNotExistEx e){
			throw e.toCDBRecordDoesNotExistEx();
		}
		if (xmlSolver == null)
			return null;

		try {
			DAO href = null;

			// bind to map
			synchronized (daoMap) {

				// double-check sync. pattern

				// there is a possibility that loadRecord (look above) is called
				// twice, but this drawback happens rarely and it pays off parallel
				// execution when different DAO are created
				if (daoMap.containsKey(curl))
					return (DAO) daoMap.get(curl);

				DAOImpl daoImp = new DAOImpl(curl, xmlSolver.m_rootNode, poa);

				// create object id
				byte[] id = curl.getBytes();

				// activate object
				poa.activate_object_with_id(id, daoImp);
				href = DAOHelper.narrow(poa.servant_to_reference(daoImp));

				// map DAO reference
				daoMap.put(curl, href);
			}

			m_logger.log(AcsLogLevel.INFO,"Returning DAO servant for: " + curl);
			return href;

		} catch (Throwable t) {
			String info = "DAO::get_DAO_Servant " + t;
			AcsJCDBXMLErrorEx xmlErr = new AcsJCDBXMLErrorEx(t);
			xmlErr.setErrorString(info);
			m_logger.log(AcsLogLevel.NOTICE, info);
			throw xmlErr.toCDBXMLErrorEx();
		}
	}

	public void shutdown() {
		orb.shutdown(false);
	}

	/**
	 * @param curl
	 */
	protected void object_changed(String curl) {
		synchronized (daoMap) {
			if (daoMap.containsKey(curl)) {
				DAO dao = (DAO) daoMap.get(curl);
				dao.destroy();
				daoMap.remove(curl);
			}
		}
	}

	/**
	 * @return File
	 */
	protected File getStorageFile() {
		if (listenersStorageFile != null)
			return listenersStorageFile;
		String filePath = FileHelper.getTempFileName("ACS_RECOVERY_FILE", "CDB_Recovery.txt");
		m_logger.log(AcsLogLevel.INFO,  "Recovery file: " + filePath);
		listenersStorageFile = new File(filePath);
		// if file do not exists create a new one so we can set permission on it
		if( !listenersStorageFile.exists() ) {
			try {
				listenersStorageFile.createNewFile();
			}
			catch (Exception e) {
				// nop
			}
		}
		FileHelper.setFileAttributes("g+w", filePath);
		
		return listenersStorageFile;
	}

	/**
	 * 
	 */
	public void loadListeners() {
		File storageFile = getStorageFile();
		if (storageFile == null || !storageFile.exists())
			return;
		try {
			InputStream in = new FileInputStream(storageFile);
			BufferedReader reader = new BufferedReader(new InputStreamReader(in));

			// load listeners
			String line;
			while (true) {
				line = reader.readLine();
				if (line == null || line.length() == 0)
					break;
				Integer id = new Integer(line);
				line = reader.readLine();
				if (line == null || line.length() == 0)
					break;
				// try to narrow it 
				DALChangeListener listener = DALChangeListenerHelper.narrow(orb.string_to_object(line));
				regListeners.put(id, listener);
			}

			// then listened curls
			String curl;
			while (true) {
				curl = reader.readLine();
				if (curl == null)
					break;
				ArrayList arr = new ArrayList();
				while (true) {
					line = reader.readLine();
					if (line == null || line.length() == 0)
						break;
					arr.add(new Integer(line));
				}
				listenedCurls.put(curl, arr);
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * @return boolean
	 */
	public boolean saveListeners() {
		String key, ior;
		File storageFile = getStorageFile();
		if (storageFile == null)
			return false;
		try {
			OutputStream out = new FileOutputStream(storageFile);
			//listenedCurls.store(new FileOutputStream(storageFile), "Listeners");
			BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(out));
			// write listeners
			Iterator reg = regListeners.keySet().iterator();
			while (reg.hasNext()) {
				Integer id = (Integer) reg.next();
				writer.write(id.toString());
				writer.newLine();
				ior = orb.object_to_string(((DALChangeListener) regListeners.get(id)));
				writer.write(ior);
				writer.newLine();
			}
			// write listened curls
			Iterator iter = listenedCurls.keySet().iterator();
			while (iter.hasNext()) {
				key = (String) iter.next();
				ArrayList arr = (ArrayList) listenedCurls.get(key);
				if(arr.size() == 0 )
					continue; // don't write curls without listeners
				// separator
				writer.newLine();
				writer.write(key);
				writer.newLine();
				for (int i = 0; i < arr.size(); i++) {
					writer.write(arr.get(i).toString());
					writer.newLine();
				}
			}
			writer.flush();
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}

	public int add_change_listener(DALChangeListener listener) {
		int id;
		while (true) {
			id = idPool.nextInt(Integer.MAX_VALUE);
			Integer key = new Integer(id);
			if (!regListeners.containsKey(key)) {
				regListeners.put(key, listener);
				break;
			}
		}
		return id;
	}

	public void listen_for_changes(String curl, int listenerID) {
		synchronized (listenedCurls) {
			ArrayList listeners = (ArrayList) listenedCurls.get(curl);
			if (listeners == null) {
				listeners = new ArrayList();
				listenedCurls.put(curl, listeners);
			}
			listeners.add(new Integer(listenerID));
			saveListeners();
		}
	}
	public void remove_change_listener(int listenerID) {
		synchronized (listenedCurls) {
			Iterator iter = listenedCurls.keySet().iterator();
			// deattach this listener from its curls
			while (iter.hasNext()) {
				String curl = (String) iter.next();
				ArrayList listeners = (ArrayList) listenedCurls.get(curl);
				if (listeners != null) {
					//listeners.remove(listener);
					for (int i = 0; i < listeners.size(); i++) {
						Integer id = (Integer) listeners.get(i);
						if (id.intValue() == listenerID) {
							listeners.remove(i);
							i--; // just to test shifted
						}
					}
				}
			}
			// and delete it from list
			regListeners.remove(new Integer(listenerID));
			saveListeners();
		}
	}
	/**
	 * Cleans listened curls from invalid listeners
	 * to avoid repeatedly calling invalid listeners
	 */
	protected void cleanListenedCurls() {
		Iterator iter = listenedCurls.keySet().iterator();
		while (iter.hasNext()) {
			String curl = (String) iter.next();
			ArrayList listeners = (ArrayList) listenedCurls.get(curl);
			if (listeners == null)
				continue;
			for (int i = 0; i < listeners.size(); i++) {
				DALChangeListener listener = (DALChangeListener) regListeners.get(listeners.get(i));
				if( listener == null ) {
					listeners.remove(i);
					i--;
				}
			}
		}
	}

	public void clear_cache(String curl) {
		// first take care of our own map
		object_changed(curl);

		// then all registered listeners
		synchronized (listenedCurls) {
			boolean needToSave = false;
			ArrayList listeners = (ArrayList) listenedCurls.get(curl);
			if (listeners == null)
				return;
			ArrayList invalidListeners = new ArrayList();
			for (int i = 0; i < listeners.size(); i++) {
				DALChangeListener listener = (DALChangeListener) regListeners.get(listeners.get(i));
				try {
					//System.out.println("Calling " + listener + " ...");
					listener.object_changed(curl);
					//System.out.println("Done " + listener);
				} catch (RuntimeException e) {
					// silent here because who knows what happend with clients
					invalidListeners.add(listeners.get(i));
				}
			}
			// now remove invalid listeners if any
			for (int i = 0; i < invalidListeners.size(); i++) {
				listeners.remove(invalidListeners.get(i));
				regListeners.remove(invalidListeners.get(i));
				needToSave = true;
			}
			if (needToSave) {
				cleanListenedCurls();
				saveListeners();
			}
		}
	}
	
        public void clear_cache_all() {
		loadFactory();
		rootNode = DALNode.getRoot(m_root);
		synchronized (listenedCurls) {
			Iterator iter = listenedCurls.keySet().iterator();
			while (iter.hasNext()) {
				String curl = (String) iter.next();
				clear_cache(curl);
			}
		}
	}
	// listing
	public String list_nodes(String name) {
		// if we didn't create a root node yet or we are listing
		// the root content then recreate the tree data
		if( rootNode == null || name.length() == 0 ) {
			rootNode = DALNode.getRoot(m_root);
		}
		//System.out.println( "Listing " + name );
		String list = rootNode.list(name);
		
		return list;
		
		/*
		TODO !!! down code removes .xml entry and adds XML nested hier. nodes
		
		
		
		// TODO caching impl. would be possible
		// e.g. check if DAO is available and take rootNode from it...
		
	    // remove .xml file
	    final String XML_ENDING = ".xml ";
	    int pos = list.indexOf(XML_ENDING);
	    if (pos > 0)
	    {
	        list = list.substring(pos+XML_ENDING.length());

		    String curl = name;
			// make sure CURL->DAO mapping is surjective function, remove leading slash
			if (curl.length() > 1 && curl.charAt(0) == '/')
				curl = curl.substring(1);

			XMLHandler xmlSolver = null;
	        try {
	            xmlSolver = loadRecord(curl, false, true);
	        } catch (Throwable th) {
	            // noop
	        }
	        StringBuffer internalNodes = null;
	        if (xmlSolver != null)
	        {
	            internalNodes = new StringBuffer();
	            Iterator iter = xmlSolver.m_rootNode.getNodesMap().keySet().iterator();
	            while (iter.hasNext()) {
	                internalNodes.append(iter.next().toString());
	                internalNodes.append(' ');
	            }
	        }

	        if (internalNodes != null && internalNodes.length() > 0)
			    list += internalNodes.toString();

	    }

		return list;
		 */

	}
	
	/**
	 * @return
	 */
	public SAXParser getSaxParser() {
		return saxParser;
	}

}
