/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
 *
 *    Created on Apr 3, 2007
 *
 */

package com.cosylab.cdb.jdal;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.persistence.EnumType;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.exolab.castor.net.URILocation;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Restrictions;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.CORBA.ORB;
import org.omg.PortableServer.IdAssignmentPolicyValue;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;
import org.w3c.dom.DOMError;
import org.w3c.dom.DOMErrorHandler;
import org.w3c.dom.DOMException;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSException;
import org.w3c.dom.ls.LSInput;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSParser;
import org.w3c.dom.ls.LSResourceResolver;
import org.xml.sax.InputSource;

import alma.TMCDB.baci.BACIPropertyType;
import alma.TMCDB.baci.ComponentData;
import alma.TMCDB.baci.EmptyStringHandlerBACIPropertyType;
import alma.TMCDB.maci.Channels;
import alma.TMCDB.maci.EventChannelNode;
import alma.acs.logging.AcsLogLevel;
import alma.acs.tmcdb.AcsService;
import alma.acs.tmcdb.AcsServiceServiceType;
import alma.acs.tmcdb.BACIPropArchMech;
import alma.acs.tmcdb.BACIProperty;
import alma.acs.tmcdb.ChannelMapping;
import alma.acs.tmcdb.Component;
import alma.acs.tmcdb.ComponentImplLang;
import alma.acs.tmcdb.ComponentType;
import alma.acs.tmcdb.Computer;
import alma.acs.tmcdb.ComputerProcessorType;
import alma.acs.tmcdb.Configuration;
import alma.acs.tmcdb.Container;
import alma.acs.tmcdb.ContainerImplLang;
import alma.acs.tmcdb.ContainerStartupOption;
import alma.acs.tmcdb.DomainsMapping;
import alma.acs.tmcdb.Event;
import alma.acs.tmcdb.EventChannel;
import alma.acs.tmcdb.EventChannelConReliability;
import alma.acs.tmcdb.EventChannelDiscardPolicy;
import alma.acs.tmcdb.EventChannelEventReliability;
import alma.acs.tmcdb.EventChannelOrderPolicy;
import alma.acs.tmcdb.LoggingConfig;
import alma.acs.tmcdb.Manager;
import alma.acs.tmcdb.NamedLoggerConfig;
import alma.acs.tmcdb.NotificationServiceMapping;
import alma.acs.tmcdb.Schemas;
import alma.acs.tmcdb.logic.ContainerStartupOptionHelper;
import alma.cdbErrType.CDBExceptionEx;
import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.CDBFieldIsReadOnlyEx;
import alma.cdbErrType.CDBRecordAlreadyExistsEx;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBRecordIsReadOnlyEx;
import alma.cdbErrType.CDBXMLErrorEx;
import alma.cdbErrType.WrongCDBDataTypeEx;
import alma.cdbErrType.wrappers.AcsJCDBExceptionEx;
import alma.cdbErrType.wrappers.AcsJCDBFieldDoesNotExistEx;
import alma.cdbErrType.wrappers.AcsJCDBRecordDoesNotExistEx;
import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;
import alma.cdbErrType.wrappers.AcsJWrongCDBDataTypeEx;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALChangeListener;
import com.cosylab.CDB.DALChangeListenerHelper;
import com.cosylab.CDB.DAO;
import com.cosylab.CDB.DAOHelper;
import com.cosylab.CDB.DAOOperations;
import com.cosylab.CDB.DAOPOA;
import com.cosylab.CDB.DAOPOATie;
import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;
import com.cosylab.CDB.WDAO;
import com.cosylab.CDB.WDAOHelper;
import com.cosylab.CDB.WDAOPOA;
import com.cosylab.CDB.WDAOPOATie;
import com.cosylab.CDB.WJDALPOA;
import com.cosylab.CDB.WJDALPOATie;
import com.cosylab.cdb.client.CDBAccess;
import com.cosylab.cdb.client.DAOProxy;
import com.cosylab.cdb.jdal.hibernate.DOMJavaClassIntrospector;
import com.cosylab.cdb.jdal.hibernate.ExtraDataFeatureUtil;
import com.cosylab.cdb.jdal.hibernate.HibernateDBUtil;
import com.cosylab.cdb.jdal.hibernate.HibernateUtil;
import com.cosylab.cdb.jdal.hibernate.RootMap;
import com.cosylab.cdb.jdal.hibernate.DOMJavaClassIntrospector.XMLSaver;
import com.cosylab.cdb.jdal.hibernate.plugin.HibernateWDALPlugin;
import com.cosylab.cdb.jdal.hibernate.plugin.PluginFactory;
import com.cosylab.util.FileHelper;

/**
 * WDAL implementation for hibernate w/ DOMJavaClassInspector.
 * @author msekoranja
 */
@SuppressWarnings("unchecked")
public class HibernateWDALImpl extends WJDALPOA implements Recoverer {

	static final String TMCDB_CONFIGURATION_NAME_KEY = "TMCDB_CONFIGURATION_NAME";

	static final String TMCDB_COMPONENT_TREE_NAME_KEY = "TMCDB_COMPONENT_TREE_NAME";
	static final String COMPONENT_TREE_NAME = System.getProperty(TMCDB_COMPONENT_TREE_NAME_KEY, "alma");
	
	static final boolean TMCDB_ACS_ONLY = Boolean.getBoolean("cdb_rdb.acsOnly");
	
	// a DAL is always valid for *one* configuration:
  	protected String configName = null;
	protected int configId = -1;
	protected Configuration config = null;
	protected final HibernateUtil hibernateUtil;
	protected final HibernateDBUtil hibernateDBUtil;
	
	protected volatile Session mainSession = null;
 	
	
	final static String DUMMY_CONTAINER_FLAG = "dummy";
	
	protected boolean loadXMLCDB = false;
	protected boolean createTables = false;
	protected boolean forceInMemory = false;
	protected String[] args;
	
	protected ORB orb;
	protected POA poa;

	private final HashMap<String, DAO> daoMap = new HashMap<String, DAO>();
	private final HashMap wdaoMap = new HashMap();

	private HashMap daoObjMap = new HashMap();
	private HashMap wdaoObjMap = new HashMap();

	// clean cache implementation
	private HashMap<String, ArrayList<Integer>> listenedCurls = new HashMap<String, ArrayList<Integer>>();
	private File listenersStorageFile = null;
	private Random idPool = new Random();
	private HashMap<Integer, DALChangeListener> regListeners = new HashMap<Integer, DALChangeListener>();
	private boolean recoveryRead = true;
	
	protected final Logger m_logger;

	protected volatile Object rootNode;
	
	protected SAXParser saxParser;	
	
	protected LSParser parser;	
	protected LSInput input;
	protected LSOutput output;
	protected DOMImplementationLS domImplementationLS;
	protected SchemaResourceResolverLoader schemaResourceResolverLoader;
	
	protected HibernateWDALPlugin plugin;
	
	private final AtomicBoolean loadInProgress = new AtomicBoolean(false);
	
	private String m_root = "CDB";
	
	private final Object xmlNodeMonitor = new Object();
	
	private AtomicBoolean shutdown = new AtomicBoolean(false);
	private AtomicLong lastSaveTime = new AtomicLong(0);
	private AtomicBoolean saveRequest = new AtomicBoolean(false);
	
	/**
	 * ctor that takes all command line args given to OracleServer
	 * @param args
	 * @param orb_val
	 * @param poa_val
	 */
	public HibernateWDALImpl(String args[], ORB orb, POA poa, Logger logger) {
		this.args = args;
		this.orb = orb;
		this.poa = poa;
		m_logger = logger;
		hibernateUtil = HibernateUtil.getInstance(logger);

		// read args
		for (int i = 0; i < args.length; i++)
			if (args[i].equals("-n")) {
				recoveryRead = false;
			} else if (args[i].equals("-configName")) {
				if (i < args.length - 1)
					configName = args[++i];
			}
			else if (args[i].equals("-root")) {
				if (i < args.length - 1) {
					m_root = args[++i] + "/CDB";
				}
			}
			else if (args[i].equals("-loadXMLCDB")) {
				loadXMLCDB = true;
			}
			else if (args[i].equals("-createTables")) {
				createTables = true;
			}
			else if (args[i].equals("-memory")) {
				forceInMemory = true;
				createTables = true;
				loadXMLCDB = true;
			}
			else if (args[i].equals("-jvmPropCfg")) {
				System.getProperties().remove(PluginFactory.CONFIGURATION_WDAL_PLUGIN_KEY);
			}
			else if (args[i].equals("-help") || args[i].equals("-h")) {
				System.out.println("usage: java " + this.getClass().getName() + " [options]");
				System.out.println("\t-root\t\tXML CDB root (used on XML import)");
				System.out.println("\t-configName\tconfiguration name to be used to insert/query");
				System.out.println("\t-loadXMLCDB\timport XML CDB into RDB");
				System.out.println("\t-createTables\tcreate tables into RDB");
				System.out.println("\t-memory\t\tforce usage of in-memory RDB (implies -loadXMLCDB and -createTables)");
				System.out.println("\t-jvmPropCfg\tforce default configuration plugin (JVM properties)");
				System.exit(1);
			}

		if (TMCDB_ACS_ONLY)
			m_logger.info("Started in ACS only mode.");
		
		File rootF = new File(m_root);
		m_root = rootF.getAbsolutePath() + '/';
		// TODO: Does the "root" make any sense if we do not import XML data?
		// Perhaps then we should not log this message, just for a dummy root "CDB"
		m_logger.log(AcsLogLevel.INFO, "HibernateDAL root is: " + m_root);
		
		if (configName == null)
		{
			m_logger.fine("No configuration name specified, trying '" + TMCDB_CONFIGURATION_NAME_KEY + "' environment variable.");
			configName = System.getProperty(TMCDB_CONFIGURATION_NAME_KEY, System.getenv(TMCDB_CONFIGURATION_NAME_KEY));
			//if (configName == null)
			//	throw new RuntimeException("Configuration name not specified. Must be set with command line arg -configName followed by a string or by environment varibale '" + TMCDB_CONFIGURATION_NAME_KEY + "'.");
			if (configName == null)
			{
				m_logger.log(AcsLogLevel.NOTICE, "ERROR: Configuration name not specified. Must be set with command line arg -configName followed by a string or by environment variable '" + TMCDB_CONFIGURATION_NAME_KEY + "'.");
				System.exit(1);
			}
			m_logger.config("Configuration name obtained from environment variable.");
		}
		m_logger.info("Using TMCDB Configuration '" + configName + "'.");

		initializeParser();
		
		plugin = PluginFactory.getPlugin(m_logger);
		hibernateDBUtil = new HibernateDBUtil(logger, plugin);

		// set-up connection
		// in-memory can be configured from outside, therefore "feedback" for forceInMemory
		forceInMemory = hibernateDBUtil.setUp(forceInMemory, createTables);

		if (loadXMLCDB) {
			if (!loadXMLCDB(args, orb, poa, configName))
				System.exit(1);
		}
		
		startRecoverySaver();
		load();
	}
	
	/**
	 * Specialization of resource handler that loads all needed resources (schemas) into RDB.
	 */
	class SchemaResourceResolverLoader implements LSResourceResolver {
		private DOMImplementationLS domImplementationLS;
		private XSDElementTypeResolver xsdElementTypeResolver;
		private Session session = null;
		
		public SchemaResourceResolverLoader(Session session, DOMImplementationLS domImplementationLS, Logger logger) {
			this.domImplementationLS = domImplementationLS;
			this.session = session;
			
			try {
				xsdElementTypeResolver = new XSDElementTypeResolver(m_root, logger);
			} catch (Throwable tx) {
				throw new RuntimeException("Failed to instantiate " + this.getClass().getName(), tx);
			}
		}
		
		/**
		 * @param session the session to set
		 */
		public void setSession(Session session) {
			this.session = session;
		}

		public LSInput resolveResource(String type, String namespaceURI,
				String publicId, String systemId, String baseURI) {
//System.err.println("==== Resolving '" + type + "' '" + namespaceURI + "' '" + publicId + "' '" + systemId + "' '" + baseURI + "'");

			if (namespaceURI == null) {
				System.err.println("SchemaResourceResolverLoader#resolveResource: namespaceURI=null found for baseURI=" + baseURI + ", type=" + type);
				return null;
			}
			
			try
			{
				String schema = null;

				Schemas schemaRecord = (Schemas)session.createCriteria(Schemas.class)
					.add(Restrictions.eq("URN", namespaceURI)).add(Restrictions.eq("configuration", config)).uniqueResult();
				if (schemaRecord != null)
					schema = schemaRecord.getSchema();

				// .. read store it into RDB, if not already there
				if (schema == null)
				{
					URILocation loc = xsdElementTypeResolver.getUriResolver().resolveURN(namespaceURI);
					if (loc != null)
					{
						final BufferedReader reader = new BufferedReader(loc.getReader());
						final StringBuffer file = new StringBuffer();
						String line;
						while ((line = reader.readLine()) != null)
							file.append(line).append('\n');
						schema = file.toString();
						
						schemaRecord = new Schemas();
						schemaRecord.setURN(namespaceURI);
						schemaRecord.setConfiguration(config);
						schemaRecord.setSchema(schema);
						session.persist(schemaRecord);
					}
					else
						return null;
				}

				// return schema
				LSInput ret = domImplementationLS.createLSInput();
				ret.setStringData(schema);
				return ret;
			}
			catch (Throwable th) { th.printStackTrace(); return null; }
		}
	}

	private void initializeParser() throws RuntimeException
	{
		try
		{
			System.setProperty(DOMImplementationRegistry.PROPERTY, "org.apache.xerces.dom.DOMImplementationSourceImpl");
			DOMImplementationRegistry registry = DOMImplementationRegistry.newInstance();
			domImplementationLS = (DOMImplementationLS) registry.getDOMImplementation("LS");
			parser = domImplementationLS.createLSParser(DOMImplementationLS.MODE_SYNCHRONOUS, "http://www.w3.org/2001/XMLSchema");
			//parser.getDomConfig().setParameter("http://apache.org/xml/features/xinclude", Boolean.TRUE);
			schemaResourceResolverLoader = new SchemaResourceResolverLoader(null, domImplementationLS, m_logger);
			parser.getDomConfig().setParameter("resource-resolver", schemaResourceResolverLoader);
			parser.getDomConfig().setParameter("http://apache.org/xml/features/validation/schema", Boolean.TRUE);
			parser.getDomConfig().setParameter("error-handler",
				new DOMErrorHandler() {
					public boolean handleError(DOMError error) {
						//((Exception) error.getRelatedException()).printStackTrace();
						throw new LSException(LSException.PARSE_ERR, error.getMessage());
					}
				});

			input = domImplementationLS.createLSInput();
			output = domImplementationLS.createLSOutput();

			// SAX parser (non-validating)
			SAXParserFactory factory = SAXParserFactory.newInstance();
			saxParser = factory.newSAXParser();
		}
		catch (Throwable th) {
			throw new RuntimeException("Failed to initialize parser.", th);
		}
	}

	private synchronized void reloadData()
	{
		// full XML reload works only with in-memory DB
		// for external DBs, existing data should have been removed from the DB...
		if (forceInMemory)
		{
			// in-memory HSQLDB only, shutdown DB
			m_logger.info("Will shutdown the in-memory HSQLDB...");
			try
			{
				try
				{
					hibernateUtil.beginTransaction();
					Session session = hibernateUtil.getSession(); 
					session.createSQLQuery("SHUTDOWN IMMEDIATELY").executeUpdate();
				}
				finally
				{
					hibernateUtil.closeSession();
				}
			}
			catch (Throwable th)
			{
				m_logger.log(Level.WARNING, "Failed to shutdown in-memory HSQLDB.", th);
			}
			
			if (hibernateUtil.getSessionFactory() != null)
				hibernateUtil.getSessionFactory().close();

			m_logger.info("Will re-create the in-memory HSQLDB...");
			hibernateDBUtil.setUp(forceInMemory, createTables);

			if (loadXMLCDB) {
				m_logger.info("Will reload XML CDB data...");
				loadXMLCDB(args, orb, poa, configName);
			}
		}
		else
		{
			try
			{
				// clear Hibernate session (cache)
				hibernateUtil.getSession().clear();
			} catch (Throwable th) {
				m_logger.log(Level.WARNING, "Failed to clear the hibernate session cache.", th);
			}
		}
		
		load();
		m_logger.info("Successfully reloaded all data.");
	}
	
	private void load()
	{
		// HSO TODO: If this is a re-don't, we need to wait until any other IDL methods have finished,
		// which entered because loadInProgress was false ?
		loadInProgress.set(true);
		
		try
		{
			mainSession = hibernateUtil.getSession(); 
			
			// reload
			config = resolveConfig(mainSession, configName);
			configId = config.getConfigurationId();

			initializeRootNode();
		
			// we do not close session (needed by WDAL)
		} catch (Throwable th) {
			m_logger.log(Level.SEVERE, "Failed to load TMCDB data from the relational database.", th);
			// @TODO use checked ex
			throw new RuntimeException("Failed to load data.", th);
		} finally {
			loadInProgress.set(false);
		}
	}

	private Configuration resolveConfig(Session session, String configName) 
	{
		try
		{
			Configuration config = (Configuration)session.createCriteria(Configuration.class).
										add(Restrictions.eq("configurationName", configName)).uniqueResult();
			if (config != null)
				return config;
			else
				throw new RuntimeException("Configuration with name '" + configName + "' does not exist.");
		} catch (Throwable th) {
			throw new RuntimeException("Failed to resolve configuration from name '" + configName + "'.", th);
		}
	}
	
	private static final String readString(DAOOperations dao, String name, String defaultValue)
	{
		try
		{
			return dao.get_string(name);
		}
		catch (Throwable th)
		{
			return defaultValue;
		}
	}
	
	private static final String[] readStringSeq(DAOOperations dao, String name, String[] defaultValue)
	{
		try
		{
			return dao.get_string_seq(name);
		}
		catch (Throwable th)
		{
			return defaultValue;
		}
	}

	private static final int readLong(DAOOperations dao, String name, int defaultValue)
	{
		try
		{
			return dao.get_long(name);
		}
		catch (Throwable th)
		{
			return defaultValue;
		}
	}

	private static final double readDouble(DAOOperations dao, String name, double defaultValue)
	{
		try
		{
			return dao.get_double(name);
		}
		catch (Throwable th)
		{
			return defaultValue;
		}
	}

	private static final Double readDouble(DAOOperations dao, String name, Double defaultValue)
	{
		try
		{
			return dao.get_double(name);
		}
		catch (Throwable th)
		{
			return defaultValue;
		}
	}

	private static final Float readFloat(DAOOperations dao, String name, Float defaultValue)
	{
		try
		{
			return new Float(dao.get_double(name));
		}
		catch (Throwable th)
		{
			return defaultValue;
		}
	}

	public String[] getSubNodes(DAL dal, String subnode) throws Throwable
	{
		ArrayList subnodes = new ArrayList();
	    
	    LinkedList stack = new LinkedList();
	    stack.addLast(subnode);
	    	
		while (!stack.isEmpty())
		{
		    String parentNode = stack.removeLast().toString();
		    
		    String nodes = dal.list_nodes(parentNode);
			if (nodes.length() > 0)
			{
			    StringTokenizer tokenizer = new StringTokenizer(nodes);
			    while (tokenizer.hasMoreTokens())
			    {
			        String nodeName = tokenizer.nextToken();
			        if (nodeName.endsWith(".xml"))
			            continue;
			        
			        String fullName = parentNode + "/" + nodeName;
			        stack.addLast(fullName);
			        // strip off relative path
			        subnodes.add(fullName.substring(subnode.length()+1));
			    }
			}
		}				
	    
		String[] retVal = new String[subnodes.size()];
		subnodes.toArray(retVal);
		return retVal;
	}
	
	// NOTE: throw NO_RESOURCES is thrown, since there is no common exception for all CORBA calls
	private final void checkAccess() throws NO_RESOURCES
	{
		if (loadInProgress.get()) {
			m_logger.warning("Incoming Corba call denied (NO_RESOURCES) because data is being loaded.");
			throw new NO_RESOURCES("load in progress");
		}
	}
	

	private void loadSchemas(Session session) throws Throwable {

		String schemas = DALImpl.getSchemas(m_root, m_logger);
		if (schemas == null)
			return;

		StringTokenizer tokenizer = new StringTokenizer(schemas);
		while (tokenizer.hasMoreTokens()) {
			String urn = tokenizer.nextToken();
			String fileName = tokenizer.nextToken();
			
			// ignore all DTDs and files ending with XMLSchema.xsd
			if (fileName.toLowerCase().endsWith(".dtd") || fileName.endsWith("XMLSchema.xsd"))
				continue;
			
			Schemas schemaRecord = null;
			try {
				schemaRecord = (Schemas) session.createCriteria(Schemas.class).
									add(Restrictions.eq("URN", urn)).
									add(Restrictions.eq("configuration", config)).
									uniqueResult();
			} 
			catch (HibernateException ex) {
				// adding this to better understand a java.sql.BatchUpdateException: ORA-31001: Invalid resource handle or path name "/XMLSchema.dtd"
				m_logger.log(Level.INFO, "Failed to select (check existence of) schema with URN='" + urn + "' in the database. The schema file to load would be " + fileName);
				throw ex;
			}
			if (schemaRecord == null) {
				final BufferedReader reader = new BufferedReader(new FileReader(fileName));
				final StringBuffer file = new StringBuffer();
				String line;
				while ((line = reader.readLine()) != null)
					file.append(line).append('\n');

				schemaRecord = new Schemas();
				schemaRecord.setURN(urn);
				schemaRecord.setConfiguration(config);
				schemaRecord.setSchema(file.toString());
				session.persist(schemaRecord);
			}
		}

	}
	
	final int NULL_ID = 0;
	//final Integer NULL_ID = null;
	
	protected boolean loadXMLCDB(String args[], ORB orb, POA poa, String configName)
	{
		m_logger.info("Reading configuration from XML CDB...");

		try
		{
			loadInProgress.set(true);
			
			/* create new poa for xmlCDB */
			org.omg.CORBA.Policy[] policies = new org.omg.CORBA.Policy[] {
				poa.create_id_assignment_policy(IdAssignmentPolicyValue.USER_ID)
			};
			
			POA xmlCDBPOA = poa.create_POA("xmlCDBPOA", poa.the_POAManager(), policies);
			
			for (int i = 0; i < policies.length; i++)
				policies[i].destroy();

			// disable cache for XML CDB DAL (important)
			String[] newArgs = new String[args.length+2];
			System.arraycopy(args, 0, newArgs, 0, args.length);
			newArgs[args.length] = "-disableRecoveryFile";
			newArgs[args.length+1] = "-disableCache";
			
			final WDALImpl servantDelegate = new WDALImpl(newArgs, orb, xmlCDBPOA, m_logger);
			final Servant wdalImplServant = new WJDALPOATie(servantDelegate);
//			WDALImpl wdalImplServant = new WDALImpl(args, orb, xmlCDBPOA, m_logger);
			xmlCDBPOA.activate_object_with_id(new byte[] { 'x', 'm', 'l', 'C', 'D', 'B' }, wdalImplServant);
			org.omg.CORBA.Object ref = xmlCDBPOA.servant_to_reference(wdalImplServant);
			final JDAL xmlCDB_ = JDALHelper.narrow(ref);
			
		    // -----
			
			// get set of BACI property attributes (non-extras)
			String[] baciPropertyAttributes = DOMJavaClassIntrospector.getAccessibleFields(new BACIPropertyType(), true);
			Set<String> baciPropertyAttributesSet = new HashSet<String>();
			for (String attribute : baciPropertyAttributes) {
				baciPropertyAttributesSet.add(attribute);
			}
			
			XSDElementTypeResolver xsdElementTypeResolver = new XSDElementTypeResolver(m_root, m_logger);
			Set<String> nonControlDeviceSet = new TreeSet<String>(); 
			Set<String> characteristicsDeviceSet = new TreeSet<String>(); 
			
			Set<String> processedComponentTypes = new HashSet<String>();
			// -----

			CDBAccess cdbAccess = new CDBAccess(orb, m_logger);
			cdbAccess.setDAL(xmlCDB_);
			
			try
			{
				hibernateUtil.beginTransaction();
				Session session = hibernateUtil.getSession(); 
				schemaResourceResolverLoader.setSession(session);

				// check if configuration already exists
				config = (Configuration)session.createCriteria(Configuration.class).
											add(Restrictions.eq("configurationName", configName)).uniqueResult();
				if (config != null)
				{
					m_logger.warning("Configuration with name '" + configName + "' already exists. Skipping loading XML stage.");
					return false;
				}
				
				// create configuration
		        config = new Configuration();
		        config.setConfigurationName(configName);
		        config.setFullName(configName);
		        config.setActive(true);
		        config.setCreationTime(Calendar.getInstance().getTime());
		        config.setDescription("Imported from CDB by HibernateWDAL");
		        session.persist(config);

				configId = config.getConfigurationId();

				// plugin importPrologue()
				if (plugin != null)
				{
					try
					{
						plugin.importPrologue(session, config, cdbAccess);
					}
					catch (Throwable th)
					{
						// @TODO decent exception log. Is OK to swallow th?
						th.printStackTrace();
					}
				}
				
				// load all schemas				
				loadSchemas(session);

				try
				{
					//DAO managerDAO = xmlCDB.get_DAO_Servant("MACI/Managers/Manager");
					DAOProxy managerDAO = cdbAccess.createDAO("MACI/Managers/Manager");
					
					LoggingConfig managerLoggingConfig =
						persistLoggingConfig(session, managerDAO, false);
	
					Manager manager = new Manager();
					manager.setConfiguration(config);
					manager.setLoggingConfig(managerLoggingConfig);
					manager.setStartup(managerDAO.get_string("Startup"));
					manager.setServiceComponents(managerDAO.get_string("ServiceComponents"));
					try {
						manager.setServiceDaemons(managerDAO.get_string("ServiceDaemons"));
					} catch (CDBFieldDoesNotExistEx e) {
						manager.setServiceDaemons(""); // Optional, but has no default!
					}
					manager.setTimeout((int)managerDAO.get_double("Timeout"));
					manager.setClientPingInterval((int)managerDAO.get_double("ClientPingInterval"));
					manager.setAdministratorPingInterval((int)managerDAO.get_double("AdministratorPingInterval"));
					manager.setContainerPingInterval((int)managerDAO.get_double("ContainerPingInterval"));
					manager.setServerThreads((byte)managerDAO.get_long("ServerThreads"));
					session.persist(manager);
					m_logger.info("Imported Manager from XML.");
				}
				catch (Throwable e)
				{
					m_logger.log(Level.WARNING,"MACI/Managers/Manager record does not exist or misconfigured, using defaults.",e);
				}

				/*
				String containers = xmlCDB.list_nodes("MACI/Containers");
				StringTokenizer tokenizer = new StringTokenizer(containers);
				while (tokenizer.hasMoreTokens())
				{
					String containerName = tokenizer.nextToken();
				*/
				StringTokenizer tokenizer;
				String[] containerSubnodes = getSubNodes(xmlCDB_, "MACI/Containers");
				for (String containerName : containerSubnodes)
				{
					//DAO containerDAO = xmlCDB.get_DAO_Servant("MACI/Containers/"+containerName);
					DAOProxy containerDAO = cdbAccess.createDAO("MACI/Containers/"+containerName);

					// check if real config, otherwice skip
					if (readLong(containerDAO, "LoggingConfig/minLogLevel", -1) < 0)
						continue;
					
					LoggingConfig loggingConfig =
						persistLoggingConfig(session, containerDAO, true);
					
					Computer hostComputer = null;
					String computerHostName = readString(containerDAO, "DeployInfo/Host", null);
					if (computerHostName != null)
					{
						hostComputer = (Computer)session.createCriteria(Computer.class).
													add(Restrictions.eq("networkName", computerHostName)).uniqueResult();
						if (hostComputer == null) {
							// NOTE: we add some dummy data as computer name, realtime flag, CPU type here
							String computerName = computerHostName;
							int dotPos = computerName.indexOf('.'); 
							if (dotPos > 0)
								computerName = computerName.substring(0, dotPos);
							hostComputer = new Computer();
							hostComputer.setName(computerName);
							hostComputer.setConfiguration(config);
							hostComputer.setNetworkName(computerHostName);
							hostComputer.setRealTime(false);
							hostComputer.setDiskless(false);
							hostComputer.setProcessorType(ComputerProcessorType.UNI);
							hostComputer.setPhysicalLocation(null);
							session.persist(hostComputer);
						}
					}
					
					final String containerPath;
					int hierarchySeparatorPos = containerName.lastIndexOf('/');
					if (hierarchySeparatorPos != -1)
					{
						containerPath = containerName.substring(0, hierarchySeparatorPos);
						containerName = containerName.substring(hierarchySeparatorPos + 1);
					}
					else
						containerPath = "/"; // for Oracle
					
					Container container = new Container();
					container.setContainerName(containerName);
					container.setPath(containerPath);
					container.setConfiguration(config);
					container.setLoggingConfig(loggingConfig);
					container.setComputer(hostComputer);
					
					container.setImplLang(ContainerImplLang.valueOfForEnum(readString(containerDAO, "ImplLang", "cpp"))); // cpp is default, since field is required
					container.setTypeModifiers(readString(containerDAO, "DeployInfo/TypeModifiers", null));
					container.setStartOnDemand(Boolean.valueOf(readString(containerDAO, "DeployInfo/StartOnDemand", "false")));
					
					container.setRealTime(false);
					container.setRealTimeType(null);
					container.setKernelModuleLocation(null);
					container.setKernelModule(null);
					
					container.setKeepAliveTime(readLong(containerDAO, "DeployInfo/KeepAliveTime", -1));
					container.setServerThreads(containerDAO.get_long("ServerThreads"));
					container.setManagerRetry(containerDAO.get_long("ManagerRetry"));
					container.setCallTimeout((int)containerDAO.get_double("Timeout"));
					container.setRecovery(Boolean.valueOf(containerDAO.get_string("Recovery")));
					
					int pingInterval = readLong(containerDAO, "PingInterval", Integer.MIN_VALUE);
					if (pingInterval != Integer.MIN_VALUE)
						container.setPingInterval(pingInterval);
					container.setAutoloadSharedLibs(containerDAO.get_string("Autoload"));
					session.persist(container);
					// convert the "Flags" string of concatenated options to ContainerStartupOption 
					String containerStartFlags = readString(containerDAO, "DeployInfo/Flags", null);
					ContainerStartupOptionHelper containerStartupOptionHelper = new ContainerStartupOptionHelper(m_logger);
					Collection<ContainerStartupOption> contOptions = containerStartupOptionHelper.convertFlagsString(container, containerStartFlags);
					for (ContainerStartupOption containerStartupOption : contOptions) {
						session.persist(containerStartupOption);
					}
				}
				if (containerSubnodes.length > 0) {
					m_logger.info("Imported Containers from XML.");
				}
				else {
					m_logger.info("No XML container data found.");
				}


				// set of all existing component names
				// used to generate *, *1, *2 names, as CDB does
				// NOTE: initial set is not filled with component names that are already in the DB
				Set<String> existingComponentNames = new HashSet<String>();
				
	            LinkedHashSet nodes = new LinkedHashSet();
	            
				//DAO componentDAO = xmlCDB.get_DAO_Servant("MACI/Components");
				DAOProxy componentDAO = null;
				
				try
				{
					componentDAO = cdbAccess.createDAO("MACI/Components");

					// current
		            nodes.add("/");
		            String[] subnodes = getSubnodes(xmlCDB_, "MACI/Components");
		            if (subnodes != null)
		                for (int i = 0; i < subnodes.length; i++)
		                    nodes.add(subnodes[i]);
				}
				catch (RuntimeException rte)
				{
					m_logger.warning("Failed to read MACI/Components DAO, skipping...");
				}
				
				Iterator iter = nodes.iterator();
	            while (iter.hasNext())
	            {
	                String path = iter.next().toString();
	                String prefix;
	                if (path.length() == 0 || path.equals("/"))
	                	prefix = "";
	                else
	                	prefix = path + "/";

	                String components = (String) componentDAO.get_field_data(prefix + "_elements");

	                // store "original" path/prefix (it can be changed)
	                final String originalPath = path;
	                final String originalPrefix = prefix;
	                
					tokenizer = new StringTokenizer(components, ",");
					while (tokenizer.hasMoreTokens())
					{
						// set original values
						path = originalPath;
						prefix = originalPrefix;
						
						String componentName = prefix + tokenizer.nextToken();
						
						String realComponentName = readString(componentDAO, componentName+"/Name", null);
						if (realComponentName == null)
							continue;
				
						// hierarchical name fix
						int hierarchySeparatorPos = realComponentName.lastIndexOf('/');
						if (hierarchySeparatorPos != -1)
							realComponentName = realComponentName.substring(hierarchySeparatorPos + 1);
				
						// handle duplicate names, i.e. "*"
						// a "trick" of adding "/" to path is being used to achieve uniques
						// "/////" is the same as "/" for TMCDB, but not for DB
						while (existingComponentNames.contains(prefix + realComponentName))
						{
							path = path + "/";
							prefix = prefix + "/";
						}
						existingComponentNames.add(prefix + realComponentName);
						
						int componentContainerId = -1;
						Container tmpComponentContainer = null;
						String containerName = readString(componentDAO, componentName+"/Container", null);
						if (containerName != null && !containerName.equals("*"))
						{

							String containerPath;
							hierarchySeparatorPos = containerName.lastIndexOf('/');
							if (hierarchySeparatorPos != -1)
							{
								containerPath = containerName.substring(0, hierarchySeparatorPos);
								containerName = containerName.substring(hierarchySeparatorPos + 1);
							}
							else {
								containerPath = "/"; // for Oracle
							}

							Container container = (Container)session.createCriteria(Container.class)
								.add(Restrictions.eq("configuration", config))
								.add(Restrictions.eq("containerName", containerName))
								.add(Restrictions.eq("path", containerPath)).uniqueResult();
							if (container != null) {
								componentContainerId = container.getContainerId();
								tmpComponentContainer = container;
							}
							else
							{
								LoggingConfig loggingConfig = new LoggingConfig();
								loggingConfig.setMinLogLevelDefault((byte)2);
								loggingConfig.setMinLogLevelLocalDefault((byte)2);
								loggingConfig.setCentralizedLogger("Log");
								loggingConfig.setDispatchPacketSize((byte)10);
								loggingConfig.setImmediateDispatchLevel((byte)10);
								loggingConfig.setFlushPeriodSeconds((byte)10);
								loggingConfig.setMaxLogQueueSize(1000);
								loggingConfig.setMaxLogsPerSecond(-1);
								session.persist(loggingConfig);

								container = new Container();
								container.setContainerName(containerName);
								container.setPath(containerPath);
								container.setConfiguration(config);
								container.setLoggingConfig(loggingConfig);
								container.setComputer(null);
								container.setImplLang(ContainerImplLang.valueOfForEnum(readString(componentDAO, "ImplLang", "cpp"))); // cpp is default, since field is required
								container.setTypeModifiers(DUMMY_CONTAINER_FLAG);
								container.setRealTime(false);
								container.setRealTimeType(null);
								container.setKernelModuleLocation(null);
								container.setKernelModule(null);
								container.setKeepAliveTime(-1);
								container.setServerThreads(5);
								container.setManagerRetry(10);
								container.setCallTimeout(2);
								container.setRecovery(false);
								container.setAutoloadSharedLibs(null);
								session.persist(container);
								componentContainerId = container.getContainerId();
								tmpComponentContainer = container;
							}
						}
						
						String xml = null;
						boolean almaBranchDoesNotExist = componentName.startsWith("*");
						boolean forceSubcomponentCheck = false;

						int typeId;
						String type = componentDAO.get_string(componentName+"/Type");
						DAOProxy componentConfigurationDAO = null; // pulled out for performance optimization, to avoid reading it twice in many cases
						Schemas schema = null;
						
						{
							String schemaURN = null;
							
							// check if it is a non-control device, simply check for existence of "ControlDevice" element
							if (!almaBranchDoesNotExist)
							{
								try
								{
										// @TODO: Suppress the NOTICE log (or lower its level) which we get from the CDB code if there is no component configuration under the CDB/alma/ branch.
										//        NOTICE [CDB-RDB] Curl 'alma/SCHEDULING_MASTERSCHEDULER' does not exist.
										componentConfigurationDAO = cdbAccess.createDAO(COMPONENT_TREE_NAME + "/" + componentName);
										
										schemaURN = componentConfigurationDAO.get_string("xmlns");

										if (!processedComponentTypes.contains(type))
										{
											boolean isControlDevice = !TMCDB_ACS_ONLY && xsdElementTypeResolver.doesExtend(schemaURN, "ControlDevice");
											m_logger.fine(schemaURN + " does extend Control? " + isControlDevice);
											if (!isControlDevice)
												nonControlDeviceSet.add(type);
											
											
											boolean isCharateristicsComponent = xsdElementTypeResolver.doesExtend(schemaURN, "CharacteristicComponent") ||
												(TMCDB_ACS_ONLY && xsdElementTypeResolver.doesExtend(schemaURN, "ControlDevice"));		// sadly ControlDevice does not extend CharacteristicComponent XSD
											m_logger.fine(schemaURN + " does extend CharacteristicsComponent? " + isCharateristicsComponent);
											if (isCharateristicsComponent)
												characteristicsDeviceSet.add(type);
											
											processedComponentTypes.add(type);
										}
								}
								catch (Throwable th)
								{
									almaBranchDoesNotExist = true;
									
									// obviously not a control device (no component, i.e. alma, branch)
									// NOTE: if this device is missing alma branch data (and as control device it should have it), 
									// this type will not be identified as control device at all, also for all devices of this type
									// (ComponentType is filled at first occurrence of the type) 
									if (th.getCause() instanceof CDBRecordDoesNotExistEx)
									{
										// does not exists, this is OK... do not complain
									}
									else if (th instanceof CDBFieldDoesNotExistEx)
									{
										// field does not exist, but it might have sub-components
										forceSubcomponentCheck = true;
									}
									else
									{
										m_logger.log(AcsLogLevel.WARNING, "Failed to read component configuration: " + COMPONENT_TREE_NAME + "/" + componentName, th);
									}
								}
							}
							
                            // get the Schema identifier for the schemaURN
                            schema = (Schemas)session.createCriteria(Schemas.class)
                            	.add(Restrictions.eq("URN", schemaURN))
                            	.add(Restrictions.eq("configuration", config)).uniqueResult();
                            if (schema == null && !almaBranchDoesNotExist)
                            	m_logger.severe("Component " + componentName + " of XSD type " + schemaURN + " has no XSD file.");
                            
                            ComponentType componentType = (ComponentType)session.createCriteria(ComponentType.class)
                            	.add(Restrictions.eq("IDL", type)).uniqueResult();
                            if (componentType == null)
                            {
                            	componentType = new ComponentType();
                            	componentType.setIDL(type);
                                session.saveOrUpdate(componentType);
                            }

							typeId = componentType.getComponentTypeId();
						}

						boolean isControlDevice = !nonControlDeviceSet.contains(type) && !almaBranchDoesNotExist;
						boolean isCharateristicsDevice = characteristicsDeviceSet.contains(type);
						if (!isControlDevice && !isCharateristicsDevice && xml == null && !almaBranchDoesNotExist)
						{
							xml = getComponentXML(xmlCDB_, componentName, xml);
						}
						
					    Component component = new Component();
					    // TODO this can be optimized!!!
					    component.setComponentType((ComponentType)session.get(ComponentType.class, typeId));
					    component.setComponentName(realComponentName);
					    component.setConfiguration(config);
//					    component.setContainerId(componentContainerId);
					    component.setContainer(tmpComponentContainer); // TODO verify this and clean up
						component.setImplLang(ComponentImplLang.valueOfForEnum(readString(componentDAO, componentName+"/ImplLang", "cpp")));	// cpp is default, since field is required
						component.setRealTime(false);
						component.setCode(componentDAO.get_string(componentName+"/Code"));
						component.setPath(path);
						component.setIsAutostart(Boolean.parseBoolean(componentDAO.get_string(componentName+"/Autostart")));
						component.setIsDefault(Boolean.parseBoolean(componentDAO.get_string(componentName+"/Default")));
						component.setIsStandaloneDefined(true);
						component.setIsControl(isControlDevice);
						component.setKeepAliveTime(componentDAO.get_long(componentName+"/KeepAliveTime"));
						component.setMinLogLevel((byte)readLong(componentDAO, componentName+"/ComponentLogger/minLogLevel", -1));
						component.setMinLogLevelLocal((byte)readLong(componentDAO, componentName+"/ComponentLogger/minLogLevelLocal", -1));
						component.setXMLDoc(xml);
                    	component.setURN(schema == null ? null : schema.getURN());
						session.persist(component);
						session.flush();
					
						// try to create alma branch (if available)
						if ((isControlDevice || isCharateristicsDevice) && !almaBranchDoesNotExist) {
							try
							{
								if (componentConfigurationDAO == null) { 
									componentConfigurationDAO = cdbAccess.createDAO(COMPONENT_TREE_NAME + "/" + componentName);
								}
								if (plugin != null && isControlDevice)
								{
									plugin.controlDeviceImportEpilogue(session, config, cdbAccess, componentName, component);
								}
								
								Set<String> propertySet = new TreeSet<String>();
								String[] propertyCandidates = componentConfigurationDAO.get_string_seq("_elements");
								for (String propertyName : propertyCandidates)
								{
									// check if really property
									if (readString(componentConfigurationDAO, propertyName + "/format", null) != null) {
										m_logger.finer("Adding property " + propertyName);
										propertySet.add(propertyName);
									}
								}
	
								if (propertySet.size() > 0)
								{
									String[] properties = propertySet.toArray(new String[propertySet.size()]);
									
									String defaultPropertyNs = componentConfigurationDAO.get_string("xmlns");
									String[] propertyNs = new String[properties.length];
									for (int i = 0; i < properties.length; i++)
										propertyNs[i] = readString(componentConfigurationDAO, properties[i] + "/xmlns", defaultPropertyNs);
									
									ExtraDataFeatureUtil extraDataFeatureUtil = new ExtraDataFeatureUtil(m_logger);
									
									String[] propertyTypes = xsdElementTypeResolver.getElementTypes(componentConfigurationDAO.getElementName(), propertyNs, properties);
									for (int i = 0; i < properties.length; i++)
									{
										String propertyName = properties[i];
										if (propertyTypes[i] != null && propertyTypes[i].endsWith("Seq")) {
											propertyTypes[i] = propertyTypes[i].substring(0, propertyTypes[i].length() - 3);
										}
		
										BACIProperty baciPropertyType = new BACIProperty();
										baciPropertyType.setComponent(component);
										baciPropertyType.setPropertyName(propertyName);
										baciPropertyType.setDescription(nonEmptyString(componentConfigurationDAO.get_string(propertyName + "/description"), "-"));
										baciPropertyType.setFormat(nonEmptyString(componentConfigurationDAO.get_string(propertyName + "/format"), "%s"));
										baciPropertyType.setUnits(nonEmptyString(componentConfigurationDAO.get_string(propertyName + "/units"), "-"));
										baciPropertyType.setResolution(nonEmptyString(componentConfigurationDAO.get_string(propertyName + "/resolution"), EmptyStringHandlerBACIPropertyType.EMPTY_STRING_SUBSTITUTE));
		
										baciPropertyType.setArchive_priority(componentConfigurationDAO.get_long(propertyName + "/archive_priority"));
										baciPropertyType.setArchive_min_int(componentConfigurationDAO.get_double(propertyName + "/archive_min_int"));
										baciPropertyType.setArchive_max_int(componentConfigurationDAO.get_double(propertyName + "/archive_max_int"));
										baciPropertyType.setArchive_suppress(Boolean.parseBoolean(componentConfigurationDAO.get_string(propertyName + "/archive_suppress")));
										baciPropertyType.setArchive_mechanism(BACIPropArchMech.valueOfForEnum(nonEmptyString(componentConfigurationDAO.get_string(propertyName + "/archive_mechanism"), "monitor_collector")));
										baciPropertyType.setDefault_timer_trig(componentConfigurationDAO.get_double(propertyName + "/default_timer_trig"));
										baciPropertyType.setMin_timer_trig(componentConfigurationDAO.get_double(propertyName + "/min_timer_trig"));
		
										baciPropertyType.setInitialize_devio(Boolean.parseBoolean(componentConfigurationDAO.get_string(propertyName + "/initialize_devio")));
		
										/* P<type> */
										baciPropertyType.setMin_delta_trig(readDouble(componentConfigurationDAO, propertyName + "/min_delta_trig", 0.0));
										baciPropertyType.setDefault_value(nonEmptyString(componentConfigurationDAO.get_string(propertyName + "/default_value"), "-"));
												
										baciPropertyType.setGraph_min(limitDouble(readDouble(componentConfigurationDAO, propertyName + "/graph_min", null)));
										baciPropertyType.setGraph_max(limitDouble(readDouble(componentConfigurationDAO, propertyName + "/graph_max", null)));
										baciPropertyType.setMin_step(readDouble(componentConfigurationDAO, propertyName + "/min_step", null));
										baciPropertyType.setArchive_delta(readDouble(componentConfigurationDAO, propertyName + "/archive_delta", 0.0));
										baciPropertyType.setArchive_delta_percent(readDouble(componentConfigurationDAO, propertyName + "/archive_delta_percent", null));
		
										/* RO<type> */
										baciPropertyType.setAlarm_high_on(readDouble(componentConfigurationDAO, propertyName + "/alarm_high_on", null));
										baciPropertyType.setAlarm_low_on(readDouble(componentConfigurationDAO, propertyName + "/alarm_low_on", null));
										baciPropertyType.setAlarm_high_off(readDouble(componentConfigurationDAO, propertyName + "/alarm_high_off", null));
										baciPropertyType.setAlarm_low_off(readDouble(componentConfigurationDAO, propertyName + "/alarm_low_off", null));
										baciPropertyType.setAlarm_timer_trig(readDouble(componentConfigurationDAO, propertyName + "/alarm_timer_trig", null));
											    
									    /* RW<type> */
									    baciPropertyType.setMin_value(limitDouble(readDouble(componentConfigurationDAO, propertyName + "/min_value", null)));
									    baciPropertyType.setMax_value(limitDouble(readDouble(componentConfigurationDAO, propertyName + "/max_value", null)));
		
										/* ROpattern */
									    baciPropertyType.setBitDescription(nonEmptyString(readString(componentConfigurationDAO, propertyName + "/bitDescription", null), EmptyStringHandlerBACIPropertyType.EMPTY_STRING_SUBSTITUTE));
									    baciPropertyType.setWhenSet(nonEmptyString(readString(componentConfigurationDAO, propertyName + "/whenSet", null), EmptyStringHandlerBACIPropertyType.EMPTY_STRING_SUBSTITUTE));
									    baciPropertyType.setWhenCleared(nonEmptyString(readString(componentConfigurationDAO, propertyName + "/whenCleared", null), EmptyStringHandlerBACIPropertyType.EMPTY_STRING_SUBSTITUTE));
	
										/* PEnum */
									    baciPropertyType.setStatesDescription(nonEmptyString(readString(componentConfigurationDAO, propertyName + "/statesDescription", null), EmptyStringHandlerBACIPropertyType.EMPTY_STRING_SUBSTITUTE));
									    baciPropertyType.setCondition(nonEmptyString(readString(componentConfigurationDAO, propertyName + "/condition", null), EmptyStringHandlerBACIPropertyType.EMPTY_STRING_SUBSTITUTE));
									    baciPropertyType.setAlarm_on(nonEmptyString(readString(componentConfigurationDAO, propertyName + "/alarm_on", null), EmptyStringHandlerBACIPropertyType.EMPTY_STRING_SUBSTITUTE));
									    baciPropertyType.setAlarm_off(nonEmptyString(readString(componentConfigurationDAO, propertyName + "/alarm_off", null), EmptyStringHandlerBACIPropertyType.EMPTY_STRING_SUBSTITUTE));
									    
									    /* alarms */
									    baciPropertyType.setAlarm_fault_family(nonEmptyString(readString(componentConfigurationDAO, propertyName + "/alarm_fault_family", null), EmptyStringHandlerBACIPropertyType.EMPTY_STRING_SUBSTITUTE));
									    baciPropertyType.setAlarm_fault_member(nonEmptyString(readString(componentConfigurationDAO, propertyName + "/alarm_fault_member", null), EmptyStringHandlerBACIPropertyType.EMPTY_STRING_SUBSTITUTE));
									    int level = readLong(componentConfigurationDAO, propertyName + "/alarm_level", Integer.MIN_VALUE);
									    if (level != Integer.MIN_VALUE)
									    	baciPropertyType.setAlarm_level(level);
									    
									    baciPropertyType.setData(extraDataFeatureUtil.getExtraDataMap(componentConfigurationDAO, propertyName, baciPropertyAttributesSet, ExtraDataFeatureUtil.EMPTY_SET));
										
										session.persist(baciPropertyType);
									}

									// add non-property elements
									propertySet.add("Address");	// this is handled by HW plugin
								    component.setXMLDoc(extraDataFeatureUtil.getExtraDataMap(componentConfigurationDAO, null, ExtraDataFeatureUtil.EMPTY_SET, propertySet));
									session.update(component);
								}
								else
								{
									// no properties, add entire XML although it is a control device
								    component.setXMLDoc(xml == null ? getComponentXML(xmlCDB_, componentName, xml) : xml);
									session.update(component);
								}

								// from remote, since DAO will compact it
								if (!almaBranchDoesNotExist || forceSubcomponentCheck)
								{
									String componentNodes = xmlCDB_.list_nodes(COMPONENT_TREE_NAME + "/" + componentName);
									if (componentNodes != null)
									{
										StringTokenizer tokenizer2 = new StringTokenizer(componentNodes);
										while (tokenizer2.hasMoreTokens())
											propertySet.add(tokenizer2.nextToken());
									}
								}
							}
							catch (RuntimeException rte)
							{
								// ignore components with no configuration; this is very ugly wat of doing it...
								if (rte.getMessage() != null && rte.getMessage().startsWith("Failed to obtain"))
								{
									// noop (there is no configuration for component)
								}
								else
									rte.printStackTrace(); 
							}
						}
					}
	            }
	            if (nodes.size() > 0) {
	            	// if the preceding while loop actually did something...
	            	m_logger.info("Imported Components from XML.");
	            }
	            
	            
	            //
	            // Channels configurations
	            //
            	String[] channelSubnodes = getSubNodes(xmlCDB_, "MACI/Channels");
				for (String channelName : channelSubnodes)
				{
					//DAO channelDAO = xmlCDB.get_DAO_Servant("MACI/Channels/"+channelName);
					DAOProxy channelDAO = cdbAccess.createDAO("MACI/Channels/"+channelName);
					
					final String channelPath;
					final String channelShortName;
					int hierarchySeparatorPos = channelName.lastIndexOf('/');
					if (hierarchySeparatorPos != -1)
					{
						channelPath = channelName.substring(0, hierarchySeparatorPos);
						channelShortName = channelName.substring(hierarchySeparatorPos + 1);
					}
					else
					{
						channelPath = "/"; // for Oracle
						channelShortName = channelName;
					}
					
					EventChannel eventChannel = new EventChannel();
					eventChannel.setConfiguration(config);
					eventChannel.setName(channelShortName);
					eventChannel.setPath(channelPath);
					eventChannel.setIntegrationLogs(Boolean.valueOf(readString(channelDAO, "IntegrationLogs", "false")));
					eventChannel.setMaxQueueLength(readLong(channelDAO, "MaxQueueLength", 0));
					eventChannel.setMaxConsumers(readLong(channelDAO, "MaxConsumers", 0));
					eventChannel.setMaxSuppliers(readLong(channelDAO, "MaxSuppliers", 0));
					eventChannel.setRejectNewEvents(Boolean.valueOf(readString(channelDAO, "RejectNewEvents", "false")));
					eventChannel.setDiscardPolicy(EventChannelDiscardPolicy.valueOfForEnum(readString(channelDAO, "DiscardPolicy", "AnyOrder")));
					eventChannel.setEventReliability(EventChannelEventReliability.valueOfForEnum(readString(channelDAO, "EventReliability", "BestEffort")));
					eventChannel.setConnectionReliability(EventChannelConReliability.valueOfForEnum(readString(channelDAO, "ConnectionReliability", "BestEffort")));
					eventChannel.setPriority((short)readLong(channelDAO, "Priority", 0));
					eventChannel.setTimeout(readLong(channelDAO, "Timeout", 0));
					eventChannel.setOrderPolicy(EventChannelOrderPolicy.valueOfForEnum(readString(channelDAO, "OrderPolicy", "AnyOrder")));
					eventChannel.setStartTimeSupported(Boolean.valueOf(readString(channelDAO, "StartTimeSupported", "false")));
					eventChannel.setStopTimeSupported(Boolean.valueOf(readString(channelDAO, "StopTimeSupported", "false")));
					eventChannel.setMaxEventsPerConsumer(readLong(channelDAO, "MaxEventsPerConsumer", 0));
					
					session.persist(eventChannel);

					Set<Event> eventSet = eventChannel.getEvents();
					String[] events = readStringSeq(channelDAO, "Events", null);
					if (events != null)
					{
						for (String eventName : events)
						{
							Event event = new Event();
							event.setName(eventName);
							event.setEventChannel(eventChannel);
							event.setMaxProcessTime(readDouble(channelDAO, eventName+"/MaxProcessTime", 2.0));
							eventSet.add(event);
							session.persist(event);
						}
					}
					
				}
				
				try
				{
					DAOProxy notificationServiceMappingDAO = cdbAccess.createDAO("MACI/Channels/NotificationServiceMapping");
					
					String defaultNotSrv = notificationServiceMappingDAO.get_string("DefaultNotificationService");
					
					NotificationServiceMapping mappings = new NotificationServiceMapping();
					mappings.setConfiguration(config);
					mappings.setDefaultNotificationService(defaultNotSrv);
					session.persist(mappings);
					
					String[] domains = readStringSeq(notificationServiceMappingDAO, "Domains", null);
					if (domains != null)
					{
						for (String domain : domains) {
							String name = notificationServiceMappingDAO.get_string("Domains/" + domain + "/Name");
							String notSrv = notificationServiceMappingDAO.get_string("Domains/" + domain + "/NotificationService");
							DomainsMapping domainsMapping = new DomainsMapping();
							domainsMapping.setNotificationServiceMapping(mappings);
							domainsMapping.setName(name);
							domainsMapping.setNotificationService(notSrv);
							mappings.getDomainsMappings().add(domainsMapping);
							session.persist(domainsMapping);
						}
					}
					
					String[] channels = readStringSeq(notificationServiceMappingDAO, "Channels_", null);
					if (channels != null)
					{
						for (String channel : channels) {
							String name = notificationServiceMappingDAO.get_string("Channels_/" + channel + "/Name");
							String notSrv = notificationServiceMappingDAO.get_string("Channels_/" + channel + "/NotificationService");
							ChannelMapping channelsMapping = new ChannelMapping();
							channelsMapping.setNotificationServiceMapping(mappings);
							channelsMapping.setName(name);
							channelsMapping.setNotificationService(notSrv);
							mappings.getChannelMappings().add(channelsMapping);
							session.persist(channelsMapping);
						}
					}

				} catch (RuntimeException re) {
					// no mappings
				}
				m_logger.info("Imported Notification Channels from XML.");


				// plugin importEpilogue()
				if (plugin != null)
				{
					try
					{
						plugin.importEpilogue(session, config, cdbAccess);
					}
					catch (Throwable th)
					{
						// @TODO: Decent exception log.
						th.printStackTrace();
					}
				}

				hibernateUtil.commitTransaction();
				
				m_logger.info("Configuration from XML CDB loaded.");
			}
			catch (CDBFieldDoesNotExistEx ex) {
				throw AcsJCDBFieldDoesNotExistEx.fromCDBFieldDoesNotExistEx(ex);
			}
			catch (WrongCDBDataTypeEx ex) {
				throw AcsJWrongCDBDataTypeEx.fromWrongCDBDataTypeEx(ex);
			}
			catch (Throwable th) {
				throw new RuntimeException("Failed to fill-in the DB from CDB.", th);
			}
			finally
			{
				hibernateUtil.closeSession();
				cdbAccess.destroy();

				xmlCDB_._release();
				servantDelegate.shutdownEmbeddedWDALImpl();
				// destroy POA
				xmlCDBPOA.destroy(true, false);
			}
			
			return true;
		}
		catch (Throwable th)
		{
			m_logger.log(Level.SEVERE, "Failed to load XML CDB, exiting...", th);
			return false;
		}
		finally 
		{
			loadInProgress.set(false);
		}
	}

	/**
	 * @param xmlCDB
	 * @param componentName
	 * @param xml
	 * @return XML string, or null, but not an empty string.
	 * @throws CDBXMLErrorEx
	 * @throws DOMException
	 * @throws RuntimeException
	 */
	private String getComponentXML(final JDAL xmlCDB, String componentName, String xml) throws CDBXMLErrorEx, DOMException, RuntimeException {
		// try to create component, i.e. alma, branch (if available)
		try
		{
			// hierarchical check..
			String nodes = xmlCDB.list_nodes(COMPONENT_TREE_NAME + "/" + componentName);
			boolean isHierarhicalWithXML = (nodes != null) && (nodes.trim().length() > 0);
			if (isHierarhicalWithXML)
			{
				try
				{
					String nameOnly = componentName;
					int pos = nameOnly.lastIndexOf('/');
					if (pos > 0) nameOnly = nameOnly.substring(pos + 1);

					// reach for file
					File xmlFile = new File(m_root + File.separatorChar + COMPONENT_TREE_NAME + File.separatorChar + componentName + File.separatorChar + nameOnly + ".xml");
					//System.out.println(xmlFile.getAbsolutePath() + ", " + xmlFile.exists());
					final BufferedReader reader = new BufferedReader(new FileReader(xmlFile));
					final StringBuffer file = new StringBuffer();
					String line;
					while ((line = reader.readLine()) != null)
						file.append(line).append('\n');
					xml = file.toString();
				} catch (Throwable th) {
					throw new RuntimeException("Failed to read XML for component " + componentName);
				}
			}
			else
			{
				xml = xmlCDB.get_DAO(COMPONENT_TREE_NAME + "/" + componentName);
			}
		}
		catch (CDBRecordDoesNotExistEx rdne)
		{
			// noop, no data
		}
		
		// Oracle XMLTYPE attributes don't like empty XML, thus we convert it to null
		if (xml != null && xml.trim().isEmpty()) {
			xml = null;
		}

		return xml;
	}

	private final static Double limitDouble(final Double value)
	{
		if (value == null)
			return null;
		// TODO @todo Oracle throws overflow exception - reaseon for this method
		/*if (value.doubleValue() == Double.MAX_VALUE)
			return new Double(1.7976931348623155E308);
		else if (value.doubleValue() == Double.MIN_VALUE)
			return new Double(-1.7976931348623155E308);
		*/return new Double(value.floatValue());
	}

	private final static String nonEmptyString(final String value, final String defaultValue)
	{
		if (value == null)
			return null;
		if (value.length() == 0)
			return defaultValue;
		else
			return value;
	}
	
	private LoggingConfig persistLoggingConfig(Session session, DAOOperations dao, boolean required)
		throws CDBFieldDoesNotExistEx, WrongCDBDataTypeEx
	{
		LoggingConfig loggingConfig = new LoggingConfig();
		if (required)
		{
			// TODO can be optimized
			loggingConfig.setMinLogLevelDefault((byte)dao.get_long("LoggingConfig/minLogLevel"));
			loggingConfig.setMinLogLevelLocalDefault((byte)dao.get_long("LoggingConfig/minLogLevelLocal"));
			loggingConfig.setCentralizedLogger(dao.get_string("LoggingConfig/centralizedLogger"));
			loggingConfig.setDispatchPacketSize((byte)dao.get_long("LoggingConfig/dispatchPacketSize"));
			loggingConfig.setImmediateDispatchLevel((byte)dao.get_long("LoggingConfig/immediateDispatchLevel"));
			loggingConfig.setFlushPeriodSeconds((byte)dao.get_long("LoggingConfig/flushPeriodSeconds"));
			loggingConfig.setMaxLogQueueSize(dao.get_long("LoggingConfig/maxLogQueueSize"));
			loggingConfig.setMaxLogsPerSecond(dao.get_long("LoggingConfig/maxLogsPerSecond"));
		}
		else
		{
			// TODO can be optimized
			loggingConfig.setMinLogLevelDefault((byte)readLong(dao, "LoggingConfig/minLogLevel", 2));
			loggingConfig.setMinLogLevelLocalDefault((byte)readLong(dao, "LoggingConfig/minLogLevelLocal", 2));
			loggingConfig.setCentralizedLogger(readString(dao, "LoggingConfig/centralizedLogger", "Log"));
			loggingConfig.setDispatchPacketSize((byte)readLong(dao, "LoggingConfig/dispatchPacketSize", 10));
			loggingConfig.setImmediateDispatchLevel((byte)readLong(dao, "LoggingConfig/immediateDispatchLevel", 10));
			loggingConfig.setFlushPeriodSeconds((byte)readLong(dao, "LoggingConfig/flushPeriodSeconds", 10));
			loggingConfig.setMaxLogQueueSize(readLong(dao, "LoggingConfig/maxLogQueueSize", 1000));
			loggingConfig.setMaxLogsPerSecond(readLong(dao, "LoggingConfig/maxLogsPerSecond", -1));
		}
		session.persist(loggingConfig);

		final String[] LC_ATTRIBUTES  = {
			"minLogLevel",
			"minLogLevelLocal",
			"centralizedLogger",
			"dispatchPacketSize",
			"immediateDispatchLevel",
			"flushPeriodSeconds",
			"maxLogQueueSize",
			"maxLogsPerSecond"
		};
		
		String[] children;
		if (required)
			children = dao.get_string_seq("LoggingConfig/");
		else
			children = readStringSeq(dao, "LoggingConfig/", new String[0]);
		for (String childName : children)
		{
			boolean attribute = false;
			for (int i = 0; i < LC_ATTRIBUTES.length; i++)
				if (childName.equals(LC_ATTRIBUTES[i]))
				{
					attribute = true;
					break;
				}
			
			if (!attribute)
			{
				NamedLoggerConfig namedLoggerConfig = new NamedLoggerConfig();
				namedLoggerConfig.setLoggingConfig(loggingConfig);
				namedLoggerConfig.setName(dao.get_string("LoggingConfig/"+childName+"/Name"));
				namedLoggerConfig.setMinLogLevel((byte)dao.get_long("LoggingConfig/"+childName+"/minLogLevel"));
				namedLoggerConfig.setMinLogLevelLocal((byte)dao.get_long("LoggingConfig/"+childName+"/minLogLevelLocal"));
				session.persist(namedLoggerConfig);
			}
		}
		return loggingConfig;
	}
	
	/**
	 * Helper method to get all subnodes of the current proxy, removes ".xml" element from the list.
	 */
	private String[] getSubnodes(JDAL dal, String node) throws Throwable
	{
		ArrayList subnodes = new ArrayList();
	    
	    LinkedList stack = new LinkedList();
		stack.addLast(node);
		while (!stack.isEmpty())
		{
		    String parentNode = stack.removeLast().toString();
		    
		    String nodes = dal.list_nodes(parentNode);
			if (nodes.length() > 0)
			{
			    StringTokenizer tokenizer = new StringTokenizer(nodes);
			    while (tokenizer.hasMoreTokens())
			    {
			        String nodeName = tokenizer.nextToken();
			        if (nodeName.endsWith(".xml"))
			            continue;
			        
			        String fullName = parentNode + "/" + nodeName;
			        stack.addLast(fullName);
			        // strip off relative path
			        subnodes.add(fullName.substring(node.length()+1));
			    }
			}
		}				
	    
		String[] retVal = new String[subnodes.size()];
		subnodes.toArray(retVal);
		return retVal;
	}
	
	protected Map<String, Object> getAlmaBranch()
	{
		final Map<String, Object> almaRoot = new RootMap<String, Object>();

		try
		{
			Session session = hibernateUtil.getSession(); 
			schemaResourceResolverLoader.setSession(session);
			
			final Set<String> loadedComponents = new HashSet<String>();
			
			//
			// add control devices
			//
			if (plugin != null)
			{
				final HibernateWDALPlugin.ControlDeviceBindCallback bindCallback = new HibernateWDALPlugin.ControlDeviceBindCallback() {
					public void bindToComponentBranch(String name, String path, Object objectToBind) {
						bindToAlmaBranch(almaRoot, name, path, objectToBind);
						if (!loadedComponents.contains(path + "/" + name))
							loadedComponents.add(path + "/" + name);
					}
					public void bindNonExpandedXMLToComponentBranch(Session session, Component component) {
						alma.TMCDB.maci.Component comp = (alma.TMCDB.maci.Component)session.createCriteria(alma.TMCDB.maci.Component.class).
							add(Restrictions.eq("ComponentId", component.getComponentId())).uniqueResult();
						if (comp == null)
							throw new RuntimeException("Component with ID " + component.getComponentId() + " does not exist.");
						bindNonExpandedComponentXMLToAlmaBranch(session, almaRoot, comp);
					}
				};
				plugin.loadControlDevices(session, config, bindCallback);
			}
			
			//
			// add devices
			//
			Iterator componentList = session.createCriteria(alma.TMCDB.maci.Component.class).
				add(Restrictions.eq("Control", false)).
				add(Restrictions.eq("ConfigurationId", configId)).list().iterator();
			while (componentList.hasNext())
			{
				alma.TMCDB.maci.Component component = (alma.TMCDB.maci.Component)componentList.next();
				
				// already loaded by plugins, skip
				if (loadedComponents.contains(component.Path + "/" + component.getName()))
					continue;
				
		        String query = "FROM alma.TMCDB.baci.BACIPropertyType WHERE ComponentId = " + component.ComponentId;
		        List propertyList = session.createQuery(query).list();
		        if (propertyList.size() > 0)
		        {
		        	ComponentData componentData = new ComponentData();
		            try {
		            	componentData.setData(component.XMLDoc);
					} catch (Throwable e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

					// add properties
					for (Iterator iter = propertyList.iterator(); iter.hasNext(); )
					{
						BACIPropertyType baciProperty = (BACIPropertyType) iter.next();
						//componentData._.put(baciProperty.PropertyName, baciProperty);
						componentData._.put(baciProperty.PropertyName, new EmptyStringHandlerBACIPropertyType(baciProperty));
					}

					// bind object to map tree
					bindToAlmaBranch(almaRoot, component.getName(), component.Path, componentData);
				}
		        else if (component.XMLDoc != null)
		        {
		        	bindNonExpandedComponentXMLToAlmaBranch(session, almaRoot, component);
		        }
			}
			
		}
		catch (Throwable th)
		{
			th.printStackTrace();
		}
		
		return almaRoot;
	}
	
	private static class ComponentDAOImplSaver extends DAOImpl implements XMLSaver
	{
		private alma.TMCDB.maci.Component component;
		public ComponentDAOImplSaver(alma.TMCDB.maci.Component component, XMLTreeNode rootNode, POA poa, Logger logger, boolean silent) {
			super(component.getName(), rootNode, poa, logger, silent);
			this.component = component;
			rootNode.m_name = component.getName();	// TODO should be dynamically temp. set on toXML only
		}

		public void save() {
			component.XMLDoc = getRootNode().toString(false);
		}
		
		public void save(String xml) {
			component.XMLDoc = xml;
		}

	}

	public void bindNonExpandedComponentXMLToAlmaBranch(Session session, Map<String, Object> parentMap, alma.TMCDB.maci.Component component)
	{
		// from XMLDOC
		if (component.URN != null)
		{
			// now with XML create DAO object
			
			// use CDB XML handler which does not creates strings...
			XMLHandler xmlSolver = new XMLHandler(false, m_logger);
			try {
				synchronized (xmlNodeMonitor)
				{
					saxParser.parse(new InputSource(new StringReader(component.XMLDoc)), xmlSolver);
				}
			} catch (Exception e) {
				m_logger.log(AcsLogLevel.ERROR, "Failed to add component '" +  component.Path + "/" + component.getName(), e);
				return;
			}
			if (xmlSolver.m_errorString != null){
				m_logger.log(AcsLogLevel.ERROR, "Failed to add component '" +  component.Path + "/" + component.getName() + "': XML parser error: " + xmlSolver.m_errorString);
				return;
			}
	
			// create non-CORBA related, silent DAO
			DAOImpl dao = new ComponentDAOImplSaver(component, xmlSolver.m_rootNode, null, m_logger, true);
	
			// bind object to map tree
			bindToAlmaBranch(parentMap, component.getName(), component.Path, dao);
		}
		else
		{
        	m_logger.finer("No schema found for component " + component.Path + "/" + component.getName());
        	ComponentData componentData = new ComponentData();
 			try {
 				componentData.setData(component.XMLDoc);
			} catch (Throwable e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			// bind object to map tree
			bindToAlmaBranch(parentMap, component.getName(), component.Path, componentData);
		}		
	}

	protected void bindToAlmaBranch(Map<String, Object> parentMap, String name, String path, Object objectToBind)
	{
		// now find its submap
		path = getNormalizedPath(path); 
		while (path != null && path.length() > 0)
		{
			// remove trailing slashes, to have unique curl (used for key)
			if (path.charAt(0) == '/')
			{
				path = path.substring(1);
				continue;
			}
			
			int pos = path.indexOf('/');
			String parentPath = (pos > 0) ? path.substring(0, pos) : path;
			String subpath = (pos > 0) ? path.substring(pos+1, path.length()) : null;

			// hierarchical component (has it own XML) with children components
			Object parentObj = parentMap.get(parentPath);
			if (parentObj instanceof ComponentDAOImplSaver) {
				ComponentData cd = new ComponentData();
				try {
					cd.setData(((ComponentDAOImplSaver)parentObj).component.XMLDoc);
				} catch (Throwable th) {
					// should never happen, but still print it out
					th.printStackTrace();
				}
				parentObj = cd;
				parentMap.put(parentPath, parentObj);
			}
				
			ComponentData parentComponent = (ComponentData)parentObj;
			if (parentComponent == null)
			{
				parentComponent = new ComponentData();
				parentMap.put(parentPath, parentComponent);
			}

			parentMap = parentComponent._;
			path = subpath;
		}

		Object current = parentMap.get(name);
		if (current != null)
		{
	        if (current instanceof ComponentData && objectToBind instanceof ComponentDAOImplSaver)
	        {
				try {
					((ComponentData)parentMap.get(name)).setData(((ComponentDAOImplSaver)objectToBind).component.XMLDoc);
				} catch (Throwable th) {
					// should never happen, but still print it out
					th.printStackTrace();
				}
	        }
	        else
	        {
	        	// still override, but with warning
	        	m_logger.warning("Overriding component node: " + path + "/" + name);
	        	parentMap.put(name, objectToBind);
	        }
		}
		else
		{
			parentMap.put(name, objectToBind);
		}
	}

	public List getListForConfiguration(Session session, Class type) throws Throwable
	{
		List<Computer> result = null;
        try
        {
        	type.getMethod("getConfiguration", (Class[])null);
        	result = session.createCriteria(type).add(Restrictions.eq("configuration", config)).list();
        } 
        catch (NoSuchMethodException nsme) {
        	result = session.createCriteria(type).add(Restrictions.eq("ConfigurationId", configId)).list();
        }
        return result;
	}
	
	private String getAcsServices(Session session, Configuration config) throws Throwable
	{

		List serviceList = getListForConfiguration(session, AcsService.class);
		if (serviceList == null || serviceList.size() == 0)
			return null;
		
		StringBuilder sb = new StringBuilder();
		for (Object obj : serviceList)
		{
			AcsService acsService = (AcsService)obj;
			String toAdd = acsService.getServiceInstanceName();
			if (toAdd == null)
			{
				AcsServiceServiceType type = acsService.getServiceType();
				switch(type) {
				case NAMING:
				case MANAGER:
					toAdd = null; // noop since it is supported by manager by default
					break;
				case IFR:
					toAdd = "InterfaceRepository";
					break;
				case CDB:
					toAdd = "CDB";
					break;
				case NOTIFICATION:					
					toAdd = "NotifyEventChannelFactory";
					break;
				case LOGGING:
					toAdd = "Log";
					break;
				case ALARM:
					toAdd = "AcsAlarmService";
					break;
				case LOGPROXY:
					toAdd = "ACSLogSvc";
					break;
				default:
					m_logger.warning("Unknown AcsService type '" + type + "'");
				}
					
			}
			
			if (toAdd != null)
			{
				if (sb.length() > 0)
					sb.append(',');
				sb.append(toAdd);
			}
		}
		
		if (sb.length() > 0)
			return sb.toString();
		else
			return null;
	}

	protected synchronized void initializeRootNode()
	{
		m_logger.info("Loading configuration from the database...");

		Map<String, Object> rootMap = new RootMap<String, Object>();
		rootNode = rootMap;
		
		try
		{
			Session session = hibernateUtil.getSession();
			
			// plugin loadPrologue()
			if (plugin != null)
			{
				try
				{
					plugin.loadPrologue(session, config, rootMap);
				}
				catch (Throwable th)
				{
					this.m_logger.log(Level.SEVERE, "Failure in plugin " + plugin.getClass().getName(), th);
					// TODO: or should we let HD fail completely?
				}
			}

			// maci branch
			Map<String, Object> maciMap = new RootMap<String, Object>();
			maciMap.put("Components", getComponentsTableMap());
			maciMap.put("Containers", getContainersTableMap());
			Object channelsMap = getChannelsTableMap();
			if (channelsMap != null)
				maciMap.put("Channels", channelsMap);
			
			Map<String, Object> managersMap = new RootMap<String, Object>();
			List managerList = getListForConfiguration(session, alma.TMCDB.maci.Manager.class);
			if (!managerList.isEmpty())
				managersMap.put("Manager", managerList.get(0));
			else
				managersMap.put("Manager", getDefaultMangerConfig());
			
			String additionalServices = getAcsServices(session, config);
			if (additionalServices != null)
			{
				alma.TMCDB.maci.Manager manager = (alma.TMCDB.maci.Manager)managersMap.get("Manager");
				String currentServices = manager.getServiceComponents();
				if (currentServices == null || currentServices.length() == 0)
					manager.setServiceComponents(additionalServices);
				else
					// there might be duplicates, but they do not harm
					manager.setServiceComponents(currentServices + "," + additionalServices);
			}
			maciMap.put("Managers", managersMap);
			
			
			rootMap.put("MACI", maciMap);
			
			// component, i.e. alma, branch
			rootMap.put(COMPONENT_TREE_NAME, getAlmaBranch());

			// plugin loadEpilogue()
			if (plugin != null) {
				try {
					plugin.loadEpilogue(session, config, rootMap);
				}
				catch (Throwable th) {
					this.m_logger.log(Level.SEVERE, "Failure in plugin " + plugin.getClass().getName(), th);
					// TODO: or should we let HD fail completely?
				}
			}

			//org.hsqldb.util.DatabaseManager.main(new String[0]);
			
			// session check
			if (hibernateUtil.getSession() != mainSession) {
				throw new RuntimeException("Unexpected change of session...");
			}

			m_logger.info("Configuration loaded.");
		}
		catch (Throwable th)
		{
			th.printStackTrace();
		}
	}

	private static final alma.TMCDB.maci.Manager getDefaultMangerConfig()
	{
		return new alma.TMCDB.maci.Manager();
	}

	public Map<String, Object> getTableMap(Session session, String keyField, Class type) 
	{
		Map<String, Object> map = new RootMap<String, Object>();
		try
		{
			Method accessor = DOMJavaClassIntrospector.getAccessorMethod(type, keyField);
			Field field = null;
			if (accessor == null)
			{
				try
				{
					field = type.getField(keyField);
				}
				catch (Throwable th) {
					throw new IllegalStateException("failed to obtain key ");
				}
			}
			
			List list = getListForConfiguration(session, type);
			for (Object data : list)
			{
				String baseKey;
				if (accessor != null)
					baseKey = accessor.invoke(data, (Object[])null).toString();
				else //if (field != null)
					baseKey = field.get(data).toString();
				
				// should not be null
				
				// unique key generation
				int count = 0;
				String key = baseKey;
				while (map.containsKey(key))
					key = baseKey + String.valueOf(++count);
				
				map.put(key, data);
			}
		}
		catch (Throwable th)
		{
			th.printStackTrace();
		}
		return map;
	}
	

	public Map<String, Object> getComponentsTableMap() 
	{
		final String keyField = "Name";
		final Class type = alma.TMCDB.maci.Component.class;
		Map<String, Object> map = new LinkedHashMap<String, Object>();
		try
		{
			Session session = hibernateUtil.getSession();
			
			Method accessor = DOMJavaClassIntrospector.getAccessorMethod(type, keyField);
			Field field = null;
			if (accessor == null)
			{
				try
				{
					field = type.getField(keyField);
				}
				catch (Throwable th) {
					throw new IllegalStateException("failed to obtain key ");
				}
			}
			
			List list = getListForConfiguration(session, type);

			// Sort the list by path + component to ensure that parent components are added before their children
			Comparator<alma.TMCDB.maci.Component> comparator = new Comparator<alma.TMCDB.maci.Component>() {
				public int compare(alma.TMCDB.maci.Component o1, alma.TMCDB.maci.Component o2) {
					String fullName1 = ((o1.Path == null ? "" : o1.Path) + "/") + o1.getName();
					String fullName2 = ((o2.Path == null ? "" : o2.Path) + "/") + o2.getName();
					return fullName1.compareTo(fullName2);
				}
			};
			Collections.sort(list, comparator);

			for (Object data : list)
			{
				String baseKey;
				if (accessor != null)
					baseKey = accessor.invoke(data, (Object[])null).toString();
				else //if (field != null)
					baseKey = field.get(data).toString();
				
				// baseKey should not be null
				
				Map parentMap = map;
				alma.TMCDB.maci.Component component = (alma.TMCDB.maci.Component)data;
				
				// some cleaning
				if (component.getComponentLogger().getMinLogLevel() == -1 &&
					component.getComponentLogger().getMinLogLevelLocal() == -1)
					component.setComponentLogger(null);
				
				// now find its map
				String path = getNormalizedPath(component.Path); 
				while (path != null && path.length() > 0)
				{
					// remove trailing slashes, to have unique curl (used for key)
					if (path.charAt(0) == '/')
					{
						path = path.substring(1);
						continue;
					}
					
					int pos = path.indexOf('/');
					String parentPath = (pos > 0) ? path.substring(0, pos) : path;
					String subpath = (pos > 0) ? path.substring(pos+1, path.length()) : null;

					alma.TMCDB.maci.ComponentNode parentComponent = (alma.TMCDB.maci.ComponentNode)parentMap.get(parentPath);
					if (parentComponent == null)
					{
						parentComponent = new alma.TMCDB.maci.ComponentNode();
						parentMap.put(parentPath, parentComponent);
					}

					parentMap = parentComponent._;
					path = subpath;
				}
				
				// unique key generation
				int count = 0;
				String key = baseKey;
				while (parentMap.containsKey(key))
					key = baseKey + String.valueOf(++count);
				
				parentMap.put(key, data);
				
				if (data instanceof alma.TMCDB.maci.Component) {
					alma.TMCDB.maci.Component comp = (alma.TMCDB.maci.Component) data;
					m_logger.finer("Loaded component name=" + comp.Path + comp.getName() + ", type=" + comp.getType() + 
							", container=" + comp.getContainer() + ", implLang=" + comp.getImplLang());
				}
				else {
					m_logger.warning("Bad component class '" + data.getClass().getName() + "' read from TMCDB.");
				}

			}

		}
		catch (Throwable th)
		{
			th.printStackTrace();
		}
		
		return map;
	}
	

	public Map<String, Object> getContainersTableMap() 
	{
		final String keyField = "Name";
		final Class type = alma.TMCDB.maci.Container.class;
		Map<String, Object> map = new RootMap<String, Object>();
		try
		{
			Session session = hibernateUtil.getSession();

			Method accessor = DOMJavaClassIntrospector.getAccessorMethod(type, keyField);
			Field field = null;
			if (accessor == null)
			{
				try
				{
					field = type.getField(keyField);
				}
				catch (Throwable th) {
					throw new IllegalStateException("failed to obtain key ");
				}
			}
			
			List list = getListForConfiguration(session, type);
			// Sort the list by path + component to ensure that parent containers are added before their children
			Comparator<alma.TMCDB.maci.Container> comparator = new Comparator<alma.TMCDB.maci.Container>() {
				public int compare(alma.TMCDB.maci.Container o1, alma.TMCDB.maci.Container o2) {
					String fullName1 = ((o1.Path == null ? "" : o1.Path) + "/") + o1.getName();
					String fullName2 = ((o2.Path == null ? "" : o2.Path) + "/") + o2.getName();
					return fullName1.compareTo(fullName2);
				}
			};
			Collections.sort(list, comparator);

			for (Object data : list)
			{
				String baseKey;
				if (accessor != null)
					baseKey = accessor.invoke(data, (Object[])null).toString();
				else //if (field != null)
					baseKey = field.get(data).toString();
				
				// baseKey should not be null
				
				Map parentMap = map;
				alma.TMCDB.maci.Container container = (alma.TMCDB.maci.Container)data;
				
				// do not include this
				if (DUMMY_CONTAINER_FLAG.equals(container.getDeployInfo().getTypeModifiers()))
					continue;
				
				// some cleaning
				if (container.getDeployInfo() != null && container.getDeployInfo().getHost() == null)
					container.setDeployInfo(null);
				
				// now find its map
				String path = getNormalizedPath(container.Path); 
				while (path != null && path.length() > 0)
				{
					// remove trailing slashes, to have unique curl (used for key)
					if (path.charAt(0) == '/')
					{
						path = path.substring(1);
						continue;
					}
					
					int pos = path.indexOf('/');
					String parentPath = (pos > 0) ? path.substring(0, pos) : path;
					String subpath = (pos > 0) ? path.substring(pos+1, path.length()) : null;

					alma.TMCDB.maci.ContainerNode parentContainer = (alma.TMCDB.maci.ContainerNode)parentMap.get(parentPath);
					if (parentContainer == null)
					{
						parentContainer = new alma.TMCDB.maci.ContainerNode();
						parentMap.put(parentPath, parentContainer);
					}

					parentMap = parentContainer._;
					path = subpath;
				}
				
				// unique key generation
				int count = 0;
				String key = baseKey;
				while (parentMap.containsKey(key))
					key = baseKey + String.valueOf(++count);

				parentMap.put(key, data);
				
				if (data instanceof alma.TMCDB.maci.Container) {
					alma.TMCDB.maci.Container cont = (alma.TMCDB.maci.Container) data;
					m_logger.finer("Loaded container name=" + cont.Path + cont.getName() + ", implLang=" + cont.getImplLang());
				}
				else {
					m_logger.warning("Bad container class '" + data.getClass().getName() + "' read from TMCDB.");
				}
			}
		}
		catch (Throwable th)
		{
			th.printStackTrace();
		}
		
		return map;
	}

	
	public Object getChannelsTableMap() 
	{
		final String keyField = "Name";
		final Class type = alma.TMCDB.maci.EventChannel.class;
		try
		{
			Session session = hibernateUtil.getSession();

			Method accessor = DOMJavaClassIntrospector.getAccessorMethod(type, keyField);
			Field field = null;
			if (accessor == null)
			{
				try
				{
					field = type.getField(keyField);
				}
				catch (Throwable th) {
					throw new IllegalStateException("failed to obtain key ");
				}
			}
			
			boolean channelsExist = false;
			Channels channels = new Channels();
			
    		alma.TMCDB.maci.NotificationServiceMapping mapping =
				(alma.TMCDB.maci.NotificationServiceMapping)session.createCriteria(alma.TMCDB.maci.NotificationServiceMapping.class).add(Restrictions.eq("ConfigurationId", configId)).uniqueResult();
			if (mapping != null) {
				channels.setNotificationServiceMapping(mapping);
				channelsExist = true;
			}
	    
			List list = getListForConfiguration(session, type);
			
			if (list.size() > 0) {
				channelsExist = true;
			}
			
			Map<String, EventChannelNode> map;
			if (channelsExist) {
				map = channels._;
			}
			else
			{
				return null; //map = new RootMap<String, EventChannelNode>();
			}
			
			for (Object data : list)
			{
				String baseKey;
				if (accessor != null)
					baseKey = accessor.invoke(data, (Object[])null).toString();
				else //if (field != null)
					baseKey = field.get(data).toString();
				
				// baseKey should not be null
				
				Map parentMap = map;
				alma.TMCDB.maci.EventChannel channel = (alma.TMCDB.maci.EventChannel)data;
				
				// now find its map
				String path = getNormalizedPath(channel.Path); 
				while (path != null && path.length() > 0)
				{
					// remove trailing slashes, to have unique curl (used for key)
					if (path.charAt(0) == '/')
					{
						path = path.substring(1);
						continue;
					}
					
					int pos = path.indexOf('/');
					String parentPath = (pos > 0) ? path.substring(0, pos) : path;
					String subpath = (pos > 0) ? path.substring(pos+1, path.length()) : null;

					alma.TMCDB.maci.EventChannelNode parentChannel = (alma.TMCDB.maci.EventChannelNode)parentMap.get(parentPath);
					if (parentChannel == null)
					{
						parentChannel = new alma.TMCDB.maci.EventChannelNode();
						parentMap.put(parentPath, parentChannel);
					}

					parentMap = parentChannel._;
					path = subpath;
				}
				
				// unique key generation
				int count = 0;
				String key = baseKey;
				while (parentMap.containsKey(key))
					key = baseKey + String.valueOf(++count);

				parentMap.put(key, data);
			}
			
			return channels;
		}
		catch (Throwable th)
		{
			th.printStackTrace();
		}
		
		return null;
	}

	
	// remove alma prefix
	private static final String getNormalizedPath(String path)
	{
		if (path != null && path.length() > 0)
		{
			final String ALMA = COMPONENT_TREE_NAME;
			final String ALMA_PREFIX = ALMA + "/";
		
			if (path.equals(ALMA))
				return null;
			else if (path.startsWith(ALMA_PREFIX))
				return path.substring(ALMA_PREFIX.length());
			else
				return path;
		}
		else
			return null;
	}
	
	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#get_DAO_Servant(java.lang.String)
	 */
	public DAO get_DAO_Servant(String curl) throws CDBXMLErrorEx, CDBRecordDoesNotExistEx {
		checkAccess();
		
		// remove trailing slashes, to have unique curl (used for key)
		if (curl.length() > 0 && curl.charAt(0) == '/')
			curl = curl.substring(1);
		
		// make sure there are no identical DAOs created
		synchronized (daoMap)
		{
			// get cached
			if (daoMap.containsKey(curl))
				return daoMap.get(curl);

			Object node = curl.length() == 0 ? rootNode : DOMJavaClassIntrospector.getNode(curl, rootNode);
			if (node == null || DOMJavaClassIntrospector.isPrimitive(node.getClass()))
			{
				AcsJCDBRecordDoesNotExistEx ex = new AcsJCDBRecordDoesNotExistEx();
				ex.setCurl(curl);
				m_logger.log(AcsLogLevel.NOTICE, "DAL::get_DAO_Servant " + ex.getShortDescription());
				throw ex.toCDBRecordDoesNotExistEx();
			}
			
			try
			{
				// create object id
				byte[] id = curl.getBytes();

				Object objImpl = null;
				DAOPOA daoImpl = null;
				if (node instanceof DAOPOA)
					objImpl = daoImpl = (DAOPOA)node;
				else if (node instanceof DAOImpl)
					objImpl = daoImpl = new DAOPOATie((DAOImpl)node);
				else if (node instanceof XMLTreeNode)
					//objImpl = daoImpl = new DAOImpl(curl, (XMLTreeNode)node, poa, m_logger);
					objImpl = daoImpl = new DAOPOATie(new NoDestroyDAOImpl(curl, (XMLTreeNode)node, poa, m_logger));
				else
				{
					//daoImpl = new HibernateDAOImpl(curl, node, poa, m_logger);
					HibernateWDAOImpl impl = new HibernateWDAOImpl(mainSession, curl, node, poa, m_logger);
					objImpl = impl;
					daoImpl = new DAOPOATie(impl);
					impl.setSetvant(daoImpl);
				}

				// activate object
				poa.activate_object_with_id(id, daoImpl);
				DAO href = DAOHelper.narrow(poa.servant_to_reference(daoImpl));

				// map DAO reference
				daoMap.put(curl, href);
				daoObjMap.put(curl, objImpl);
				
				m_logger.log(AcsLogLevel.INFO, "Returning DAO servant for: " + curl);
				return href;
			}
			catch (Throwable t)
			{
				// @todo not clean, just to be consistent v DAL impl
				String info = "DAL::get_DAO_Servant " + t;
				AcsJCDBXMLErrorEx xmlErr = new AcsJCDBXMLErrorEx(t);
				xmlErr.setErrorString(info);
				m_logger.log(AcsLogLevel.NOTICE, info);
				throw xmlErr.toCDBXMLErrorEx();
			}
			
		}
	}


	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#get_DAO(java.lang.String)
	 */
	public String get_DAO(String curl) throws CDBXMLErrorEx, CDBRecordDoesNotExistEx {
		checkAccess();

		// remove trailing slashes, to have unique curl (used for key)
		if (curl.length() > 0 && curl.charAt(0) == '/')
			curl = curl.substring(1);
		
		m_logger.log(AcsLogLevel.INFO, "Returning XML record for: " + curl);
		
		Object node = curl.length() == 0 ? rootNode : DOMJavaClassIntrospector.getNode(curl, rootNode);
		if (node == null  || DOMJavaClassIntrospector.isPrimitive(node.getClass()))
		{
			AcsJCDBRecordDoesNotExistEx ex = new AcsJCDBRecordDoesNotExistEx();
			ex.setCurl(curl);
			String detailMsg = ( node == null ? "node is null." : "node is primitive (" + node.getClass().getName() + ").");
			m_logger.log(AcsLogLevel.NOTICE, detailMsg, ex);
			throw ex.toCDBRecordDoesNotExistEx();
		}
		else if (node instanceof DAOImpl) {
			String ret = ((DAOImpl)node).getRootNode().toString(false);
			m_logger.finest("get_DAO(" + curl + ") returning " + ret);
			return ret;
		}

		// remove last slash
		if (curl.length() > 0 && curl.charAt(curl.length() - 1) == '/')
			curl = curl.substring(0, curl.length() - 1);
		
		// get node name only
		String name;
		int pos = curl.lastIndexOf('/');
		if (pos == -1)
			name = curl;
		else
			name = curl.substring(pos+1, curl.length());

		// root
		if (name.length() == 0)
			name = "root";
		
		try
		{
			String ret = "<?xml version='1.0' encoding='ISO-8859-1'?>" + DOMJavaClassIntrospector.toXML(DOMJavaClassIntrospector.getRootNodeXMLName(name, node), node, curl, m_logger);
			m_logger.finest("get_DAO(" + curl + ") returning " + ret);
			return ret;
		}
		catch (Throwable t)
		{
			t.printStackTrace();
			String info = "DAL::get_DAO " + t;
			AcsJCDBXMLErrorEx xmlErr = new AcsJCDBXMLErrorEx(t);
			xmlErr.setErrorString(info);
			m_logger.log(AcsLogLevel.NOTICE, info);
			throw xmlErr.toCDBXMLErrorEx();
		}
	}


	protected String listNodes(String curl, boolean daosOnly) {

		final String EMPTY_STRING = "";
		
		if (curl == null)
			curl = EMPTY_STRING;
		
		Object node = DOMJavaClassIntrospector.getNode(curl, rootNode);
		if (node == null  || DOMJavaClassIntrospector.isPrimitive(node.getClass()))
		{
			/*
			AcsJCDBRecordDoesNotExistEx ex = new AcsJCDBRecordDoesNotExistEx();
			ex.setCurl(curl);
			m_logger.log(AcsLogLevel.NOTICE, "list_nodes " + ex.getShortDescription());
			throw ex.toCDBRecordDoesNotExistEx();
			*/
			return EMPTY_STRING;
		}
		else
		{
			if (daosOnly)
			{
				if (node instanceof RootMap)
					return "";
				else
				{
					if (DOMJavaClassIntrospector.getFields(node).length > 0 || DOMJavaClassIntrospector.getNodes(node).length > 0)
						return DOMJavaClassIntrospector.stringifyArray(DOMJavaClassIntrospector.getNodes(node), ' ') + " ";
					else
						return "";
				}
			}
			
			ArrayList<String> listedNodex = new ArrayList<String>();
			String[] subnodes = DOMJavaClassIntrospector.getSubnodes(node);
			for (String subnode : subnodes)
			{
				Object childNode = DOMJavaClassIntrospector.getNode(subnode, node);
				if (childNode == null)
					continue;
				
				boolean hasMapChildren = false;
				if (!(childNode instanceof RootMap) && DOMJavaClassIntrospector.isMapSubnode(subnode, node))
				{
					hasMapChildren = true;
					Object subsubMap = DOMJavaClassIntrospector.getChild(DOMJavaClassIntrospector.SUBNODES_MAP_NAME, childNode);
					if (subsubMap instanceof Map && ((Map)subsubMap).size() > 0)
						hasMapChildren = false;
				}
				if (!hasMapChildren)
					listedNodex.add(subnode);
			}
			return DOMJavaClassIntrospector.stringifyArray(listedNodex.toArray(new String[listedNodex.size()]), ' ');
		}
	}
	
	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#list_nodes(java.lang.String)
	 */
	public String list_nodes(String curl) {
		checkAccess();
		String ret = listNodes(curl, false);
		m_logger.finest("list_nodes(=" + curl + ") returning " + ret);
		return ret;
	}

	
	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#list_daos(java.lang.String)
	 */
	public String list_daos(String name) {
		checkAccess();
		String ret = listNodes(name, true);
		m_logger.finest("list_daos(=" + name + ") returning " + ret);
		return ret;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DALOperations#configuration_name()
	 */
	public String configuration_name() {
		return configName;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDALOperations#add_node(java.lang.String, java.lang.String)
	 */
	public void add_node(String curl, String xml) throws CDBExceptionEx, CDBXMLErrorEx, CDBRecordAlreadyExistsEx {
		checkAccess();
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDALOperations#remove_node(java.lang.String)
	 */
	public void remove_node(String curl) throws CDBRecordIsReadOnlyEx, CDBRecordDoesNotExistEx {
		checkAccess();
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDALOperations#get_WDAO_Servant(java.lang.String)
	 */
	public WDAO get_WDAO_Servant(String curl) throws CDBRecordIsReadOnlyEx, CDBXMLErrorEx, CDBRecordDoesNotExistEx {
		checkAccess();
		
		// remove trailing slashes, to have unique curl (used for key)
		if (curl.length() > 0 && curl.charAt(0) == '/')
			curl = curl.substring(1);
		
		// make sure there are no identical DAOs created
		synchronized (wdaoMap)
		{
			// get cached
			if (wdaoMap.containsKey(curl))
				return (WDAO)wdaoMap.get(curl);

			Object node = curl.length() == 0 ? rootNode : DOMJavaClassIntrospector.getNode(curl, rootNode);
			if (node == null || DOMJavaClassIntrospector.isPrimitive(node.getClass()))
			{
				AcsJCDBRecordDoesNotExistEx ex = new AcsJCDBRecordDoesNotExistEx();
				ex.setCurl(curl);
				m_logger.log(AcsLogLevel.NOTICE, "WDAL::get_WDAO_Servant " + ex.getShortDescription());
				throw ex.toCDBRecordDoesNotExistEx();
			}
			
			try
			{
				Object objImpl = null;
				WDAOPOA wdaoImpl = null;
				if (node instanceof WDAOPOA)
					objImpl = wdaoImpl = (WDAOPOA)node;
				//else if (node instanceof XMLTreeNode)
				//{
		        //	DAOImpl daoImpl = new DAOImpl(curl, (XMLTreeNode)node, poa, m_logger);
				//	objImpl = wdaoImpl = new WDAOImpl(this, curl, daoImpl, poa, m_logger);
				//}
				else
				{
					HibernateWDAOImpl impl = new HibernateWDAOImpl(mainSession, curl, node, poa, m_logger);
					objImpl = impl;
					wdaoImpl = new WDAOPOATie(impl);
					impl.setSetvant(wdaoImpl);
				}
				
				// create object id
				byte[] id = ("WDAO"+curl).getBytes();
				// activate object
				poa.activate_object_with_id(id, wdaoImpl);
				WDAO href = WDAOHelper.narrow(poa.servant_to_reference(wdaoImpl));

				// map DAO reference
				wdaoMap.put(curl, href);
				wdaoObjMap.put(curl, objImpl);

				m_logger.log(AcsLogLevel.INFO, "Returning WDAO servant for: " + curl);
				return href;
			}
			catch (Throwable t)
			{
				// @todo not clean, just to be consistent v DAL impl
				String info = "WDAL::get_WDAO_Servant " + t;
				AcsJCDBXMLErrorEx xmlErr = new AcsJCDBXMLErrorEx(t);
				xmlErr.setErrorString(info);
				m_logger.log(AcsLogLevel.NOTICE, info);
				throw xmlErr.toCDBXMLErrorEx();
			}
			
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDALOperations#set_DAO(java.lang.String, java.lang.String)
	 */
	public void set_DAO(String curl, String xml) throws CDBFieldDoesNotExistEx, CDBRecordIsReadOnlyEx, CDBExceptionEx, CDBXMLErrorEx, CDBRecordDoesNotExistEx {
		checkAccess();

		m_logger.log(AcsLogLevel.INFO, "set_DAO: " + curl);

		
		
		// read given xml and iterate through its content and check if something was changed
		DAOImpl daoImp = null;
		XMLHandler daoXMLSolver = null;

		// get content of the given xml string using parser without any shemas and validation
		// since given xml string come from a client that have no shemas and it is full expanded version
		// of existing xml or it is small composed xml of few properties
		XMLHandler xmlSolver = new XMLHandler(false, m_logger);
		// TODO markArrays == 2 impl. is a mess... I think lot of code could be removed!
		//xmlSolver.setMarkArrays(2);
		parseXML(xml, xmlSolver);
		
		
		
		Object node = DOMJavaClassIntrospector.getNode(curl, rootNode);
		if (node == null) {
			AcsJCDBRecordDoesNotExistEx ex = new AcsJCDBRecordDoesNotExistEx();
			ex.setCurl(curl);
			throw ex.toCDBRecordDoesNotExistEx();
		}

		// node is saved as XML
		if (node instanceof XMLSaver)
		{
			XMLSaver saver = (XMLSaver)node;
			
			Transaction tr = null;
			try
			{
				tr = mainSession.beginTransaction();
				saver.save(xml);
				tr.commit();
			} catch (Throwable th) {
				if (tr != null)
					tr.rollback();
				m_logger.log(AcsLogLevel.NOTICE, "Failed to set DAO: " + curl, th);
				AcsJCDBExceptionEx cdbex = new AcsJCDBExceptionEx(th);
				throw cdbex.toCDBExceptionEx();
			}
			
			return;
		}

		
		
		// get original xml that we will use to compare
		xml = get_DAO(curl);
		daoXMLSolver = new XMLHandler(false, m_logger);
		parseXML(xml, daoXMLSolver);
		daoImp = new DAOImpl(curl, daoXMLSolver.m_rootNode, poa, m_logger);

		// iterater throuth given xml and put changed attributes in map
		LinkedHashMap map = new LinkedHashMap();
		try{
			checkforChanges("", xmlSolver.m_rootNode, map, daoImp);
			saveChanges(curl, map);
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}catch(AcsJCDBXMLErrorEx e){
			throw e.toCDBXMLErrorEx();
		}
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
		throws CDBXMLErrorEx, CDBExceptionEx, CDBFieldDoesNotExistEx, CDBRecordDoesNotExistEx
	{

		Object node = curl.length() == 0 ? rootNode : DOMJavaClassIntrospector.getNode(curl, rootNode);
		if (node == null || DOMJavaClassIntrospector.isPrimitive(node.getClass()))
		{
			AcsJCDBRecordDoesNotExistEx ex = new AcsJCDBRecordDoesNotExistEx();
			ex.setCurl(curl);
			m_logger.log(AcsLogLevel.NOTICE, "WDAL::saveChanges " + ex.getShortDescription());
			throw ex.toCDBRecordDoesNotExistEx();
		}
		
		boolean commit = false;
		Transaction tr = mainSession.beginTransaction();
		try
		{
			HibernateWDAOImpl wdao = new HibernateWDAOImpl(mainSession, curl, node, poa, m_logger, false, false);
			for (Object key : propertyMap.keySet())
			{
				String propertyName = key.toString();
				wdao.set_field_data(propertyName, propertyMap.get(propertyName).toString());
			}
			commit = true;
		}
		catch (CDBFieldIsReadOnlyEx fne) {
			AcsJCDBExceptionEx cdbex = new AcsJCDBExceptionEx(fne);
			throw cdbex.toCDBExceptionEx();
		}
		catch (WrongCDBDataTypeEx fne) {
			AcsJCDBExceptionEx cdbex = new AcsJCDBExceptionEx(fne);
			throw cdbex.toCDBExceptionEx();
		}
		finally
		{
			if (commit)
				tr.commit();
			else
				tr.rollback();
		}
		
		/// TODO revert memory state?!!!
	}
	
	// ------------------------------------------------------------------------------
	// code below copied from DALImpl
	// ------------------------------------------------------------------------------
	
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

	private void parseXML(String xml, XMLHandler xmlSolver)
	throws CDBXMLErrorEx
	{
		try {
			SAXParserFactory factory = SAXParserFactory.newInstance();
			SAXParser saxParser = factory.newSAXParser();
			synchronized (xmlNodeMonitor)
			{
				saxParser.parse(new InputSource(new StringReader(xml)), xmlSolver);
			}
			
			if(xmlSolver.m_errorString != null) {
				String info = "XML parser error: " + xmlSolver.m_errorString;
				CDBXMLErrorEx xmlErr = new CDBXMLErrorEx();
				m_logger.log(AcsLogLevel.NOTICE, info);
				throw xmlErr;
			}
		} catch(Throwable t) {
			String info = "SAXException: " + t.getMessage();
			m_logger.log(AcsLogLevel.NOTICE, info);
			AcsJCDBXMLErrorEx xmlErr = new AcsJCDBXMLErrorEx(t);
			xmlErr.setErrorString(info);
			throw xmlErr.toCDBXMLErrorEx();
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

	public void shutdown() {
		shutdown.set(true);
		orb.shutdown(false);
	}

	private void objectChangedForMap(String curl, Map map, Map objMap)
	{
		synchronized (map) {
			if (map.containsKey(curl)) {
				DAO dao = (DAO) map.get(curl);
				Object objDAO = objMap.get(curl);
				
				Object node = curl.length() == 0 ? rootNode : DOMJavaClassIntrospector.getNode(curl, rootNode);
				if (node == null || DOMJavaClassIntrospector.isPrimitive(node.getClass()))
				{
					// no longer exists
					dao.destroy();
					objMap.remove(curl);
					map.remove(curl);
				}
				
				if (objDAO instanceof DAOImpl)
				{
					if (node instanceof XMLTreeNode)
						((DAOImpl)objDAO).setRootNode((XMLTreeNode)node);
					else
					{
						// type changed, destroy this one and reactivate new
						dao.destroy();
						objMap.remove(curl);
						map.remove(curl);
						try {
							get_DAO(curl);
						} catch (Throwable th) { th.printStackTrace(); }
					}
				}
				else if (objDAO instanceof HibernateWDAOImpl)
				{
					if (node instanceof XMLTreeNode)
					{
						// type changed, destroy this one and reactivate new
						dao.destroy();
						map.remove(curl);
						try {
							get_DAO(curl);
						} catch (Throwable th) { th.printStackTrace(); }
					}
					else
						((HibernateWDAOImpl)objDAO).setRootNode(node);
				}
				
			}
		}
	}
	/**
	 * @param curl
	 */
	protected void object_changed(String curl) {
		objectChangedForMap(curl, daoMap, daoObjMap);
		objectChangedForMap(curl, wdaoMap, wdaoObjMap);
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
				synchronized (regListeners) {
					regListeners.put(id, listener);
				}
			}

			// then listened curls
			String curl;
			while (true) {
				curl = reader.readLine();
				if (curl == null)
					break;
				ArrayList<Integer> arr = new ArrayList<Integer>();
				while (true) {
					line = reader.readLine();
					if (line == null || line.length() == 0)
						break;
					arr.add(new Integer(line));
				}
				synchronized (listenedCurls) {
					listenedCurls.put(curl, arr);
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	static final long LISTENER_RECOVERY_FILE_SAVE_FREQ_MS = 10000;	// 10s
	
	private void startRecoverySaver()
	{
		if (lastSaveTime.compareAndSet(0, System.currentTimeMillis()))
		{
			new Thread(new Runnable() {
				
				@Override
				public void run() {
					saveLoop();
				}
			}, "recovery file saver").start();
		}
		
	}
	
	private void saveLoop()
	{
		while (!shutdown.get())
		{
			// NOTE: solution with "sleep(LISTENER_RECOVERY_FILE_SAVE_FREQ_MS)"
			// gives worst time of 2*LISTENER_RECOVERY_FILE_SAVE_FREQ_MS time between saves
			try {
				Thread.sleep(1000);
			} catch (InterruptedException ie) { /* noop */ }
			
			long now = System.currentTimeMillis();
			if ((now - lastSaveTime.get()) >= LISTENER_RECOVERY_FILE_SAVE_FREQ_MS)
				saveListenersInternal();
		}
	}

	/**
	 * @return boolean
	 */
	public boolean saveListeners() {
		saveRequest.set(true);
		return true;
	}
	
	/**
	 * @return boolean
	 */
	private boolean saveListenersInternal() {
		
		synchronized (lastSaveTime)
		{
			if (!saveRequest.getAndSet(false))
				return true;
			
			// set last time this method was called, also in case of failure
			lastSaveTime.set(System.currentTimeMillis());
			
			String key, ior;
			File storageFile = getStorageFile();
			if (storageFile == null)
				return false;
			try {
				OutputStream out = new FileOutputStream(storageFile);
				//listenedCurls.store(new FileOutputStream(storageFile), "Listeners");
				BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(out));
				// write listeners
				synchronized (regListeners) {
					Iterator reg = regListeners.keySet().iterator();
					while (reg.hasNext()) {
						Integer id = (Integer) reg.next();
						writer.write(id.toString());
						writer.newLine();
						ior = orb.object_to_string(regListeners.get(id));
						writer.write(ior);
						writer.newLine();
					}
				}
				// write listened curls
				synchronized (listenedCurls) {
					Iterator<String> iter = listenedCurls.keySet().iterator();
					while (iter.hasNext()) {
						key = iter.next();
						ArrayList arr = listenedCurls.get(key);
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
				}
				writer.flush();
			} catch (Exception e) {
				e.printStackTrace();
				return false;
			}
			return true;
		}
	}

	public int add_change_listener(DALChangeListener listener) {
		int id;
		while (true) {
			id = idPool.nextInt(Integer.MAX_VALUE);
			Integer key = new Integer(id);
			synchronized (regListeners) {
				if (!regListeners.containsKey(key)) {
					regListeners.put(key, listener);
					break;
				}
			}
		}
		return id;
	}

	public void listen_for_changes(String curl, int listenerID) {
		synchronized (listenedCurls) {
			ArrayList<Integer> listeners = listenedCurls.get(curl);
			if (listeners == null) {
				listeners = new ArrayList<Integer>();
				listenedCurls.put(curl, listeners);
			}
			listeners.add(listenerID);
			saveListeners();
		}
	}
	public void remove_change_listener(int listenerID) {
		synchronized (listenedCurls) {
			Iterator iter = listenedCurls.keySet().iterator();
			// deattach this listener from its curls
			while (iter.hasNext()) {
				String curl = (String) iter.next();
				ArrayList<Integer> listeners = listenedCurls.get(curl);
				if (listeners != null) {
					//listeners.remove(listener);
					for (int i = 0; i < listeners.size(); i++) {
						Integer id = listeners.get(i);
						if (id.intValue() == listenerID) {
							listeners.remove(i);
							i--; // just to test shifted
						}
					}
				}
			}
			// and delete it from list
			synchronized (regListeners) {
				regListeners.remove(new Integer(listenerID));
			}
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
				DALChangeListener listener = null;
				synchronized (regListeners) {
					listener = (DALChangeListener) regListeners.get(listeners.get(i));
				}
				if( listener == null ) {
					listeners.remove(i);
					i--;
				}
			}
		}
	}

	public void clear_cache(String curl) {
		// NOTICE: this really does not reload the data from DB...
		
		// first take care of our own map
		object_changed(curl);

		// then notify all registered listeners
		synchronized (listenedCurls) {
			boolean needToSave = false;
			ArrayList<Integer> listeners = listenedCurls.get(curl);
			if (listeners == null) {
				return;
			}
			ArrayList<Integer> invalidListeners = new ArrayList<Integer>();
			for (int i = 0; i < listeners.size(); i++) {
				DALChangeListener listener = null;
				synchronized (regListeners) {
					listener = regListeners.get(listeners.get(i));
				}
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
				synchronized (regListeners) {
					regListeners.remove(invalidListeners.get(i));
				}
				needToSave = true;
			}
			if (needToSave) {
				cleanListenedCurls();
				saveListeners();
			}
		}
	}
	
	volatile boolean firstTime = true;
	public void clear_cache_all() {

		if (recoveryRead && firstTime)
			firstTime = false;
		else
		{
			reloadData();
		}

		Set<String> curls = new HashSet<String>();
		synchronized (daoMap) {
			curls.addAll(daoMap.keySet());
		}
		synchronized (wdaoMap) {
			curls.addAll(wdaoMap.keySet());
		}
		
		synchronized (listenedCurls) {
			Iterator iter = listenedCurls.keySet().iterator();
			while (iter.hasNext()) {
				String curl = (String) iter.next();
				curls.add(curl);
			}
		}

		for (String curl : curls)
			clear_cache(curl);
		
	}

}
