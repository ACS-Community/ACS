/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.cdb;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALChangeListener;
import com.cosylab.CDB.DALChangeListenerPOA;
import com.cosylab.CDB.DALHelper;
import com.cosylab.CDB.DAOOperations;

import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;
import com.cosylab.cdb.jdal.DAOImpl;
import com.cosylab.cdb.jdal.XMLHandler;

import org.omg.CORBA.ORB;
import org.xml.sax.InputSource;

import alma.acs.util.ACSPorts;

/**
 * Class managing CDB access (establishing connection to the CDB,
 * observing DAO changes, and providing accessor methods to the user).
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class CDBAccess
{

	/**
	 * The constant denoting the prefix of the DAL configuration keys.
	 */
	private static final String CDBDAL_PREFIX = "DAL";

	/**
	 * The constant denoting the prefix of the DAO configuration keys.
	 */
	private static final String CDBDAO_PREFIX = "DAO";

	/**
	 * The constant denoting the name of default DAL reference property key.
	 */
	private static final String CDBDAL_DEFAULT_REFERENCE = CDBDAL_PREFIX + ".defaultReference";

	/**
	 * The constant denoting the default DAL reference (if non is set).
	 */
	private static final String DEFAULT_REFERENCE = "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getCDBPort() + "/CDB";

	/**
	 * The constant denoting the name of default DAO remote switch property key.
	 */
	private static final String CDBDAO_REMOTE = CDBDAO_PREFIX + ".remote";

	/**
	 * Switch to turn remote DAO on.
	 */
	private boolean remoteDAO = true;

	/**
	 * Default DAL CORBA reference (if not authority is specified).
	 */
	private String defaultDAL = null;

	/**
	 * DAL reference.
	 */
	private DAL dalReference = null;

	/**
	 * CORBA Object Request Broker (ORB) reference.
	 */
	private ORB orb = null;

	/**
	 * Logger.
	 */
	private Logger logger = null;

	/**
	 * Listener for CDB change.
	 */
	private ChangeListener changeListener = null;

	/**
	 * This private class will handle CDB restart or data change in the CDB.
	 * 
	 * @author		Dragan Vitas (dragan.vitas@cosylab.com)
	 * @version	@@VERSION@@
	 */
	private class ChangeListener extends DALChangeListenerPOA
	{
		/**
		 * Map of monitored CURLs entities.
		 */
		private HashMap curlMap = new HashMap();
		
		/**
		 * Map of registered listeners on DALs.
		 */
		private HashMap dalMap = new HashMap();
		
		/**
		 * CORBA instance of <code>DALChangeListener</code> object.
		 */
		private DALChangeListener cl = null;

		/**
		 * Reconnect connectable in separate thread to avoid blocking.
		 */
		private class ReconnectTask extends Thread
		{
			/**
			 * Object to be reconnected.
			 */
			DAOProxy connectable;
			
			/**
			 * Constructor of the class.
			 * 
			 * @param	connectable	object to be reconnected.
			 */
			public ReconnectTask(DAOProxy connectable)
			{
				this.connectable = connectable;
			}
			
			/**
			 * Thread worker implementation.
			 */
			public void run()
			{
				final int RETRIES = 3;
				
				for (int i = 0; i < RETRIES; i++)
				{
					try
					{
						internalConnect(connectable);
						break;
					}
					catch (Throwable th)
					{
						// if we cannot reestablish connection the exception will report elsewhere
					}
				}
			}
		}

		/**
		 * Called from DAL server when curl changed or when DAL startups.
		 * NOTE: reconnection does not work if DAL is running on Java (Sun) CORBA - server problem (DAL reference!)
		 * 
		 * @param	curl	changed entity in CDB
		 * @see DALChangeListenerOperations#object_changed(String)
		 */
		public void object_changed(String curl)
		{
			// get the connectable from cache and reconnect it in separate thread
			DAOProxy connectable = (DAOProxy) curlMap.get(curl);
			if (connectable != null)
				new ReconnectTask(connectable).start();
		}

		/**
		 * Add listener for curl on DAL server so the connectable object can be reconnected.
		 * 
		 * @param dal the reference of the DAL server where the curl is obtained
		 * @param curl the path for our DAO object
		 * @param conn the object for which we made the DAO
		 */
		public void handle(DAL dal, String curl, DAOProxy conn)
		{
			// create CORBA instance
			if (cl == null)
				cl = changeListener._this(orb);
			
			// register DAL listener, if not already
			Integer listenerID = (Integer) dalMap.get(dal);
			if (listenerID == null)
			{
				int id = dal.add_change_listener(cl);
				listenerID = new Integer(id);
				dalMap.put(dal, listenerID);
			}
			
			// listen for <code>curl</code> changes
			dal.listen_for_changes(curl, listenerID.intValue());

			// after all registrations are done successfully
			// remember that we are monitoring <code>curl</code>
			curlMap.put(curl, conn);
		}

		/**
		 * Checks if object with the given CURL is already registered to this listener.
		 * @param curl the path for our DAO object
		 */
		public boolean isRegistered(String curl)
		{
			return curlMap.containsKey(curl);
		}

		/**
		 * Unregister this listener from DAL server(s).
		 */
		public void destroy()
		{
			DAL dal;
			Integer listenerID;
			
			Iterator iter = dalMap.keySet().iterator();
			while (iter.hasNext())
			{
				dal = (DAL) iter.next();
				listenerID = (Integer) dalMap.get(dal);
				
				// do not bail out if derefistration of one listener fails
				try
				{
					dal.remove_change_listener(listenerID.intValue());
				}
				catch (Exception ex)
				{
					// no-op.
				}
			}
			
			// clear caches
			dalMap.clear();
			curlMap.clear();
		}
	};

	/**
	 * Constructor.
	 * @param orb 	CORBA ORB.
	 * @param logger logger.
	 */
	public CDBAccess(ORB orb, Logger logger)
	{
		assert (orb != null);
		assert (logger != null);
		this.orb = orb;
		this.logger = logger;

		readConfiguration();
		changeListener = new ChangeListener();
	}

	/**
	 * Performs the connect of the specified DAO.
	 * 
	 * @param	curl	DAO curl, non-<code>null</code>
	 * @return	DAO proxy.
	 */
	public DAOProxy createDAO(String curl) 
	{
		DAOProxy proxy = new DAOProxy(curl);
		internalConnect(proxy);
		return proxy;
	}

	/**
	 * Performs the connect of the specified DAO.
	 * 
	 * @param	proxy	the proxy to connect, non-<code>null</code>
	 * @throws	RemoteException	if the connection fails
	 */
	private void internalConnect(DAOProxy proxy) 
	{

		String curl = null;			

		try
		{
			checkDALConnection();
		}
		catch (Throwable th) {
			// TODO @todo replace
			RuntimeException re = new RuntimeException("Failed to obtain DAO for proxy '" + proxy + "'.", th);
			throw re;
		}
		
		DAOOperations dao = null;
		
		try
		{

			curl = proxy.getCURL();
			
			if (remoteDAO)
			{
				dao = dalReference.get_DAO_Servant(curl);
			}
			else
			{
				String xml = dalReference.get_DAO(curl);

				SAXParserFactory factory = SAXParserFactory.newInstance();
				SAXParser saxParser = factory.newSAXParser();
				
				// use CDB XML handler which does not creates strings...
				XMLHandler xmlSolver = new XMLHandler(false);
				
				saxParser.parse(new InputSource(new StringReader(xml)), xmlSolver);
				
				if (xmlSolver.m_errorString != null){
					AcsJCDBXMLErrorEx e = new AcsJCDBXMLErrorEx();
					e.setErrorString("XML parser error: " + xmlSolver.m_errorString);
					throw e;
					//throw new XMLerror("XML parser error: " + xmlSolver.m_errorString);
				}
				// create non-CORBA related, silent DAO
				dao = new DAOImpl(curl, xmlSolver.m_rootNode, null, true);
			}
			
			// register listener, if not already registered
			if (changeListener != null)
			{
				if (!changeListener.isRegistered(curl))
					changeListener.handle(dalReference, curl, proxy);
			}
			
		} catch (Throwable th)
		{
			// TODO @todo replace
			RuntimeException re = new RuntimeException("Failed to obtain DAO object for proxy '" + proxy + "'.", th);
			throw re;
		}
			
		try
		{
			proxy.initialize(dao);
		} catch (Throwable th)
		{
			// TODO @todo replace
			RuntimeException re = new RuntimeException("The proxy '" + proxy + "' rejects the DAO.", th);
			throw re;
		}

		logger.config("Connected to DAO '" + proxy.getCURL() + "'.");

	}

	/**
	 * Checks connection status (if already connected) and connects if necessary.
	 */
	private void checkDALConnection() {
		try
		{
			
			if (dalReference == null)
			{

				logger.info("Connecting to DAL '" + defaultDAL + "'...");
		
				// connect to DAL
				dalReference = DALHelper.narrow(orb.string_to_object(defaultDAL));

				if (dalReference == null)
					// TODO @todo replace
					throw new RuntimeException("Failed to connect to the DAL object with reference, got 'null' reference.");
					
				logger.info("Connected to DAL '" + defaultDAL + "'.");

			}

		} catch (Throwable th)
		{
			logger.info("Failed to connect to DAL '" + defaultDAL + "'.");

			// TODO @todo replace
			RuntimeException re = new RuntimeException("Failed to connect to the DAL object with reference '" + defaultDAL + "'.", th);
			throw re;
		}
	}

	/**
	 * Sets the DAO of the proxy to <code>null</code>
	 * 
	 * @param	proxy	the proxy to disconnect, non-<code>null</code>
	 */
	private void internalDisconnect(DAOProxy proxy) 
	{
		assert (proxy != null);
		
		proxy.initialize(null);
	}

	/**
	 * Helper method to get all subnodes of the current proxy, removes ".xml" element from the list.
	 * @param proxy		proxy whose subnodes to return.	
	 * @return	array of subnodes.
	 * @throws	Throwable	exception on failure (e.g. connection failure, etc.)
	 */
	public String[] getSubNodes(DAOProxy proxy) throws Throwable
	{
		assert (proxy != null);

		checkDALConnection();
		
		ArrayList subnodes = new ArrayList();
	    
	    LinkedList stack = new LinkedList();
		stack.addLast(proxy.getCURL());
		while (!stack.isEmpty())
		{
		    String parentNode = stack.removeLast().toString();
		    
		    String nodes = dalReference.list_nodes(parentNode);
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
			        subnodes.add(fullName.substring(proxy.getCURL().length()+1));
			    }
			}
		}				
	    
		String[] retVal = new String[subnodes.size()];
		subnodes.toArray(retVal);
		return retVal;
	}

	/**
	 * Interprets the configuration delivered by System JVM properties.
	 */
	public void readConfiguration()
	{
		// system property overrides default configuration, DAL reference
		defaultDAL = System.getProperty(CDBDAL_DEFAULT_REFERENCE, DEFAULT_REFERENCE);

		// use remote DAO?
		remoteDAO = Boolean.valueOf(System.getProperty(CDBDAO_REMOTE, "false")).booleanValue();
	}

	/**
	 * Destroys.
	 */
	public void destroy()
	{
		if(changeListener != null)
			changeListener.destroy();
		
		// TODO @todo should DAOProxies be destroyed too?
	}
	
	/**
	 * DAL accessor.
	 * @return DAL reference.
	 */
	public DAL getDAL() 
	{
		return dalReference;
	}
	
}
