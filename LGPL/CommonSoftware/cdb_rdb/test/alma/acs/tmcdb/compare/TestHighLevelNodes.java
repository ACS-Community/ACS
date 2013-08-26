/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
package alma.acs.tmcdb.compare;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.collections.CollectionUtils;
import org.custommonkey.xmlunit.XMLTestCase;
import org.jdom.Document;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import org.omg.CORBA.ORB;

import alma.acs.util.ACSPorts;
import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.WrongCDBDataTypeEx;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DAO;
import com.cosylab.CDB.DAOOperations;
import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;
import com.cosylab.cdb.client.CDBAccess;
import com.cosylab.cdb.client.DAOProxy;

/**
 * @author jschwarz
 * Compares the output from two different DAL implementations
 * Assuming that ACS is not running on instance 0, the following two commands should be issued before
 * running this test:
 * 
 * hibernateCdbJDal -configName TEST0 -loadXMLCDB -memory  # This should also be done with Oracle every now and then
 * cdbjDAL -OAport 3013
 * For remote debugging: export JAVA_OPTIONS="$JAVA_OPTIONS -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=8080<or some other port>" 
 */
public class TestHighLevelNodes extends XMLTestCase {
	
	private ORB orb;
	
	private final CDBAccess xmlAccess;
	private final CDBAccess rdbAccess;
	private final Logger logger;
	private final JDAL xmlDAL;
	private final JDAL rdbDAL;
	private String xmlXml;
	private String rdbXml;

	public TestHighLevelNodes(String name) {
		super(name);
		String rdbIOR = "corbaloc::" + ACSPorts.getIP() + ":" + "3012/CDB"; // ACSPorts.getCDBPort() + "/CDB";
		String xmlIOR = "corbaloc::" + ACSPorts.getIP() + ":" + "3013/CDB"; // ACSPorts.getCDBPort() + "/CDB";
		String args[] = {};
		logger = Logger.getAnonymousLogger();
		Properties props = new Properties();
		props.setProperty("org.omg.CORBA.ORBClass","org.jacorb.orb.ORB");
		props.setProperty("org.omg.CORBA.ORBSingletonClass","org.jacorb.orb.ORBSingleton");
		orb = ORB.init(args, props);
		xmlDAL = JDALHelper.narrow(orb.string_to_object(xmlIOR));
		xmlAccess = new CDBAccess(orb,logger);
		xmlAccess.setDAL(xmlDAL);
		rdbDAL = JDALHelper.narrow(orb.string_to_object(rdbIOR));
		rdbAccess = new CDBAccess(orb,logger);
		rdbAccess.setDAL(rdbDAL);
	}

	protected void setUp() throws Exception {
		super.setUp();
		xmlXml = xmlDAL.get_DAO("MACI/");
		rdbXml = rdbDAL.get_DAO("MACI/");
	}
	
	public void testMACI_Managers() throws Exception {
		HashSet<String> xmlNodes = new HashSet<String>(Arrays.asList(xmlDAL.list_nodes("MACI/Managers").split(" ")));
		HashSet<String> rdbNodes = new HashSet<String>(Arrays.asList(rdbDAL.list_nodes("MACI/Managers").split(" ")));
		logger.info("XML: "+xmlNodes.toString()+"; TMCDB: "+rdbNodes.toString());
		assertEquals(xmlNodes,rdbNodes);
		for (Iterator<String> iterator = xmlNodes.iterator(); iterator.hasNext();) {
			String xmlstring = "MACI/Managers/"+(String) iterator.next();
			DAO xmlDao = xmlDAL.get_DAO_Servant(xmlstring);
			DAO rdbDao = rdbDAL.get_DAO_Servant(xmlstring);
			examineLoggingConfig(xmlDao,rdbDao);
			assertEquals(xmlDao.get_string("Startup"),rdbDao.get_string("Startup"));
			assertEquals(xmlDao.get_string("ServiceComponents"),rdbDao.get_string("ServiceComponents"));
			String sx = null;
			boolean xbool = true;
			try {
				sx = xmlDao.get_string("ServiceDaemons");
			} catch (CDBFieldDoesNotExistEx e) {
				xbool = false;
			}
			String sr = null;
			try {
				sr = rdbDao.get_string("ServiceDaemons");
			} catch (CDBFieldDoesNotExistEx e) {
				if (xbool) fail("Service Daemons: XML CDB has value: "+sx+" but TMCDB can't find the field.");
				continue; // Neither CDB can find it; move to next property
			}
			if (!xbool) fail("Service Daemons: TMCDB has value: "+sr+" but XML CDB can't find the field.");
			assertEquals(sx,sr); // TODO: Redo this once Matej's implementation is complete
		}
	}
	
	public void testMACI_Channels() throws Exception {
		String[] xmlNodes = xmlDAL.list_nodes("MACI/Channels").split(" ");
		String[] rdbNodes = rdbDAL.list_nodes("MACI/Channels").split(" ");
		System.out.println("list_nodes gives for XML: "+xmlNodes.length+"; and for RDB: "+rdbNodes.length);
		// TODO: Fix this
		//assertEquals(xmlNodes.length,rdbNodes.length);
		String[] xmlDaos = xmlDAL.list_daos("MACI/Channels").split(" ");
		String[] rdbDaos = rdbDAL.list_daos("MACI/Channels").split(" ");
		System.out.println("list_daos gives for XML: "+xmlDaos.length+"; and for RDB: "+rdbDaos.length);
		System.out.println("\n******** list_daos output ********");
		System.out.println("**** XML CDB ****");
		for (int i = 0; i < xmlDaos.length; i++) {
			System.out.println(xmlDaos[i]);
		}
		System.out.println("\n**** RDB CDB ****");

		for (int i = 0; i < rdbDaos.length; i++) {
			System.out.println(rdbDaos[i]);
		}

		System.out.println("\n\n******** list_nodes output ********");
		System.out.println("**** XML CDB ****");
		for (int i = 0; i < xmlNodes.length; i++) {
			System.out.println(xmlNodes[i]);
		}
		System.out.println("\n**** RDB CDB ****");
		for (int i = 0; i < rdbNodes.length; i++) {
			System.out.println(rdbNodes[i]);
		}

		//assertEquals(xmlDaos.length,rdbDaos.length);
	}
	
	public void testMACI_Containers() throws Exception {
		String[] xmlNodes = getSubNodes(xmlDAL, "MACI/Containers");
		String[] rdbNodes = getSubNodes(rdbDAL, "MACI/Containers");
		logger.info("XML: "+xmlNodes.toString()+"; TMCDB: "+rdbNodes.toString());
//		assertEquals(xmlNodes,rdbNodes);

		for (int i = 0; i < xmlNodes.length; i++) {
			String xmlstring = "MACI/Containers/"+xmlNodes[i];
			System.out.println(xmlstring);
			DAO xmlDao = xmlDAL.get_DAO_Servant(xmlstring);
			DAO rdbDao = rdbDAL.get_DAO_Servant(xmlstring);
			// check if real config, otherwice skip
			if (readLong(xmlDao, "LoggingConfig/minLogLevel", -1) < 0)
				continue;
			assertEquals(xmlDao.get_string("Autoload"),rdbDao.get_string("Autoload"));
			examineDeployInfo(xmlstring);
			examineLoggingConfig(xmlDao,rdbDao);

			assertEquals(xmlDao.get_string("ImplLang"),rdbDao.get_string("ImplLang"));
			assertEquals(xmlDao.get_double("Timeout"),rdbDao.get_double("Timeout"));
			checkEqualsBoolean("UseIFR", xmlDao, rdbDao);
			assertEquals(xmlDao.get_long("ManagerRetry"),rdbDao.get_long("ManagerRetry"));
			String str = "PingInterval";
			checkLongOptionalNoDefault(str, xmlDao, rdbDao);
//			assertEquals(xmlDao.get_string("DALtype"),rdbDao.get_string("DALtype")); // Get rid of this attribute in XML CDB??
			assertEquals(xmlDao.get_long("ServerThreads"),rdbDao.get_long("ServerThreads"));
			checkEqualsBoolean("Recovery",xmlDao, rdbDao); // Optional Boolean, but it works!

		}
	}
	public void testMACI_Components() throws Exception {
		String[] xmlAllNodes = retrieveComponentsList(xmlAccess);
		String[] rdbAllNodes = retrieveComponentsList(rdbAccess);
		assertEquals(xmlAllNodes.length,rdbAllNodes.length);
		HashSet<String> xmlNodes = new HashSet<String>(Arrays.asList(xmlAllNodes));
		HashSet<String> rdbNodes = new HashSet<String>(Arrays.asList(rdbAllNodes));
		logger.info("XML: "+xmlNodes.toString()+";\n TMCDB: "+rdbNodes.toString());

		assertTrue(xmlNodes.equals(rdbNodes));
		assertTrue(CollectionUtils.isEqualCollection(xmlNodes, rdbNodes));
		
		for (Iterator<String> iterator = xmlNodes.iterator(); iterator.hasNext();) {
			String xmlstring = "MACI/Components/"+(String) iterator.next();
			DAO xmlDao = xmlDAL.get_DAO_Servant(xmlstring);
			DAO rdbDao = rdbDAL.get_DAO_Servant(xmlstring);
			assertEquals(xmlDao.get_string("Code"),rdbDao.get_string("Code"));
			assertEquals(xmlDao.get_string("Type"),rdbDao.get_string("Type"));
			assertEquals(xmlDao.get_string("Container"),rdbDao.get_string("Container"));
			assertEquals(xmlDao.get_string("Default"),rdbDao.get_string("Default"));
			assertEquals(xmlDao.get_string("ImplLang"),rdbDao.get_string("ImplLang"));
		}
		

	}

	private void checkEqualsBoolean(String propName, DAO xmlDao, DAO rdbDao)
			throws CDBFieldDoesNotExistEx, WrongCDBDataTypeEx {
		String xs = xmlDao.get_string(propName);
		String rs = rdbDao.get_string(propName);
		Boolean xbool = Boolean.parseBoolean(xs);
		Boolean rbool = Boolean.parseBoolean(rs);
		try {
			Integer ix = Integer.parseInt(xs);
			xbool = ix == 1 ? true : false;
		} catch (NumberFormatException e) {
		}
		try {
			Integer ir = Integer.parseInt(rs);
			rbool = ir == 1 ? true : false;
		} catch (NumberFormatException e) {
		}
//		System.out.println("xs = "+xbool+"; rs = "+rbool);
		assertEquals(xbool,rbool); // Optional Boolean: XML returns "1", TMCDB returns "true"
	}

	private void checkLongOptionalNoDefault(String str, DAO xmlDao, DAO rdbDao)
			throws Exception {
		boolean noRecord = false;
		int xmlInt = 0;
		int rdbInt = 0;
		try {
			xmlInt = xmlDao.get_long(str);
		} catch (CDBFieldDoesNotExistEx e) {
			noRecord = true;
		}
		try {
			rdbInt = rdbDao.get_long(str);
			if (noRecord) fail("Property "+str+" found in TMCDB but not in XML CDB.");
		} catch (CDBFieldDoesNotExistEx e) {
			if (!noRecord) fail("Property "+str+" found in XML CDB but not in TMCDB.");
		}
		if (noRecord)
			return;
		assertEquals(xmlInt,rdbInt);
	}

	private void examineDeployInfo(String xmlstring) throws Exception {
		boolean noRecordXml = false;
		DAO xmlDao = null;
		DAO rdbDao = null;
		try {
			xmlDao = xmlDAL.get_DAO_Servant(xmlstring+"/DeployInfo");
		} catch (CDBRecordDoesNotExistEx ex) {
			noRecordXml = true;
		}
		try {
			rdbDao = rdbDAL.get_DAO_Servant(xmlstring+"/DeployInfo");
			if (noRecordXml) fail("DeployInfo found for TMCDB in "+xmlstring+" but not in  XML CDB");
		} catch (CDBRecordDoesNotExistEx ex) {
			if (!noRecordXml) fail("DeployInfo found for XML CDB in "+xmlstring+" but not in TMCDB");
		}
		if (noRecordXml) return;
		final String[] propertyName = {"TypeModifiers","Host","Flags","KeepAliveTime"}; //TODO: KeepAliveTime is an integer!
		for (int i = 0; i < propertyName.length; i++) {
			System.out.println(propertyName[i]);
			String sx = null;
			boolean xbool = true;
			try {
				sx = xmlDao.get_string(propertyName[i]);
			} catch (CDBFieldDoesNotExistEx e) {
				xbool = false;
			}
			String sr = null;
			try {
				sr = rdbDao.get_string(propertyName[i]);
			} catch (CDBFieldDoesNotExistEx e) {
				if (xbool) fail("XML CDB has value: "+sx+" but TMCDB can't find the field.");
				continue; // Neither CDB can find it; move to next property
			}
			if (!xbool) fail("TMCDB has value: "+sr+" but XML CDB can't find the field.");
			assertEquals(sx,sr);
		}
		checkEqualsBoolean("StartOnDemand", xmlDao, rdbDao);
	}
	
	private void examineLoggingConfig(DAO xmlDao, DAO rdbDao) throws Exception {
		assertEquals(xmlDao.get_string("LoggingConfig/centralizedLogger"),rdbDao.get_string("LoggingConfig/centralizedLogger"));
		assertEquals(xmlDao.get_long("LoggingConfig/dispatchPacketSize"),rdbDao.get_long("LoggingConfig/dispatchPacketSize"));
		assertEquals(xmlDao.get_long("LoggingConfig/immediateDispatchLevel"),rdbDao.get_long("LoggingConfig/immediateDispatchLevel"));
		assertEquals(xmlDao.get_long("LoggingConfig/flushPeriodSeconds"),rdbDao.get_long("LoggingConfig/flushPeriodSeconds"));
		assertEquals(xmlDao.get_long("LoggingConfig/maxLogQueueSize"),rdbDao.get_long("LoggingConfig/maxLogQueueSize"));
		assertEquals(xmlDao.get_long("LoggingConfig/maxLogsPerSecond"),rdbDao.get_long("LoggingConfig/maxLogsPerSecond"));
		//TODO: Handle NamedLogger, UnnamedLogger

	}
	
	public void testXmlMACI() throws Exception {
		logger.info("MACI XML string -- Classic: "+xmlXml);
		SAXBuilder sb = new SAXBuilder();

		InputStream is = new ByteArrayInputStream(rdbXml.getBytes("UTF-8"));
		Document doc = sb.build(is);
		XMLOutputter xout = new XMLOutputter(Format.getPrettyFormat());
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		xout.output(doc, out);
		logger.info("MACI XML string -- RDB: "+out.toString());
		assertXMLEqual("MACI XML pieces are similar ",xmlXml ,rdbXml); // This fails at the moment because XML returns namespace info, TMCDB doesn't
	}
	
	private String[] getSubNodes(DAL dal, String subnode) throws Exception
	{
		ArrayList<String> subnodes = new ArrayList<String>();
	    
	    LinkedList<String> stack = new LinkedList<String>();
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
	
	/**
	 * Searches dao for all potential (nodes containing Name attribute) ComponentInfo nodes.
	 * @param dc	dao to be searched.
	 * @returns list of all potential ComponentInfo nodes.
	 */
	private String[] retrieveComponentsList(CDBAccess cdbAccess)
    {
        ArrayList<String> componentList = new ArrayList<String>();

        try
        {
        	DAOProxy dc = cdbAccess.createDAO("MACI/Components");
            LinkedHashSet<String> nodes = new LinkedHashSet<String>();
            // current
            nodes.add("");
            String[] subnodes = cdbAccess.getSubNodes(dc);
            if (subnodes != null)
                for (int i = 0; i < subnodes.length; i++)
                    nodes.add(subnodes[i]);

            Iterator<String> iter = nodes.iterator();
            while (iter.hasNext())
            {
                String prefix = iter.next().toString();
                if (prefix.length() > 0)
                    prefix += "/";
                String attributes = dc.get_field_data(prefix + "_characteristics");

                // convert into array
                StringTokenizer tokenizer = new StringTokenizer(attributes, ",");
                while (tokenizer.hasMoreTokens())
                {
                    String subname = tokenizer.nextToken().toString();
                    String componentName = prefix + subname;

                    // check if potentially valid ComponentInfo entry - read name
                    /// @todo this could be done better (to check if all attributes exist)
                    if (readStringCharacteristics(dc, componentName + "/Name", true) != null)
                        componentList.add(componentName);
                }
            }

        } catch (Throwable th)
        {
            Exception ce = new Exception("Failed to obtain list of all components.", th);
			logger.log(Level.WARNING, ce.getMessage(), ce);
        }

        String[] retVal = new String[componentList.size()];
        componentList.toArray(retVal);

        //logger.log(Level.INFO,"Found " + retVal.length + " component entries in the configuration database.");

        return retVal;
    }

	/**
	 * Reads DAO (CDB access) of string type.
	 *
	 * @param	path		path to be read non-<code>null</code>.
	 * @param	dao			DAO on which to perform read request.
	 * @param	silent		do not complain, if characteristics not found
	 * @return	String		value read, <code>null</code> on failure
	 * @throws Exception 
	 */
	private String readStringCharacteristics(DAOProxy dao, String path, boolean silent) throws Exception
	{
		assert (path != null);

		String retVal = null;

		try
		{
			retVal = dao.get_string(path);
		}
		catch (Throwable th)
		{
			Exception ce = new Exception("Failed to read '"+path+"' field on DAO dao '"+dao+"'.", th);
			if (!silent)
				throw ce;
		}

		return retVal;

	}
	
	protected void tearDown() throws Exception {
		super.tearDown();
	}

}
