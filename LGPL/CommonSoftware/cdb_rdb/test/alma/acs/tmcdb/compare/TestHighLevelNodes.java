package alma.acs.tmcdb.compare;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.logging.Logger;

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

import com.cosylab.CDB.DAO;
import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;
import com.cosylab.cdb.client.CDBAccess;

/**
 * @author jschwarz
 * Compares the output from two different DAL implementations
 * Assuming that ACS is not running on instance 0, the following two commands should be issued before
 * running this test:
 * 
 * hibernateCdbJDal -configName TEST0 -loadXMLCDB -memory  # This should also be done with Oracle every now and then
 * cdbjDAL -OAport 3013
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
	
	public void testMACI() throws Exception {
		HashSet<String> xmlNodes = new HashSet<String>(Arrays.asList(xmlDAL.list_nodes("MACI/Containers").split(" ")));
		HashSet<String> rdbNodes = new HashSet<String>(Arrays.asList(rdbDAL.list_nodes("MACI/Containers").split(" ")));
		logger.info("XML: "+xmlNodes.toString()+"; TMCDB: "+rdbNodes.toString());
		assertEquals(xmlNodes,rdbNodes);
		
		for (Iterator<String> iterator = xmlNodes.iterator(); iterator.hasNext();) {
			String xmlstring = "MACI/Containers/"+(String) iterator.next();
			DAO xmlDao = xmlDAL.get_DAO_Servant(xmlstring);
			DAO rdbDao = rdbDAL.get_DAO_Servant(xmlstring);
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
			assertEquals(xmlDao.get_string("Recovery"),rdbDao.get_string("Recovery")); // Optional Boolean, but it works!

		}
		
		xmlNodes = new HashSet<String>(Arrays.asList(xmlDAL.list_daos("MACI/Components").split(" ")));
		rdbNodes = new HashSet<String>(Arrays.asList(rdbDAL.list_daos("MACI/Components").split(" ")));
		logger.info("XML: "+xmlNodes.toString()+"; TMCDB: "+rdbNodes.toString());
		assertEquals(xmlNodes,rdbNodes);
		
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
		final String[] propertyName = {"StartOnDemand","TypeModifiers","Host","Flags","KeepAliveTime"}; //TODO: KeepAliveTime is an integer!
		for (int i = 0; i < propertyName.length; i++) {
			assertEquals(xmlDao.get_string(propertyName[i]),rdbDao.get_string(propertyName[i]));
		}
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

	protected void tearDown() throws Exception {
		super.tearDown();
	}

}
