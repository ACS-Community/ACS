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

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Properties;
import java.util.logging.Logger;

import junit.framework.TestCase;
import static org.junit.Assert.*;
//import static org.hamcrest.MatcherAssert.assertThat;
//import static org.hamcrest.Matchers.*;


import org.junit.Ignore;
import org.junit.Test;
import org.omg.CORBA.ORB;

import alma.acs.util.ACSPorts;
import alma.acs.util.StopWatch;

import com.cosylab.CDB.DAO;
import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;
import com.cosylab.CDB.WDAL;
import com.cosylab.CDB.WDALHelper;
import com.cosylab.CDB.WDAO;
import com.cosylab.cdb.client.CDBAccess;

/**
 * @author jschwarz
 * Compares the output from two different DAL implementations when a write, followed by a clear_cache and a read are done.
 * Assuming that ACS is not running on instance 0, the following two commands should be issued before
 * running this test:
 * 
 * hibernateCdbJDal -configName TEST0 -loadXMLCDB -memory  # This should also be done with Oracle every now and then
 * cdbjDAL -OAport 3013
 * For remote debugging: export JAVA_OPTIONS="$JAVA_OPTIONS -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=8080<or some other port>" 
 */

public class ClearCacheTest extends TestCase {

	private static final int BLKSIZ = 8192;

	private ORB orb;
	
	private CDBAccess rdbAccess;
	private CDBAccess xmlAccess;
	private Logger logger;
	private JDAL rdbDAL;
	private JDAL xmlDAL;
	
	private DAO rdbDao;
	private DAO xmlDao;

	private String firstNode;

	private StopWatch sw;

	private String[] before;

	private WDAL wdal;
	private WDAL xmlWdal;
	
	private WDAO wdao;
	private WDAO xmlWdao;

	private String[] xbefore;

	private String[] after;

	private static final String[] afterStartup = {"CLOCK1", "TIMER1", "MOUNT1"};

	private static final String MANAGER_XML = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"+
	"<Manager   xmlns:cdb=\"urn:schemas-cosylab-com:CDB:1.0\" "
			+ "xmlns=\"urn:schemas-cosylab-com:Manager:1.0\" "
			+ "xmlns:log=\"urn:schemas-cosylab-com:LoggingConfig:1.0\" "
			+ "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
			+ "Timeout=\"50.0\" "
			+ "ClientPingInterval=\"10.0\" "
			+ "ContainerPingInterval=\"10.0\" "
			+ "AdministratorPingInterval=\"10.0\">"
			+ "<Startup>"
			+ "      <cdb:_ string=\"CLOCK1\"/>"
			+ "      <cdb:_ string=\"TIMER1\"/>"
			+ "      <cdb:_ string=\"MOUNT2\"/>"
			+ "</Startup>"
			+ "<ServiceComponents>"
			+ 	"<cdb:_ string=\"Log\"/>"
			+ 	"<cdb:_ string=\"LogFactory\"/>"
			+ 	"<cdb:_ string=\"NotifyEventChannelFactory\"/>"
			+ 	"<cdb:_ string=\"ArchivingChannel\"/>"
			+ 	"<cdb:_ string=\"LoggingChannel\"/>"
			+ 	"<cdb:_ string=\"InterfaceRepository\"/>"
			+ 	"<cdb:_ string=\"CDB\"/>"
			+ 	"<cdb:_ string=\"ACSLogSvc\"/>"
			+ 	"<cdb:_ string=\"PDB\"/>"
			+ 	"<cdb:_ string=\"AcsAlarmService\"/>"
			+ 	"</ServiceComponents>"
			+ 	"<LoggingConfig>"
			+ 	"<log:_ Name=\"jacorb@Manager\" minLogLevel=\"5\" minLogLevelLocal=\"4\"/>"
			+ "</LoggingConfig>" + 
	"</Manager>";


	public ClearCacheTest(String name) {
		super(name);
	}

	protected void setUp() throws Exception {
		super.setUp();
		String rdbIOR = "corbaloc::" + ACSPorts.getIP() + ":" + "3012/CDB"; // ACSPorts.getCDBPort() + "/CDB";
		String xmlIOR = "corbaloc::" + ACSPorts.getIP() + ":" + "3013/CDB"; // ACSPorts.getCDBPort() + "/CDB";
		String args[] = {};
		logger = Logger.getAnonymousLogger();
		Properties props = new Properties();
		props.setProperty("org.omg.CORBA.ORBClass","org.jacorb.orb.ORB");
		props.setProperty("org.omg.CORBA.ORBSingletonClass","org.jacorb.orb.ORBSingleton");
		orb = ORB.init(args, props);
		
		rdbDAL = JDALHelper.narrow(orb.string_to_object(rdbIOR));
		rdbAccess = new CDBAccess(orb,logger);
		rdbAccess.setDAL(rdbDAL);
		
		xmlDAL = JDALHelper.narrow(orb.string_to_object(xmlIOR));
		xmlAccess = new CDBAccess(orb,logger);
		xmlAccess.setDAL(xmlDAL);
		after = new String[]{"fubar","snafu"};
		String[] rdbNodes = rdbDAL.list_nodes("MACI/Managers").split(" ");
		if (rdbNodes.length == 0)
			fail("No Manager nodes present.");
		firstNode = "MACI/Managers/"+rdbNodes[0];
		rdbDao = rdbDAL.get_DAO_Servant(firstNode);
		xmlDao = xmlDAL.get_DAO_Servant(firstNode);
		sw = new StopWatch(logger);
		before = rdbDao.get_string_seq("Startup");
		xbefore = xmlDao.get_string_seq("Startup");
		
		wdal = WDALHelper.narrow(rdbDAL);
		wdao = wdal.get_WDAO_Servant(firstNode);
		xmlWdal = WDALHelper.narrow(xmlDAL);
		xmlWdao = xmlWdal.get_WDAO_Servant(firstNode);
	}

	protected void tearDown() throws Exception {
		wdao.set_string_seq("Startup", before);
		rdbDAL.clear_cache(firstNode);
//		xmlWdao.set_string("Startup", xbefore);
//		xmlDAL.clear_cache(firstNode);
		super.tearDown();
	}
	
	public void testRdbWdalChange() throws Exception {
		wdao.set_string_seq("Startup", after);
		System.out.println(sw.getLapTimeMillis()+" ms after writing new record");
		assertFalse(before.equals(rdbDao.get_string("Startup")));
//		assertEquals(after,rdbDao.get_string_seq("Startup")); // WDAL changes recognized on next read
//		assertEquals(after,rdbDao.get_string_seq("Startup")); // See whether second read causes the failure
		rdbDAL.clear_cache(firstNode);
//		rdbDao = rdbDAL.get_DAO_Servant(firstNode); // This makes the test succeed
		System.out.println(sw.getLapTimeMillis()+" ms after clearing the cache");
		assertArrayEquals(after,rdbDao.get_string_seq("Startup"));
	}
	
	public void testXmlWdalChange1() throws Exception {
		System.out.println("firstNode: "+firstNode);
		Reader is = new FileReader("Manager.xml");
		String managerXmlFromFile = readerToString(is);
		System.out.println(managerXmlFromFile);
		xmlWdal.set_DAO(firstNode, managerXmlFromFile);
//		xmlWdao.set_string_seq("Startup", after);
//		assertFalse(xbefore.equals(xmlDao.get_string("Startup")));
//		assertEquals(after,xmlDao.get_string_seq("Startup")); // WDAL changes recognized on next read
//		assertEquals(after,xmlDao.get_string_seq("Startup")); // See whether second read causes the failure
		xmlDAL.clear_cache(firstNode);
		xmlDao = xmlDAL.get_DAO_Servant(firstNode); // This makes the test succeed
		assertArrayEquals(afterStartup ,xmlDao.get_string_seq("Startup"));		
	}
	
	public void testXmlWdalChange2() throws Exception {
		System.out.println("firstNode: "+firstNode);
		xmlWdal.set_DAO(firstNode, MANAGER_XML);
		//		xmlWdao.set_string_seq("Startup", after);
		//		assertFalse(xbefore.equals(xmlDao.get_string("Startup")));
		//		assertEquals(after,xmlDao.get_string_seq("Startup")); // WDAL changes recognized on next read
		//		assertEquals(after,xmlDao.get_string_seq("Startup")); // See whether second read causes the failure
		xmlDAL.clear_cache(firstNode);
		xmlDao = xmlDAL.get_DAO_Servant(firstNode); // This makes the test succeed
		assertArrayEquals(afterStartup,xmlDao.get_string_seq("Startup"));		
	}


	private String readerToString(Reader is) throws IOException {
		StringBuffer sb = new StringBuffer();
		char[] b = new char[BLKSIZ];
		int n;
		while ((n = is.read(b)) > 0) {
			sb.append(b, 0, n);
		}
		return sb.toString();
	}

}
