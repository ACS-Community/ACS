package alma.acs.tmcdb.compare;

import java.util.Properties;
import java.util.logging.Logger;

import junit.framework.TestCase;

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

public class ClearCacheTest extends TestCase {

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
		assertEquals(after,rdbDao.get_string_seq("Startup"));
	}
	
	public void testXmlWdalChange() throws Exception {
		System.out.println("xbefore: "+xbefore[0]+" "+xbefore[1]);
		xmlWdao.set_string_seq("Startup", after);
		System.out.println(sw.getLapTimeMillis()+" ms after writing new record");
//		assertFalse(xbefore.equals(xmlDao.get_string("Startup")));
//		assertEquals(after,xmlDao.get_string_seq("Startup")); // WDAL changes recognized on next read
//		assertEquals(after,xmlDao.get_string_seq("Startup")); // See whether second read causes the failure
		xmlDAL.clear_cache(firstNode);
		xmlDao = xmlDAL.get_DAO_Servant(firstNode); // This makes the test succeed
		System.out.println(sw.getLapTimeMillis()+" ms after clearing the cache");
//		assertEquals(after,xmlDao.get_string_seq("Startup"));		
	}

}
