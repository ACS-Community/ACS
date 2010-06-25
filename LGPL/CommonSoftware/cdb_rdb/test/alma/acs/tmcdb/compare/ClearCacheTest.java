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
	
	private final CDBAccess rdbAccess;
	private final Logger logger;
	private final JDAL rdbDAL;
	
	private DAO rdbDao;

	private String firstNode;

	private StopWatch sw;

	private String before;

	private WDAL wdal;

	private WDAO wdao;

	public ClearCacheTest(String name) {
		super(name);
		String rdbIOR = "corbaloc::" + ACSPorts.getIP() + ":" + "3012/CDB"; // ACSPorts.getCDBPort() + "/CDB";
		String args[] = {};
		logger = Logger.getAnonymousLogger();
		Properties props = new Properties();
		props.setProperty("org.omg.CORBA.ORBClass","org.jacorb.orb.ORB");
		props.setProperty("org.omg.CORBA.ORBSingletonClass","org.jacorb.orb.ORBSingleton");
		orb = ORB.init(args, props);

		rdbDAL = JDALHelper.narrow(orb.string_to_object(rdbIOR));
		rdbAccess = new CDBAccess(orb,logger);
		rdbAccess.setDAL(rdbDAL);
	}

	protected void setUp() throws Exception {
		super.setUp();
		String[] rdbNodes = rdbDAL.list_nodes("MACI/Managers").split(" ");
		if (rdbNodes.length == 0)
			fail("No Manager nodes present.");
		firstNode = "MACI/Managers/"+rdbNodes[0];
		rdbDao = rdbDAL.get_DAO_Servant(firstNode);
		sw = new StopWatch(logger);
		before = rdbDao.get_string("Startup");
		wdal = WDALHelper.narrow(rdbDAL);
		wdao = wdal.get_WDAO_Servant(firstNode);
	}

	protected void tearDown() throws Exception {
		wdao.set_string("Startup", before);
		rdbDAL.clear_cache(firstNode);
		super.tearDown();
	}
	
	public void testWdalChange() throws Exception {
		System.out.println(sw.getLapTimeMillis()+" ms after writing new record");
		String after = "fubar";
		wdao.set_string("Startup", after);
		assertFalse(before.equals(rdbDao.get_string("Startup")));
		assertEquals(after,rdbDao.get_string("Startup")); // WDAL changes recognized on next read
		rdbDAL.clear_cache(firstNode);
		System.out.println(sw.getLapTimeMillis()+" ms after clearing the cache");
		assertEquals(after,rdbDao.get_string("Startup"));
	}

}
