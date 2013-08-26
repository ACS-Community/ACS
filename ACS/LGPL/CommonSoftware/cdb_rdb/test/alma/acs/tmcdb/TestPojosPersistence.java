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
package alma.acs.tmcdb;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import junit.framework.TestCase;

import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.QueryException;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.criterion.Restrictions;
import org.hibernate.jdbc.Work;

import alma.acs.logging.ClientLogManager;
import alma.acs.logging.level.AcsLogLevelDefinition;

import com.cosylab.cdb.jdal.hibernate.HibernateUtil;
import com.cosylab.cdb.jdal.hibernate.HibernateUtil.HibernateUtilException;
import com.cosylab.cdb.jdal.logging.AcsLoggerHelper;

public class TestPojosPersistence extends TestCase {

	private final String CREATE_TMCDB_SWCORE = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigcore/CreateHsqldbTables.sql";
	private final String CREATE_TMCDB_SWEXT  = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigext/CreateHsqldbTables.sql";
	private final String DROP_TMCDB_SWCORE = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigcore/DropAllTables.sql";
	private final String DROP_TMCDB_SWEXT  = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigext/DropAllTables.sql";

    private final static int DEFAULT_BUF_LEN = 64 * 1024;

    private final String  COMPUTER_NAME = "eva";
    private final String  COMPUTER_NETNAME = "eva.eso.org";
    private final String  COMPUTER_LOCATION = "A033";
    private final boolean COMPUTER_RT = true;
    private final boolean COMPUTER_DISKLESS = false;

	private Logger logger;
	private HibernateUtil hibernateUtil;
//	private SessionFactory _sessionFactory;

	protected void setUp() throws Exception {
		System.out.println("\n---------------- " + getName() + " ---------------\n");
		String loggerName = getName(); // name of junit test
		AcsLoggerHelper acsLoggerHelper = AcsLoggerHelper.getInstance();
		acsLoggerHelper.setLoggerName(loggerName);
		acsLoggerHelper.setDefaultLogLevels(AcsLogLevelDefinition.INFO, AcsLogLevelDefinition.INFO);
		logger = AcsLoggerHelper.getInstance().getSharedLogger();
		logger.info("Example test log");
		
		acsLoggerHelper.setHibernateLogLevels(AcsLogLevelDefinition.INFO, AcsLogLevelDefinition.TRACE);
		acsLoggerHelper.setHibernateSqlLogLevels(AcsLogLevelDefinition.TRACE, AcsLogLevelDefinition.TRACE);

//		ACSLoggerFactory hibernateLoggerFactory = ((ACSLoggerFactory)StaticLoggerBinder.getSingleton().getLoggerFactory());
//		hibernateLoggerFactory.getLogger("blabla").info("Greetings from your hibernate logger");
		
		hibernateUtil = HibernateUtil.getInstance(logger);
		hibernateUtil.setConfiguration(new AnnotationConfiguration().configure("test-hibernate.cfg.xml"));
		hibernateUtil.getSessionFactory().openSession();
	}

	protected void tearDown() throws Exception {
		// null the various static logger references; otherwise junit will not create new loggers in subsequent setUp
		HibernateUtil.clearInstance();
		AcsLoggerHelper.getInstance().shutdown(); 
		ClientLogManager.getAcsLogManager().shutdown(true);
	}
	
	public void testCreateDB() throws Exception {
		createDB();
	}

	public void testDrop() throws Exception {
		dropDB();
	}

	public void testSimpleSave() throws Exception {

		createDB();

		try {
			ComponentType componentType = new ComponentType();
			componentType.setIDL("IDL:alma/samp/Samp.idl:1.0");

			// The fact that this first simple save works
			// implies that ID generation for HSQLDB and Oracle
			// is working fine
			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(componentType);
			hibernateUtil.commitTransaction();

			TMCDBVersion tmcdbVersion = new TMCDBVersion();
			tmcdbVersion.setDBDate("12.01.10");
			tmcdbVersion.setDBName("TMCDB");
			tmcdbVersion.setDBVersion("1.2.3");
	
			try{
				hibernateUtil.beginTransaction();
				hibernateUtil.getSession().save(tmcdbVersion);
				hibernateUtil.commitTransaction();
				fail("Duplicated PK allowed!"); // Should fail, TMCDB already contains a "TMCDB" record
			} catch(HibernateUtilException e) {
				hibernateUtil.rollbackTransaction();
			}

			Configuration config = new Configuration();
			config.setActive(true);
			config.setConfigurationName("rtobarConfig");
			config.setCreationTime(new Date());
			config.setFullName("Testing configuration");
			config.setDescription("Configuration used for testing purposes");

			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(config);
			hibernateUtil.commitTransaction();

			NetworkDevice networkDevice = new NetworkDevice();
			networkDevice.setName(COMPUTER_NAME);
			networkDevice.setNetworkName(COMPUTER_NETNAME);
			networkDevice.setPhysicalLocation(COMPUTER_LOCATION);
			networkDevice.setConfiguration(null);

			try{
				hibernateUtil.beginTransaction();
				hibernateUtil.getSession().save(networkDevice);
				hibernateUtil.commitTransaction();
				fail("networkDevice's configuration is null, FK is beign violated and shouldn't be saved");
			} catch(Exception e) {
				hibernateUtil.closeSession();
			}

			networkDevice = new NetworkDevice();
			networkDevice.setName(COMPUTER_NAME);
			networkDevice.setNetworkName(COMPUTER_NETNAME);
			networkDevice.setPhysicalLocation(COMPUTER_LOCATION);
			networkDevice.setConfiguration(config);

			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(networkDevice);
			hibernateUtil.commitTransaction();
			
			// Testing loggingConfig, which only states GENERATED in the model
			LoggingConfig loggingConfig = new LoggingConfig();
			loggingConfig.setMinLogLevelDefault((byte)0x01);
			loggingConfig.setMinLogLevelLocalDefault((byte)0x01);
			loggingConfig.setCentralizedLogger("");
			loggingConfig.setDispatchPacketSize((byte)0x01);
			loggingConfig.setImmediateDispatchLevel((byte)0x01);
			loggingConfig.setFlushPeriodSeconds((byte)0x01);
			loggingConfig.setMaxLogQueueSize(0);
	
			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(loggingConfig);
			hibernateUtil.commitTransaction();
		}
		finally {
			dropDB();
		}
	}

	@SuppressWarnings("unchecked")
	public void testXmlTypeSave() throws Exception {

		createDB();

		try {
			ComponentType componentType = new ComponentType();
			componentType.setIDL("IDL:alma/xmltype/XML.idl:1.0");
			
			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(componentType);
			hibernateUtil.commitTransaction();

			Configuration config = new Configuration();
			config.setActive(true);
			config.setConfigurationName("rtobarConfig");
			config.setCreationTime(new Date());
			config.setFullName("Testing configuration");
			config.setDescription("Configuration used for testing purposes");

			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(config);
			hibernateUtil.commitTransaction();

			Component comp;
			String url = hibernateUtil.getConfiguration().getProperty("hibernate.connection.url");
			if( url.contains("oracle") ) {
				comp = createTrascientComponent(componentType, config);

				try {
					hibernateUtil.beginTransaction();
					hibernateUtil.getSession().save(comp);
					hibernateUtil.commitTransaction();
					fail("XMLDoc is not really XML, should fail with Oracle"); // No valid XML, should fail with Oracle
				} catch (Exception e) {
					hibernateUtil.closeSession();
				}

				comp = createTrascientComponent(componentType, config);
				comp.setComponentName("component3");
				comp.setXMLDoc("");

				try {
				hibernateUtil.beginTransaction();
				hibernateUtil.getSession().save(comp);
				hibernateUtil.commitTransaction();
				fail("Emtpy string is not valid XML");
				} catch(Exception e){
					hibernateUtil.closeSession();
				}
			}

			comp = createTrascientComponent(componentType, config);
			comp.setXMLDoc("<?xml version='1.0' encoding='ISO-8859-1'?>\n<CorrCanMngr xmlns=\"urn:schemas-cosylab-com:CorrCanMngr:1.0\" xmlns:baci=\"urn:schemas-cosylab-com:BACI:1.0\" xmlns:cdb=\"urn:schemas-cosylab-com:CDB:1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" id=\"0\" resetWait=\"30\"></CorrCanMngr>");

			hibernateUtil.beginTransaction();
			try {
			hibernateUtil.getSession().save(comp);
			} catch(Exception e) {
				e.printStackTrace();
			}
			hibernateUtil.commitTransaction();

			comp = createTrascientComponent(componentType, config);
			comp.setComponentName("component2");
			comp.setXMLDoc(null);

			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(comp); // Should not fail with null XMLDoc
			hibernateUtil.commitTransaction();

			for(Component component: (List<Component>)hibernateUtil.getList(Component.class))
				component.getXMLDoc();

		} finally {
			dropDB();
		}
	}

	@SuppressWarnings("unchecked")
	public void testSaveInheritance() throws Exception {

		createDB();

		try {

			createConfigurationComputerAndTwoNetworkDevices();
			// Now we get all NetworkDevice's from DB (we should get two)
			List<NetworkDevice> nDevices = hibernateUtil.getList(NetworkDevice.class);

			assertEquals(3, nDevices.size()); // Found the 2 objects :)

			boolean found = false;
			for (Iterator<NetworkDevice> iterator = nDevices.iterator(); iterator.hasNext();) {
				NetworkDevice nd = (NetworkDevice)iterator.next();
				if( nd instanceof Computer ) {
					Computer comp = (Computer)nd;
					assertEquals( comp.getName(), COMPUTER_NAME);
					assertEquals( comp.getRealTime().booleanValue(), COMPUTER_RT);
					assertEquals( comp.getNetworkName(), COMPUTER_NETNAME);
					assertEquals( comp.getProcessorType(), ComputerProcessorType.SMP);
					assertEquals( comp.getPhysicalLocation(), COMPUTER_LOCATION);
					assertEquals( comp.getDiskless().booleanValue(), COMPUTER_DISKLESS);
					found = true;
					break;
				}
			}
			assertTrue(found);

		} finally {
			dropDB();
		}
	
	}

	private void createConfigurationComputerAndTwoNetworkDevices() throws Exception  {
		
		Configuration config = new Configuration();
		config.setActive(true);
		config.setConfigurationName("rtobarConfig");
		config.setCreationTime(new Date());
		config.setFullName("Testing configuration");
		config.setDescription("Configuration used for testing purposes");

		Computer computer = new Computer();
		computer.setName(COMPUTER_NAME);
		computer.setRealTime(COMPUTER_RT);
		computer.setNetworkName(COMPUTER_NETNAME);
		computer.setProcessorType(ComputerProcessorType.SMP);
		computer.setPhysicalLocation(COMPUTER_LOCATION);
		computer.setDiskless(COMPUTER_DISKLESS);
		computer.setConfiguration(config);

		hibernateUtil.beginTransaction();
		hibernateUtil.getSession().save(config);
		hibernateUtil.getSession().save(computer);
		hibernateUtil.commitTransaction();

		NetworkDevice networkDevice = new NetworkDevice();
		networkDevice.setName("wall-e");
		networkDevice.setNetworkName("wall-e.eso.org");
		networkDevice.setPhysicalLocation("A033-2");
		networkDevice.setConfiguration(config);

		hibernateUtil.beginTransaction();
		hibernateUtil.getSession().save(networkDevice);
		hibernateUtil.commitTransaction();

		networkDevice = new NetworkDevice();
		networkDevice.setName("wall-e");
		networkDevice.setNetworkName("wall-e.alma.cl");
		networkDevice.setPhysicalLocation("A033-2");
		networkDevice.setConfiguration(config);

		hibernateUtil.beginTransaction();
		hibernateUtil.getSession().save(networkDevice);
		hibernateUtil.commitTransaction();
	}

	public void testCriteriaAPI() throws Exception {

		createDB();

		try {
			createConfigurationComputerAndTwoNetworkDevices();

			Configuration config = (Configuration)hibernateUtil.getList(Configuration.class).iterator().next();
			assertNotNull(config);

			// Now we test that using the criteria API we can find our objects
			Criteria c = hibernateUtil.getSession().createCriteria(NetworkDevice.class);
			c.add( Restrictions.eq("name", "wall-e") );
			assertEquals(2, c.list().size());

			c = hibernateUtil.getSession().createCriteria(NetworkDevice.class);
			c.add( Restrictions.eq("name", "wall-e") );
			c.add( Restrictions.eq("networkName", "wall-e.eso.org") );
			assertEquals(1, c.list().size());

			c = hibernateUtil.getSession().createCriteria(NetworkDevice.class);
			c.add( Restrictions.eq("configuration", config) );
			assertEquals(3, c.list().size());

			c = hibernateUtil.getSession().createCriteria(Configuration.class);
			c.add( Restrictions.eq("configurationName", "rtobarConfig"));
			c.add( Restrictions.lt("creationTime", new Date()));
			assertEquals(1, c.list().size());

			try {
				c = hibernateUtil.getSession().createCriteria(Configuration.class);
				c.add( Restrictions.eq("configuratioName", "rtobarConfig")); // typo: should be configurationName
				c.list();
				fail("Should fail, property 'configuratioName' doesn't exist for Configuration objects");
			} catch(QueryException e) {
			}

		} finally {
			dropDB();
		}
	
	}

	public void testHQL() throws Exception {

		createDB();

		try {
			createConfigurationComputerAndTwoNetworkDevices();

			Configuration config = (Configuration)hibernateUtil.getList(Configuration.class).iterator().next();
			assertNotNull(config);

			// Now we test that using HQL queries
			Query q = hibernateUtil.getSession().createQuery("from NetworkDevice as nd where nd.name = ?");
			q.setParameter(0, "wall-e");
			assertEquals(2, q.list().size());

			q = hibernateUtil.getSession().createQuery("from NetworkDevice as nd where nd.name = ? and nd.networkName = ?");
			q.setParameter( 0, "wall-e");
			q.setParameter(1, "wall-e.eso.org" );
			assertEquals(1, q.list().size());

			q = hibernateUtil.getSession().createQuery("from NetworkDevice as nd where nd.configuration = ?");
			q.setParameter( 0, config );
			assertEquals(3, q.list().size());

			q = hibernateUtil.getSession().createQuery("from Configuration as conf where conf.configurationName = ? and creationTime < ?");
			q.setParameter(0, "rtobarConfig");
			q.setParameter(1, new Date());
			assertEquals(1, q.list().size());

			try {
				// typo: should be configurationName
				q = hibernateUtil.getSession().createQuery("from Configuration as conf where conf.configuratioName = ?");
				q.setParameter(0, "rtobarConfig");
				q.list();
				fail("Should fail, property 'configuratioName' doesn't exist for Configuration objects");
			} catch(QueryException e) {
			}

		} finally {
			dropDB();
		}
	}

	public void testShutdown() throws HibernateUtilException {

		String url = hibernateUtil.getConfiguration().getProperty("hibernate.connection.url");
		if( url.contains("file") ) {
			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().doWork( new Work() {
				public void execute(Connection conn) throws SQLException {
					runScript("shutdown", conn);
				}
			});
			hibernateUtil.commitTransaction();
		}

	}

	private Component createTrascientComponent(ComponentType componentType,
			Configuration config) {
		Component comp = new Component();
		comp.setCode("CodeImplClass");
		comp.setConfiguration(config);
		comp.setXMLDoc("Something that is not XML");
		comp.setRealTime(true);
		comp.setImplLang(ComponentImplLang.JAVA);
		comp.setIsAutostart(true);
		comp.setIsDefault(false);
		comp.setIsControl(true);
		comp.setPath("/path");
		comp.setKeepAliveTime(0);
		comp.setComponentName("rtobarComponent");
		comp.setComponentType(componentType);
		comp.setMinLogLevel((byte)-1);
		comp.setMinLogLevelLocal((byte)-1);
		return comp;
	}
	
	private void createDB() throws HibernateUtilException {

		String url = hibernateUtil.getConfiguration().getProperty("hibernate.connection.url");

		if( url.contains("oracle") ) {
			return;
		}
		hibernateUtil.beginTransaction();
		hibernateUtil.getSession().doWork( new Work() {
			public void execute(Connection conn) throws SQLException {
				conn.setAutoCommit(true);
				runScriptFile(CREATE_TMCDB_SWCORE, conn);
				runScriptFile(CREATE_TMCDB_SWEXT, conn);
			}
		});
		hibernateUtil.commitTransaction();
	}

	private void dropDB() throws HibernateUtilException {

		String url = hibernateUtil.getConfiguration().getProperty("hibernate.connection.url");

		if( !url.contains("mem") ) {
			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().doWork( new Work() {
				public void execute(Connection conn) throws SQLException {
					conn.setAutoCommit(true);
					runScript("delete from event;" +
							"delete from eventchannel;" +
							"delete from baciproperty;" +
							"delete from snmptrapsink;" +
							"delete from powerstripsocket;" +
							"delete from networkpowerstrip;" +
							"delete from networkdevicesnmpconfig;" +
							"delete from component;" +
							"delete from componenttype;" +
							"delete from namedloggerconfig;" +
							"delete from loggingconfig;" +
							"delete from eventchannel;" +
							"delete from computer;" +
							"delete from networkdevice;" +
							"delete from configuration",
							conn);
				}
			});
			hibernateUtil.commitTransaction();
			return;
		}
		hibernateUtil.beginTransaction();
		hibernateUtil.getSession().doWork( new Work() {
			public void execute(Connection conn) throws SQLException {
				conn.setAutoCommit(true);
				runScriptFile(DROP_TMCDB_SWEXT, conn);
				runScriptFile(DROP_TMCDB_SWCORE, conn);
			}
		});
		hibernateUtil.commitTransaction();
	}

    private void runScriptFile( String script, Connection conn ) throws SQLException {

    	String sql = "";
    	
    	try {
			// try to get hold of the script
			InputStream is = getResourceStream(script);
			sql = fileToString(new InputStreamReader(is));
		} catch (IOException e) {
			// convert to runtime, as this is only used for testing
			throw new RuntimeException(e);
		}
		
    	runScript(sql, conn);
    }

    private void runScript( String sql, Connection conn ) throws SQLException {

    	Statement stmt = conn.createStatement();
    	String[] statements = sql.split( ";", -1 );
    	for( int i = 0; i < statements.length; i++ ) {
    		String statement = statements[i].trim();
    		if( statement.length() == 0 ) {
    			// skip empty lines
    			continue;
    		}
    		stmt.execute( statement );
    	}
    }

	private InputStream getResourceStream(String pathname) throws IOException {
        InputStream s = null; // this is the stream we return

        // Look for the resource on the file system
        // -----------------------------------------
        File f = new File( pathname );
        s = new FileInputStream( f );

        return s;
	}

    private String fileToString( Reader reader ) throws IOException {

        BufferedReader br = new BufferedReader( reader );
        StringBuffer sb = new StringBuffer();
        char[] buff = new char[DEFAULT_BUF_LEN];
        while( br.ready() ) {
            int nread = br.read( buff, 0, buff.length );
            if( nread <= 0 ) {
                break;
            }
            sb.append( buff, 0, nread );
        }
        br.close();
        return sb.toString();
    }
}
