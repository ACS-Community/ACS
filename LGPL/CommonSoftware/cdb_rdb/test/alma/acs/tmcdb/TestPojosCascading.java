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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.TestCase;

import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.jdbc.Work;

import alma.acs.logging.ClientLogManager;
import alma.acs.logging.level.AcsLogLevelDefinition;

import com.cosylab.cdb.jdal.hibernate.HibernateUtil;
import com.cosylab.cdb.jdal.hibernate.HibernateUtil.HibernateUtilException;
import com.cosylab.cdb.jdal.logging.AcsLoggerHelper;

public class TestPojosCascading extends TestCase {

	private final String CREATE_TMCDB_SWCORE = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigcore/CreateHsqldbTables.sql";
	private final String CREATE_TMCDB_SWEXT  = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigext/CreateHsqldbTables.sql";
	private final String DROP_TMCDB_SWCORE = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigcore/DropAllTables.sql";
	private final String DROP_TMCDB_SWEXT  = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigext/DropAllTables.sql";
    private final static int DEFAULT_BUF_LEN = 64 * 1024;

	private Logger logger;
	private HibernateUtil hibernateUtil;

	protected void setUp() throws Exception {
		System.out.println("\n---------------- " + getName() + " ---------------\n");
		String loggerName = getName(); // name of junit test method
		AcsLoggerHelper acsLoggerHelper = AcsLoggerHelper.getInstance();
		acsLoggerHelper.setLoggerName(loggerName);
		acsLoggerHelper.setDefaultLogLevels(AcsLogLevelDefinition.INFO, AcsLogLevelDefinition.INFO);
		logger = AcsLoggerHelper.getInstance().getSharedLogger();
		
		acsLoggerHelper.setHibernateLogLevels(AcsLogLevelDefinition.INFO, AcsLogLevelDefinition.WARNING);
		acsLoggerHelper.setHibernateSqlLogLevels(AcsLogLevelDefinition.TRACE, AcsLogLevelDefinition.TRACE);

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

	public void testNoCascading() throws Exception {

		createDB();

		try {
			Configuration conf = new Configuration();
			conf.setConfigurationName("rtobarConfig");
			conf.setFullName("Super full name");
			conf.setActive(true);
			conf.setCreationTime(new Date());
			conf.setDescription("Testing configuration");

			EventChannel ec = new EventChannel();
			ec.setName("NC1");
			ec.setPath("");
			ec.setConfiguration(conf);

			try {
				hibernateUtil.beginTransaction();
				hibernateUtil.getSession().save(ec);
				hibernateUtil.commitTransaction();
				fail("Should fail since configuration is not cascaded by EC");
			} catch(Throwable t) {
				hibernateUtil.closeSession();
			}

			ec = new EventChannel();
			ec.setName("NC1");
			ec.setPath("path");    // path is part of a UNIQUE statement, so it must have a value (it seems)
			ec.setConfiguration(conf);
			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(conf);
			hibernateUtil.getSession().save(ec);
			hibernateUtil.commitTransaction();
		} catch (Exception ex) {
			logger.log(Level.WARNING, "Got a failure already before dropDB is called", ex);
			throw ex;
		} finally {
			dropDB();
		}

	}

	public void testCascadingAggregation() throws Exception {

		createDB();

		try {

			LoggingConfig lconf = new LoggingConfig();

			NamedLoggerConfig nlconf = new NamedLoggerConfig();
			nlconf.setName("rtobarNamedLoggingConfig");
			nlconf.setMinLogLevel((byte)0x01);
			nlconf.setMinLogLevelLocal((byte)0x01);
			nlconf.setLoggingConfig(lconf);

			Set<NamedLoggerConfig> nlconfs = new HashSet<NamedLoggerConfig>();
			nlconfs.add(nlconf);
			lconf.setNamedLoggerConfigs(nlconfs);

			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(lconf); // Should cascade the NamedLoggerConfig
			hibernateUtil.commitTransaction();

		} finally {
			dropDB();
		}
	}

	@SuppressWarnings("unchecked")
	public void testCascadingInverseAggregation() throws Exception {

		createDB();

		try {
			Component comp = createTrasientFilledComponent();

			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(comp.getConfiguration());
			hibernateUtil.getSession().save(comp.getComponentType());
			hibernateUtil.getSession().save(comp); // compType cascades
			hibernateUtil.commitTransaction();

			List res = hibernateUtil.getList(ComponentType.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(Component.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(Configuration.class);
			assertEquals(1, res.size());

			// Now try inverse cascading
			Set<BACIProperty> props = new HashSet<BACIProperty>();
			for(int i=0; i!= 10; i++) { 
				props.add( createTrasientFilledBACIProperty("Prop-" + i, comp) );
			}
			comp.setBACIProperties(props);

			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().saveOrUpdate(comp); // props cascade as inverse
			hibernateUtil.commitTransaction();

			res = hibernateUtil.getList(ComponentType.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(Component.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(Configuration.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(BACIProperty.class);
			assertEquals(10, res.size());

		} catch (Exception ex) {
			logger.log(Level.WARNING, "Got a failure already before dropDB is called", ex);
			throw ex;
		} finally  {
			dropDB();
		}
	}

	@SuppressWarnings("unchecked")
	public void testCascadingInverseComposition() throws Exception {
		createDB();

		try {
			Component comp = createTrasientFilledComponent();

			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().save(comp.getConfiguration());
			hibernateUtil.getSession().save(comp.getComponentType());
			hibernateUtil.getSession().save(comp); // compType cascades
			hibernateUtil.commitTransaction();

			List res = hibernateUtil.getList(ComponentType.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(Component.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(Configuration.class);
			assertEquals(1, res.size());

			// Now try inverse cascading
			Set<BACIProperty> props = new HashSet<BACIProperty>();
			for(int i=0; i!= 10; i++) { 
				props.add( createTrasientFilledBACIProperty("Prop-" + i, comp) );
			}
			comp.setBACIProperties(props);

			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().saveOrUpdate(comp); // props cascade as inverse
			hibernateUtil.commitTransaction();

			res = hibernateUtil.getList(ComponentType.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(Component.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(Configuration.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(BACIProperty.class);
			assertEquals(10, res.size());

			// And now delete the component - Component and BACI properties should get deleted
			hibernateUtil.beginTransaction();
			hibernateUtil.getSession().delete(comp); // props cascade as inverse
			hibernateUtil.commitTransaction();

			res = hibernateUtil.getList(ComponentType.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(Component.class);
			assertEquals(0, res.size());
			res = hibernateUtil.getList(Configuration.class);
			assertEquals(1, res.size());
			res = hibernateUtil.getList(BACIProperty.class);
			assertEquals(0, res.size());

		} catch (Exception ex) {
			logger.log(Level.WARNING, "Got a failure already before dropDB is called", ex);
			throw ex;
		} finally  {
			dropDB();
		}

	}

	private Component createTrasientFilledComponent() {

		ComponentType compType = new ComponentType();
		compType.setIDL("IDL:alma/Some/IF.idl:1.0");

		Configuration conf = new Configuration();
		conf.setConfigurationName("rtobarConfig");
		conf.setFullName("Super full name");
		conf.setActive(true);
		conf.setCreationTime(new Date());
		conf.setDescription("Testing configuration");

		Component comp = new Component();
		comp.setComponentType(compType);
		comp.setConfiguration(conf);


		comp.setContainer(null); // Container can be null
		comp.setComponentName("COMPONENT");
		comp.setCode("Code");
		comp.setImplLang(ComponentImplLang.JAVA);
		comp.setRealTime(false);
		comp.setPath(".");
		comp.setIsAutostart(true);
		comp.setIsDefault(true);
		comp.setIsControl(false);
		comp.setIsStandaloneDefined(true);
		comp.setKeepAliveTime(0);
		comp.setMinLogLevel((byte)0);
		comp.setMinLogLevelLocal((byte)0);
		comp.setXMLDoc("<doc/>");

		return comp;
	}

	private BACIProperty createTrasientFilledBACIProperty(String name, Component comp) {
		BACIProperty prop = new BACIProperty();
		prop.setComponent(comp);
		prop.setPropertyName(name);
		prop.setDescription("desc");
		prop.setFormat("%d");
		prop.setUnits("[s]");
		prop.setResolution("0.1");
		prop.setArchive_priority(1);
		prop.setArchive_min_int(1.0);
		prop.setArchive_max_int(1.0);
		prop.setArchive_suppress(false);
		prop.setArchive_mechanism(BACIPropArchMech.MONITOR_COLLECTOR);
		prop.setDefault_timer_trig(0.1);
		prop.setMin_timer_trig(0.1);
		prop.setInitialize_devio(true);
		prop.setDefault_value("0.1");
		prop.setArchive_delta(0.1);
		return prop;
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
