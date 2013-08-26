/* @(#)TestTmcdb.java $Revision: 1.5 $ $Date: 2011/05/13 17:40:40 $
 *
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory,
 * 2007
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at
 * your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
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
import java.util.logging.Logger;

import junit.framework.TestCase;

import org.hibernate.LockMode;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.jdbc.Work;

import alma.acs.logging.level.AcsLogLevelDefinition;

import com.cosylab.cdb.jdal.hibernate.HibernateUtil;
import com.cosylab.cdb.jdal.hibernate.HibernateUtil.HibernateUtilException;
import com.cosylab.cdb.jdal.logging.AcsLoggerHelper;

/** TestTmcdb class tests TMCDB for SNMP.  It creates a NodeTree, fills the
 *  TMCDB, and finally creates a new NodeTree based on the TMCDB content.
 *
 * @version $Id: TestSnmpTables.java,v 1.5 2011/05/13 17:40:40 rtobar Exp $
 * @author P.Grosbol, ESO, <pgrosbol@eso.org>
 */

public class TestSnmpTables extends TestCase {

	private final String CREATE_TMCDB_SWCORE = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigcore/CreateHsqldbTables.sql";
	private final String CREATE_TMCDB_SWEXT  = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigext/CreateHsqldbTables.sql";
	private final String DROP_TMCDB_SWCORE = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigcore/DropAllTables.sql";
	private final String DROP_TMCDB_SWEXT  = System.getenv("ACSDATA") + "/config/DDL/hsqldb/TMCDB_swconfigext/DropAllTables.sql";

    private final static int DEFAULT_BUF_LEN = 64 * 1024;                       
    private HibernateUtil hibernateUtil;
    private Logger logger = Logger.getLogger("SnmpTmcdb");

    protected void setUp() throws Exception {

		System.out.println("\n---------------- " + getName() + " ---------------\n");
		String loggerName = getName(); // name of junit test
		AcsLoggerHelper acsLoggerHelper = AcsLoggerHelper.getInstance();
		acsLoggerHelper.setLoggerName(loggerName);
		acsLoggerHelper.setDefaultLogLevels(AcsLogLevelDefinition.INFO, AcsLogLevelDefinition.INFO);
		logger = AcsLoggerHelper.getInstance().getSharedLogger();
		logger.info("Example test log");

		acsLoggerHelper.setHibernateLogLevels(AcsLogLevelDefinition.INFO, AcsLogLevelDefinition.WARNING);
		acsLoggerHelper.setHibernateSqlLogLevels(AcsLogLevelDefinition.TRACE, AcsLogLevelDefinition.TRACE);
    	
    	
    	hibernateUtil = HibernateUtil.getInstance(logger);
    	AnnotationConfiguration ac = new AnnotationConfiguration();
    	ac.configure("test-hibernate.cfg.xml");
    	hibernateUtil.setConfiguration(ac);
    	hibernateUtil.getSessionFactory().openSession();                
    }

    @SuppressWarnings("unchecked")
	public void testFillSnmpTmcdb() throws Exception {

    	createDB();

    	try {
    		Configuration config = new Configuration();
    		config.setActive(true);
    		config.setConfigurationName("pg_Config");
    		config.setCreationTime(new Date());
    		config.setFullName("Testing configuration");
    		config.setDescription("Configuration used for testing SNMP");

    		Computer comp1 = new Computer();
    		comp1.setName("osf.comp-1");   
    		comp1.setNetworkName("pc014692.hq.eso.org");
    		comp1.setPhysicalLocation("Alma building");
    		comp1.setRealTime(false); 
    		comp1.setDiskless(false);        
    		comp1.setProcessorType(ComputerProcessorType.SMP);
    		comp1.setConfiguration(config);

    		Computer comp2 = new Computer();
    		comp2.setName("osf.comp-2");   
    		comp2.setNetworkName("pc008506.hq.eso.org");
    		comp2.setPhysicalLocation("Alma building");
    		comp2.setRealTime(false); 
    		comp2.setDiskless(false);        
    		comp2.setProcessorType(ComputerProcessorType.UNI);         
    		comp2.setConfiguration(config);

    		/* setup a SNMP trap sink                                */

    		SnmpTrapSink sink = new SnmpTrapSink();
    		sink.setComputer(comp1);
    		sink.setTrapPort(new Integer(10162));
    		sink.setTrapSourcesNetworkMask("0.0.0.0");
    		sink.setSnmpTrapCommunity("public");
    		sink.setConfiguration(config);

    		/* setup a powerstrip with 3 sockets                     */
    		NetworkPowerstrip strip1 = new  NetworkPowerstrip();
    		strip1.setName("osf.pdu1");   
    		strip1.setNetworkName("pdu1.hq.eso.org");
    		strip1.setPhysicalLocation("Alma building");
    		strip1.setConfiguration(config);

    		PowerstripSocket socket1 = new PowerstripSocket();
    		socket1.setNetworkPowerstrip(strip1);
    		socket1.setNetworkDevice(comp1);
    		socket1.setSocketNumber(new Integer(1));
    		socket1.setSocketName("comp-1");
    		PowerstripSocket socket2 = new PowerstripSocket();
    		socket2.setNetworkPowerstrip(strip1);
    		socket2.setNetworkDevice(comp1);
    		socket2.setSocketNumber(new Integer(2));
    		socket2.setSocketName("comp-1");
    		PowerstripSocket socket3 = new PowerstripSocket();
    		socket3.setNetworkPowerstrip(strip1);
    		socket3.setNetworkDevice(comp2);
    		socket3.setSocketNumber(new Integer(3));
    		socket3.setSocketName("comp-2");

    		HashSet<PowerstripSocket> pss = new HashSet<PowerstripSocket>();
    		pss.add(socket1);
    		pss.add(socket2);
    		pss.add(socket3);

    		//strip1.setPowerstripSockets(pss);

    		HashSet<NetworkDevice> nds = new HashSet<NetworkDevice>();
    		nds.add(comp1);
    		nds.add(comp2);
    		nds.add(strip1);
    		//config.setNetworkDevices(nds);

    		/* setup a SNMP configuration for computers                    */

    		NetworkDeviceSnmpConfig snmp1 = new NetworkDeviceSnmpConfig();
    		snmp1.setPropagateNA(false);
    		snmp1.setSnmpCommunity("public");
    		snmp1.setAcsAlarm(NetDevSnmpConfigAcsAlarm.NEVER);
    		snmp1.setNetworkDevice(comp1);
    		String xml1 = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
    			+"<netnode name=\"ps014692\" xmlns=\"Alma/Exec/SnmpConfig\">\n"
    			+"<uptime />\n"
    			+"<cpuload  type=\"2\" max=\"10\" />\n"
    			+"<memory  min=\"10\" />\n"
    			+"<diskspace>\n"
    			+" <disk  name=\"/home\"  max=\"90\" />\n"
    			+" <disk  name=\"/\"  max=\"90\" />\n"
    			+" <file  name=\"/var/log/messages\"  max=\"100000000\" />\n"
    			+" </diskspace>\n"
    			+" <services>\n"
    			+" <proc  name=\"maciContainer\" min=\"0\" max=\"1\"  />\n"
    			+" </services>\n"
    			+" <scripts>\n"
    			+" <exec  name=\"/usr/bin/uptime\" />\n"
    			+" <exec  name=\"/sbin/reboot\" />\n"
    			+" <exec  name=\"/home/pgrosbol/bin/snmpexec.sh\" />\n"
    			+" </scripts>\n"
    			+" <netports>\n"
    			+"<port  name=\"udp/0.0.0.0:161\" />\n"
    			+" <port  name=\"udp/0.0.0.0:10162\" />\n"
    			+" </netports>\n"
    			+" <interfaces>\n"
    			+" <interface  name=\"eth0\"  />\n"
    			+" </interfaces>\n"
    			+" </netnode>\n";
    		snmp1.setSnmpXmlClob(xml1);

    		NetworkDeviceSnmpConfig snmp2 = new NetworkDeviceSnmpConfig();
    		snmp2.setPropagateNA(true);
    		snmp2.setSnmpCommunity("public");
    		snmp2.setAcsAlarm(NetDevSnmpConfigAcsAlarm.ALWAYS);
    		snmp2.setNetworkDevice(comp2);
    		String xml2 = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
    			+"<netnode name=\"pc008506\" xmlns=\"Alma/Exec/SnmpConfig\">\n"
    			+"<uptime />\n"
    			+"<cpuload  type=\"2\" max=\"10\" />\n"
    			+"<memory  min=\"10\" />\n"
    			+"<diskspace>\n"
    			+" <disk  name=\"/home\"  max=\"90\" />\n"
    			+" <disk  name=\"/\"  max=\"90\" />\n"
    			+" <file  name=\"/var/log/messages\"  max=\"100000000\" />\n"
    			+" </diskspace>\n"
    			+" <services>\n"
    			+" <proc  name=\"maciContainer\" min=\"0\" max=\"1\"  />\n"
    			+" </services>\n"
    			+" <scripts>\n"
    			+" <exec  name=\"/usr/bin/uptime\" />\n"
    			+" <exec  name=\"/sbin/reboot\" />\n"
    			+" <exec  name=\"/home/pgrosbol/bin/snmpexec.sh\" />\n"
    			+" </scripts>\n"
    			+" <netports>\n"
    			+"<port  name=\"udp/0.0.0.0:161\" />\n"
    			+" <port  name=\"udp/0.0.0.0:10162\" />\n"
    			+" </netports>\n"
    			+" <interfaces>\n"
    			+" <interface  name=\"eth0\"  />\n"
    			+" </interfaces>\n"
    			+" </netnode>\n";
    		snmp2.setSnmpXmlClob(xml2);

    		/*  store data in TMCDB                                   */

    		hibernateUtil.beginTransaction();
    		hibernateUtil.getSession().save(config);
    		hibernateUtil.getSession().save(comp1);
    		hibernateUtil.getSession().save(comp2);
    		hibernateUtil.getSession().save(snmp1);
    		hibernateUtil.getSession().save(snmp2);
    		hibernateUtil.getSession().save(sink);
    		hibernateUtil.getSession().save(strip1);
    		hibernateUtil.getSession().save(socket1);
    		hibernateUtil.getSession().save(socket2);
    		hibernateUtil.getSession().save(socket3);
    		hibernateUtil.commitTransaction();

    		assertEquals( 1 , hibernateUtil.getList(Configuration.class).size());
    		assertEquals( 1 , hibernateUtil.getList(NetworkPowerstrip.class).size());
    		assertEquals( 1 , hibernateUtil.getList(SnmpTrapSink.class).size());
    		assertEquals( 3 , hibernateUtil.getList(PowerstripSocket.class).size());
    		assertEquals( 2 , hibernateUtil.getList(Computer.class).size());
    		assertEquals( 2 , hibernateUtil.getList(NetworkDeviceSnmpConfig.class).size());

    		hibernateUtil.closeSession();
    		hibernateUtil.beginTransaction();
    		List<PowerstripSocket> sockets = hibernateUtil.getList(PowerstripSocket.class);
    		hibernateUtil.getSession().lock(strip1, LockMode.NONE);
    		for (PowerstripSocket powerstripSocket : sockets) {
    			hibernateUtil.getSession().lock(powerstripSocket, LockMode.NONE);
    			assertEquals( powerstripSocket.getSocketName().equals("comp-1") ? comp1.getName() : comp2.getName(), powerstripSocket.getNetworkDevice().getName() );
    			assertEquals( strip1.getName(), powerstripSocket.getNetworkPowerstrip().getName() );
    		}
    		hibernateUtil.closeSession();
    	} finally {
    		dropDB();	
    	}
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

    private void runScriptFile( String script, Connection conn )
    throws SQLException {

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

    private void runScript( String sql, Connection conn )
    throws SQLException {

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

    private InputStream getResourceStream(String pathname)
    throws IOException {

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
