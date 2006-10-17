/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.cdb.dal.test;

import java.net.InetAddress;
import java.util.Properties;

import javax.swing.JApplet;

import com.cosylab.lifecycle.LifecycleListener;
import com.cosylab.lifecycle.LifecyclePhase;
import com.cosylab.lifecycle.LifecycleReporterSupport;

import junit.framework.TestCase;
import junit.framework.TestSuite;

import abeans.core.Identifier;
import abeans.engine.Database;
import abeans.core.defaults.ExceptionIgnorer;
import abeans.framework.ApplicationContext;
import abeans.framework.ApplicationEngine;
import abeans.framework.FrameworkLayer;
import abeans.models.ConnectableConstants;
import abeans.models.Family;
import abeans.models.acs.cdb.dal.DAOChannel;
import alma.acs.util.ACSPorts;

/**
 * JUnit Test for CDB DAL model and plug.
 * Make sure that default ACS CDB database is running.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class CDBDALTest extends TestCase implements ApplicationEngine
{

	/**********************************************************************************/
	
	private final static String POWERSUPPLY_NAME = "alma/PBEND_B_01";
	
	protected LifecycleReporterSupport lifecycleSupport;

	protected Family df = null;
	protected ApplicationContext ctx = null;
	
	private Database db = null;
	private Identifier id = null;
	

	/**
	 */
	public CDBDALTest(String name)
	{
		super(name);
	    lifecycleSupport = new LifecycleReporterSupport(this, false, LifecyclePhase.BEFORE_INITIALIZATION);
	}
	
	/**
	 */
	public static TestSuite suite()
	{
		return new TestSuite(CDBDALTest.class);
	}
	
	/**
	 * @see ApplicationEngine#getApplet()
	 */
	public JApplet getApplet()
	{
		return null;
	}

	/**
	 * @see ApplicationEngine#getModelName()
	 */
	public String getModelName()
	{
		return "Channel";
	}

	/**
	 * @see Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		class IdentifierImpl implements Identifier
		{
			private String name = null;
			private String shortName = null;
			private String qualifiedShortName = null;
			private String qualifiedLongName = null;
			private short type = Identifier.APPLICATION;
	
			public IdentifierImpl()
			{
				this.name = "CDBDALTest";
				this.shortName = "CDBDALTest";
				this.type = Identifier.APPLICATION;
				
			}
			public String getName()
			{
				return name;
			}
			public String getShortName()
			{
				return shortName;
			}
			public String getQualifiedLongName()
			{
				if (qualifiedLongName == null)
				{
						qualifiedLongName = "CDBDALTest";
				}
				return qualifiedLongName;
			}
			public String getQualifiedShortName()
			{
				if (qualifiedShortName == null)
				{
					qualifiedShortName = "CDBDALTest";
				}
				return qualifiedShortName;
			}
			public short getType()
			{
				return type;
			}
		}
		
		if (id == null) id = new IdentifierImpl();
		return id;
	}

	/**
	 * @see Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return false;
	}

	
	/**
	 */
	public void setUp()
	{
		
		try
		{
			// turn off flod of Abeans messages
			Properties systemProps = System.getProperties();
			 
			String namingReference = "iiop://" + InetAddress.getLocalHost().getHostName() + ":" + ACSPorts.getNamingServicePort();
			systemProps.setProperty("NamingServiceRemoteDirectory.reference", namingReference);
			
			String dalReference ="corbaloc::" + InetAddress.getLocalHost().getHostName() + ":"+ACSPorts.getCDBPort()+"/CDB";
			systemProps.setProperty("DAL.defaultReference", dalReference);

			// start 
			lifecycleSupport.fireInitializing();
		    lifecycleSupport.fireInitialized();
		    
			ctx = FrameworkLayer.registerApplication(this);
			db = ctx.getDatabase();	
			df = ctx.getDefaultFamily();
			assertNotNull(ctx);
			assertNotNull(db);
			assertNotNull(df);
			
      
		} catch (Exception e)
		{
			e.printStackTrace();
			fail();
		}
	}
	
	/**
	 */
	public void tearDown()
	{

		try
		{
			lifecycleSupport.fireDestroying();
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
		
		ctx = null; db = null;	id = null;	df = null;

		try
		{
			lifecycleSupport.fireDestroyed();
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}

	}
	
	
	/**
	 */
	public void testConnectAndDisconnect()
	{
		// try connecting
		DAOChannel dc = null;
		try
		{
			dc = new DAOChannel(df, ctx.createRemoteInfo(POWERSUPPLY_NAME));
			assertEquals(ConnectableConstants.CONNECTABLE_CONNECTED, dc.getConnectionStatus());
			assertTrue(df == dc.getParent());
			assertNotNull(dc.getChildren());
			assertEquals(0, dc.getChildren().length);
			assertTrue(db == dc.getDatabase());
			assertEquals(dc.getRemoteInfo(), ctx.createRemoteInfo(POWERSUPPLY_NAME));
			assertTrue(dc.getUniqueName().equals(dc.getRemoteInfo().toURI().toString()));
			assertTrue(!dc.isDestroyed());
			assertTrue(dc.isLinked());
			assertTrue(!dc.isSuspended());
		} catch (Exception e)
		{
			e.printStackTrace();
			fail();
		}
		
		// try disconnecting
		dc.destroy();

		// TODO better implementation preffered
		try
		{
			Thread.sleep(3000);
		}
		catch (InterruptedException ie) {}

		assertEquals(ConnectableConstants.CONNECTABLE_DESTROYED, dc.getConnectionStatus());
		assertTrue(dc.isDestroyed());
		assertTrue(!dc.isLinked());
		assertTrue(!dc.isSuspended());
		//assertNull(dc.getRemoteInfo());
		assertNotNull(dc.getChildren());
		assertEquals(0, dc.getChildren().length);
		
	}
	
	public void testFailedConnect()
	{
		DAOChannel dc = null;
		try
		{
			dc = new DAOChannel(df, ctx.createRemoteInfo("invalid"));
			fail();
		} catch (Exception e)
		{
			new ExceptionIgnorer(e);
			System.out.println("This is OK: " + e.getMessage());
		}
		finally
		{
			if (dc!=null)
				dc.destroy();
		}
	}
	
	
	public void testAllGets()
	{
		DAOChannel dc = null;
		try
		{
			dc = new DAOChannel(df, ctx.createRemoteInfo(POWERSUPPLY_NAME));

			// test long 
			assertEquals(65535, dc.getLongCharacteristic("current/resolution"));

			// test double
			assertEquals(0.01526, dc.getDoubleCharacteristic("current/min_delta_trig"), 0.0);
			assertEquals(980.0, dc.getDoubleCharacteristic("readback/alarm_high_off"), 0.0);

			// test string
			assertTrue("%9.4f".equals(dc.getStringCharacteristic("readback/format")));
			assertTrue("Status".equals(dc.getStringCharacteristic("status/description")));

			// unknown
			assertTrue("A".equals(dc.getCharacteristic("current/units")));


			// test long sequence
			int[] lref = new int[] { 2, 2, 0, 0, 0, 0, 1, 1, 1 };
			int[] l = dc.getIntegerSeqCharacteristic("status/whenSet");
			assertEquals(l.length, lref.length);
			for (int i=0; i<l.length; i++)
				assertEquals(lref[i], l[i]);

			// test double sequence
			double[] dref = new double[] { 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0 };
			double[] d = dc.getDoubleSeqCharacteristic("status/whenCleared");
			assertEquals(d.length, dref.length);
			for (int i=0; i<d.length; i++)
				assertEquals(dref[i], d[i], 0.0);

			// test string sequence
			String[] sref = new String[] { "On", "Remote", "Sum Failure", "External Interlock", "DC Overcurrent", "Phase Failure", "Not Ready", "State Inconsistent", "Ramping" };
			String[] s = dc.getStringSeqCharacteristic("status/bitDescription");
			assertEquals(sref.length, s.length);
			for (int i=0; i<s.length; i++)
				assertTrue(s[i].equals(sref[i]));


			// test string sequence - query sub node
			sref = new String[] {
				"description", "format", "units", "resolution", "bitDescription", "whenSet", "whenCleared",
				"alarm_low_on", "alarm_low_off", "alarm_high_on", "alarm_high_off", "alarm_timer_trig",
				"default_value", "min_step",
				"archive_delta", "initialize_devio", "archive_priority", "archive_min_int", "archive_max_int",
				"default_timer_trig", "min_timer_trig"
			};
			s = dc.getStringSeqCharacteristic("status");
			assertEquals(sref.length, s.length);
			for (int i=0; i<s.length; i++)
				assertTrue(s[i].equals(sref[i]));
			// test string sequence - getValue
			sref = new String[] { "current", "readback", "status" };
			s = dc.getValue();
			assertEquals(sref.length, s.length);
			for (int i=0; i<s.length; i++)
				assertTrue(s[i].equals(sref[i]));

		} catch (Exception e)
		{
			e.printStackTrace();
			fail();
		}
		finally
		{
			if (dc!=null)
				dc.destroy();
		}
	}
	
	/**
	 * @see com.cosylab.lifecycle.LifecycleReporter#addLifecycleListener(LifecycleListener)
	 */
	public void addLifecycleListener(LifecycleListener l) {
	    lifecycleSupport.addLifecycleListener(l);
	}

	/**
	 * @see com.cosylab.lifecycle.LifecycleReporter#getLifecyclePhase()
	 */
	public LifecyclePhase getLifecyclePhase() {
		return lifecycleSupport.getLifecyclePhase();
	}

	/**
	 * @see com.cosylab.lifecycle.LifecycleReporter#isRecyclable()
	 */
	public boolean isRecyclable() {
		return lifecycleSupport.isRecyclable();
	}

	/**
	 * @see com.cosylab.lifecycle.LifecycleReporter#removeLifecycleListener(LifecycleListener)
	 */
	public void removeLifecycleListener(LifecycleListener l) {
	    lifecycleSupport.removeLifecycleListener(l);
	}

	public static void main(String[] args) {
		//junit.textui.TestRunner.run(CDBDALTest.class);
		CDBDALTest test = new CDBDALTest("testName");
		test.setUp();
		test.testConnectAndDisconnect();
		test.tearDown();
		
		System.exit(0);
	}

}
