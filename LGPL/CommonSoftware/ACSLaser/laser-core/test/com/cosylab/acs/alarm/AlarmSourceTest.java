/*
 * Created on Mar 30, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.alarm;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.Iterator;
import java.util.Properties;

import cern.laser.source.alarmsysteminterface.ASIException;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterface;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.listener.ASIListener;
import cern.laser.source.alarmsysteminterface.listener.ASISubscriberFactory;
import cern.laser.util.buffer.SynchroBuffer;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class AlarmSourceTest extends AlarmTestBase implements ASIListener {

	private long lastReceived;
	final static private long THROUGHPUT_COUNT = 10000;
	final static private double DESIRED_THROUGHPUT = 1000;
	
	//final static private String ALARM_SOURCE_NAME = "TEST_ALARM_SOURCE_THROUGHPUT";
	final static private String ALARM_SOURCE_NAME = "TEST_LASER";
	final static private String ALARM_FAMILY = "TEST_FAMILY";
	final static private String ALARM_MEMBER = "TEST_MEMBER";
	final static private int ALARM_CODE = 12345;
	
	private int count;
	private boolean failed;

	/**
	 * Constructor for AlarmSourceTest.
	 * @param arg0
	 */
	public AlarmSourceTest(String arg0) {
		super(arg0);
		String configBase = AlarmSourceTest.class.getPackage().getName().replaceAll("\\.", "/");
		System.setProperty("laser.asi.config", configBase + "/asi-configuration.xml");
		System.setProperty("cmw.mom.config", configBase + "/cmw-mom-config.properties");
		System.setProperty("synchrobuffer.duplicatepolicy", new Integer(SynchroBuffer.DUPLICATE_OK).toString());
	}
	
	public void test() throws ASIException, InterruptedException
	{
		AlarmSystemInterface alarmSource = AlarmSystemInterfaceFactory.createSource(ALARM_SOURCE_NAME);   
		
		this.count = 0;
		this.failed = false;
		
		ASISubscriberFactory.create().subscribe(ALARM_SOURCE_NAME, this);
		
		long t0 = System.currentTimeMillis();
		
		for(int i = 0; i < THROUGHPUT_COUNT; ++i) {
			FaultState fs = AlarmSystemInterfaceFactory.createFaultState(ALARM_FAMILY, ALARM_MEMBER, ALARM_CODE);
			fs.setDescriptor(FaultState.ACTIVE);
			fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));
		
			Properties props = new Properties();
			props.setProperty(FaultState.ASI_PREFIX_PROPERTY, "prefix"); 
			props.setProperty(FaultState.ASI_SUFFIX_PROPERTY, "suffix"); 
			props.setProperty("TEST_PROPERTY", "TEST_VALUE"); 
			fs.setUserProperties(props); 

			alarmSource.push(fs);			
		}
		long t1 = System.currentTimeMillis();
	
		// TODO This delay is necessary, as otherwise alarms are lost!
		while(this.count < THROUGHPUT_COUNT) {
			Thread.sleep(100);
		}

		assertEquals("Not all alarms were delivered!", THROUGHPUT_COUNT, this.count);
		assertEquals("Alarm content error! See STDERR!", false, this.failed);
		
		double sendThroughput = THROUGHPUT_COUNT/((t1-t0)/1000.0);
		System.out.println("Alarm send throughput: " + sendThroughput + "alarms/s");
		
		double allThroughput = THROUGHPUT_COUNT/((this.lastReceived-t0)/1000.0);
		System.out.println("Alarm roundtrip throughput: " + allThroughput + "alarms/s");
		
		assertTrue("Alarm throughput insufficient!", sendThroughput > DESIRED_THROUGHPUT);
	}

	synchronized public void onMessage(String source, String sourceHostname, Timestamp sourceTimestamp, boolean backup, Collection states) {
		this.count += states.size();
		this.lastReceived = System.currentTimeMillis();
		System.out.println("Message recived:");
		System.out.println("SOURCE NAME: " + source);
		System.out.println("SOURCE HOSTNAME: " + sourceHostname);
		if(ALARM_SOURCE_NAME.compareToIgnoreCase(source) != 0) {
			System.err.println("AlarmSourceTest.onMessage: alarm source name mismatch, got " + source);
			this.failed = true;
		}
		Iterator i = states.iterator();
		while(i.hasNext()) {
			FaultState fs = (FaultState)i.next();
			long delay = System.currentTimeMillis() - fs.getUserTimestamp().getTime();
			//if(delay > 100) {
			//	System.err.println("AlarmSourceTest.onMessage: alarm delayed for " + delay + "ms");
			//	this.failed = true;
			//}
			if(!FaultState.ACTIVE.equals(fs.getDescriptor())) {
				System.err.println("AlarmSourceTest.onMessage: unexpected FaultState descriptor: " + fs.getDescriptor());
				this.failed = true;
			}
			if(!ALARM_FAMILY.equals(fs.getFamily())) {
				System.err.println("AlarmSourceTest.onMessage: unexpected FaultState family: " + fs.getFamily());
				this.failed = true;
			}	
			if(!ALARM_MEMBER.equals(fs.getMember())) {
				System.err.println("AlarmSourceTest.onMessage: unexpected FaultState member: " + fs.getMember());
				this.failed = true;
			}	
			if(fs.getCode() != ALARM_CODE) {
				System.err.println("AlarmSourceTest.onMessage: unexpected FaultState code: " + fs.getCode());
				this.failed = true;
			}
			Properties props = fs.getUserProperties();
			if(!"prefix".equals(props.get(FaultState.ASI_PREFIX_PROPERTY))) {
				System.err.println("AlarmSourceTest.onMessage: unexpected ASI_PREFIX_PROPERTY: " + props.get(FaultState.ASI_PREFIX_PROPERTY));				
				this.failed = true;
			}
			if(!"suffix".equals(props.get(FaultState.ASI_SUFFIX_PROPERTY))) {
				System.err.println("AlarmSourceTest.onMessage: unexpected ASI_SUFFIX_PROPERTY: " + props.get(FaultState.ASI_SUFFIX_PROPERTY));				
				this.failed = true;
			}
			if(!"TEST_VALUE".equals(props.get("TEST_PROPERTY"))) {
				System.err.println("AlarmSourceTest.onMessage: unexpected TEST_PROPERTY: " + props.get("TEST_PROPERTY"));				
				this.failed = true;
			}
		}
	}
}
