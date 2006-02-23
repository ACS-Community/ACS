package com.cosylab.acs.alarm;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.Iterator;

import cern.laser.source.alarmsysteminterface.AlarmSystemInterface;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.listener.ASIListener;
import cern.laser.source.alarmsysteminterface.listener.ASISubscriberFactory;
import cern.laser.util.buffer.SynchroBuffer;

public class AlarmListener extends AlarmTestBase implements ASIListener
{
	final static private String ALARM_SOURCE_NAME = "TEST_LASER";
	final static private String ALARM_FAMILY = "TEST_FAMILY";
	final static private String ALARM_MEMBER = "TEST_MEMBER";
	final static private int ALARM_CODE = 12345;
	
	public AlarmListener(String whatIsThis)
	{
		super(whatIsThis);
		
		String configBase = AlarmSourceTest.class.getPackage().getName().replaceAll("\\.", "/");
		System.setProperty("laser.asi.config", configBase + "/asi-configuration.xml");
		System.setProperty("cmw.mom.config", configBase + "/cmw-mom-config.properties");
		System.setProperty("synchrobuffer.duplicatepolicy", new Integer(SynchroBuffer.DUPLICATE_OK).toString());
	}

	static final String[] ALARM_SOURCE_NAMES={
			"ALARM_SOURCE_A",
			"ALARM_SOURCE_B",
			"ALARM_SOURCES"
	};
	
	public void test()
	{
		try {
			for (int a=0; a<ALARM_SOURCE_NAMES.length; a++) {
				AlarmSystemInterface alarmSource = AlarmSystemInterfaceFactory.createSource(ALARM_SOURCE_NAMES[a]);
		
				ASISubscriberFactory.create().subscribe(ALARM_SOURCE_NAMES[a], this);
			}
			
			long t0 = System.currentTimeMillis();
	
			Thread.sleep(2000);
		} catch (Exception e) {
			e.printStackTrace();
		}   
	}

	public void onMessage(String source, String sourceHostname, Timestamp sourceTimestamp, boolean backup, Collection states)
	{
		System.out.println("-------------------");
		System.out.println(source);
		System.out.println(sourceHostname);
		System.out.println(sourceTimestamp);
		System.out.println(backup);
		Iterator i=states.iterator();
		while (i.hasNext())
			System.out.println(i.next());
	}
	
	public static void main(String[] args)
	{
		System.setProperty("ACS.manager", "corbaloc::192.168.0.168:3000/Manager");
		new AlarmListener("JustListener").test();
	}
}
