package com.cosylab.acs.perftest.client;

import java.util.logging.Level;
import java.util.logging.Logger;
import alma.acs.logging.ClientLogManager;

import com.cosylab.acs.perftest.client.ComponentClientSingleton;
import com.cosylab.acs.perftest.LogErrTestComponent;

import alma.BenchmarkErrType.BenchmarkErr0Ex;
import alma.ACSErr.ACSException;

/**
 * Log Error Test Client
 * Client application that performs tests on C++ built LogErrComponent.
 * 
 * @author anzez
 */
public class LECTClient
{
	private LogErrTestComponent m_LEC;
	long m_startTime, m_endTime;
	String m_deviceName = "LETC1";
	
	public LECTClient() throws Exception {
		m_LEC = com.cosylab.acs.perftest.LogErrTestComponentHelper.narrow(ComponentClientSingleton.getInstance().getContainerServices().getComponent(m_deviceName));
	}
	
	public long getStartTime() {
		return m_startTime;
	}
	
	public long getEndTime() {
		return m_endTime;
	}
	
	public void testClientLogging(long count, long size) {
		StringBuffer buf = new StringBuffer((int)size);
		for (int i = 0; i < size; i++)
			buf.append('*');
		String str = buf.toString();
		Logger logger = ClientLogManager.getAcsLogManager().getStartupLogger();
		m_startTime = System.currentTimeMillis();
		for (int i = 0; i < count; i++)
			logger.log(Level.INFO, str);
		m_endTime = System.currentTimeMillis();
	}
	
	public void testServerLogging(long count, long size) {
		m_startTime = System.currentTimeMillis();
		m_LEC.testLogging((int)count, (int)size);
		m_endTime = System.currentTimeMillis();
	}
	
	public boolean testServerExceptions(long depth) {
		boolean b = false;
		m_startTime = System.currentTimeMillis();
		try {
			m_LEC.testExceptions((int)depth, true);
		} catch (BenchmarkErr0Ex e) {
			// Expected behaviour!
			b = true;
		} catch (ACSException e) {}
		m_endTime = System.currentTimeMillis();
		return b;
	}
	
	public static void main(String[] args) {
		System.setProperty("ACS.manager", "corbaloc::localhost:3000/Manager");
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out
			.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
		String clientName = "LETClient";
		LECTClient client = null;
		try {
			ComponentClientSingleton.prepareInstance(null, managerLoc, clientName);
			client = new LECTClient();
//			client.testClientLogging(10, 100);
//			client.testServerLogging(1000, 100);
			client.testServerExceptions(300);
			System.out.println("Time is: " + (client.getEndTime() - client.getStartTime()));
		}
		catch (Exception e) {
			e.printStackTrace(System.err);
		}
		finally {
			try { ComponentClientSingleton.destroyInstance(); }
			catch (Exception e1) {}
		}
	}
}
