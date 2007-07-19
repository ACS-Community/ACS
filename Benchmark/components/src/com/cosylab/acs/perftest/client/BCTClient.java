package com.cosylab.acs.perftest.client;

import java.util.Vector;

import org.omg.CORBA.ORB;

import alma.acs.component.client.ComponentClient;
import com.cosylab.acs.perftest.client.ComponentClientSingleton;
import alma.perftest.SimpleBACIComponent;
import alma.perftest.ComplexBACIComponent;

import alma.ACS.RWlong;
import alma.ACS.CBlongPOA;
import alma.ACS.CBvoidPOA;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACS.Monitorlong;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;

/**
 * BACI Components Test Client
 * Client application that performs tests on C++ built BACI components.
 * 
 * @author anzez
 */
public class BCTClient
{
	class CBvoidImpl extends CBvoidPOA
	{
		private long m_num;
		private boolean m_done;

		public synchronized void done(Completion completion, CBDescOut desc) {
			if (--m_num == 0) {
				m_done = true;
				this.notify();
			}
		}

		public synchronized void working(Completion completion, CBDescOut desc) {
		}
		
		public boolean negotiate(long arg0, CBDescOut arg1) {
			return false;
		}
		
		public void setReturns(long num) {
			m_num = num;
			m_done = false;
		}
		
		public boolean isDone() {
			return m_done;
		}
	}
	
	class CBlongImpl extends CBlongPOA
	{
		private long m_num;
		private boolean m_done;
		private Vector m_queue = new Vector();
		
		public synchronized void done(int value, Completion completion, CBDescOut desc) {
			if (--m_num == 0) {
				m_done = true;
				this.notify();
			}
		}

		public synchronized void working(int value, Completion completion, CBDescOut desc) {
			if (desc.id_tag == 1208) {
				if (--m_num == 0) {
					m_done = true;
					this.notify();
				}
			}
			else {
				m_queue.add(new java.lang.Long(System.currentTimeMillis()));
				this.notify();
			}
		}
				
		public boolean negotiate(long arg0, CBDescOut arg1) {
			return false;
		}
		
		public void setReturns(long num) {
			m_num = num;
			m_done = false;
		}

		public boolean isDone() {
			return m_done;
		}
		
		public boolean isEmptyQueue() {
			return m_queue.size() == 0;
		}
		
		public void clearQueue() {
			m_queue.clear();
		}
		
		public long nextTime() {
			if (m_queue.size() == 0)
				return 0;
			return ((java.lang.Long)m_queue.remove(0)).longValue();
		}
	}
	
	private static final String[] SBCs = new String[] {"SBC01", "SBC02", "SBC03", "SBC04", "SBC05", "SBC06", "SBC07", "SBC08", "SBC09", "SBC10", "SBC11", "SBC12", "SBC13", "SBC14", "SBC15", "SBC16"};
	private static final String[] CBCs = new String[] {"CBC01", "CBC02", "CBC03", "CBC04", "CBC05", "CBC06", "CBC07", "CBC08", "CBC09", "CBC10", "CBC11", "CBC12", "CBC13", "CBC14", "CBC15", "CBC16"};
	
	private static final String m_deviceName = "SBC00";
	private static final CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
	private static final CBDescIn descIn2 = new CBDescIn(50000, 50000, 1209);
	private SimpleBACIComponent m_SBC;
	private RWlong m_property;
	private CBlongImpl m_cbReturned = new CBlongImpl();
	private CBvoidImpl m_cbNotified = new CBvoidImpl();
	private long m_startTime, m_midTime, m_endTime;
	private long m_lastTime;
	private Monitorlong m_monitor = null;
	
	public BCTClient() throws Exception {
		m_SBC = alma.perftest.SimpleBACIComponentHelper.narrow(ComponentClientSingleton.getInstance().getContainerServices().getComponent(m_deviceName));
		m_property = m_SBC.property();
	}
	
	public long getStartTime() {
		return m_startTime;
	}
	
	public long getMidTime() {
		return m_midTime;
	}
	
	public long getEndTime() {
		return m_endTime;
	}

	/**
	 * TEST_1_3_1 & TEST_1_3_3
	 * @param loop
	 */
	public boolean activateSBC(long count) {
		if (count > 16) return false;
		SimpleBACIComponent[] SBC = new SimpleBACIComponent[(int)count];
		ComponentClient cc = ComponentClientSingleton.getInstance();
		m_startTime = System.currentTimeMillis();
		try {
			for (int i = 0; i < count; i++)
				SBC[i] = alma.perftest.SimpleBACIComponentHelper.narrow(cc.getContainerServices().getComponent(SBCs[i]));
		} catch (Exception e) { return false; }
		m_midTime = System.currentTimeMillis();
		for (int i = 0; i < count; i++) {
			SBC[i] = null;
			cc.getContainerServices().releaseComponent(SBCs[i]);
		}
		m_endTime = System.currentTimeMillis();
		return true;
	}

	/**
	 * TEST_1_3_2 & TEST_1_3_4
	 * @param loop
	 */
	public boolean activateCBC(long count) {
		if (count > 16) return false;
		ComplexBACIComponent[] CBC = new ComplexBACIComponent[(int)count];
		ComponentClient cc = ComponentClientSingleton.getInstance();
		m_startTime = System.currentTimeMillis();
		try {
			for (int i = 0; i < count; i++)
				CBC[i] = alma.perftest.ComplexBACIComponentHelper.narrow(cc.getContainerServices().getComponent(CBCs[i]));
		} catch (Exception e) { return false; }
		m_midTime = System.currentTimeMillis();
		for (int i = 0; i < count; i++) {
			CBC[i] = null;
			cc.getContainerServices().releaseComponent(CBCs[i]);
		}
		m_endTime = System.currentTimeMillis();
		return true;
	}

	/**
	 * TEST_2_1_1 & TEST_3_1_1
	 * @param loop
	 */
	public void getPropertySync(long loop) {
		CompletionHolder completionHolder = new CompletionHolder();
		m_startTime = System.currentTimeMillis();
		for(int i = 0; i < loop; i++)
			m_property.get_sync(completionHolder);
		m_endTime = System.currentTimeMillis();
	}
	
	/**
	 * TEST_2_1_2 & TEST_3_1_2
	 * @param loop
	 */
	public void setPropertySync(long loop) {
		m_startTime = System.currentTimeMillis();
		for(int i = 0; i < loop; i++)
			m_property.set_sync(0);
		m_endTime = System.currentTimeMillis();
	}

	/**
	 * TEST_2_1_3 & TEST_3_1_3
	 * @param loop
	 * @return
	 */
	public boolean getDeviceCharacteristic(long loop) {
		try {
			m_startTime = System.currentTimeMillis();
			for (int i = 0; i < loop; i++)
				m_SBC.get_characteristic_by_name("characteristic");
			m_endTime = System.currentTimeMillis();
		} catch (Exception e) { return false; }
		return true;
	}

	/**
	 * TEST_2_1_4 & TEST_3_1_4
	 * @param loop
	 * @return
	 */
	public boolean getPropertyCharacteristic(long loop) {
		try {
			m_startTime = System.currentTimeMillis();
			for (int i = 0; i < loop; i++)
				m_property.get_characteristic_by_name("description");
			m_endTime = System.currentTimeMillis();
		} catch (Exception e) { return false; }
		return true;
	}
	
	/**
	 * TEST_2_1_5 & TEST_3_1_5
	 * @param loop
	 */
	public void methodSync(long loop) {
		m_startTime = System.currentTimeMillis();
		for(int i = 0; i < loop; i++)
			m_SBC.method();
		m_endTime = System.currentTimeMillis();
	}
	
	/**
	 * TEST_2_2_1_* & TEST_3_2_1_*
	 * @param loop
	 * @param delay
	 * @return
	 */
	public boolean getPropertyAsync(long loop, long delay) {
		m_cbReturned.setReturns(loop);
		ORB orb = ComponentClientSingleton.getInstance().getORB();
		m_startTime = System.currentTimeMillis();
		for (int i = 0; i < loop; i++) {
			m_property.get_async(m_cbReturned._this(orb), descIn);
			try { Thread.sleep(delay); } catch(Exception e) {}
		}
		m_midTime = System.currentTimeMillis();
		synchronized (m_cbReturned) {
			if (!m_cbReturned.isDone()) 
				try { m_cbReturned.wait(); } catch(InterruptedException ie) {}
			m_endTime = System.currentTimeMillis();
		}
		return m_cbReturned.isDone();
	}

	/**
	 * TEST_2_2_2_* & TEST_3_2_2_*
	 * @param loop
	 * @param delay
	 * @return
	 */
	public boolean setPropertyAsync(long loop, long delay) {
		m_cbNotified.setReturns(loop);
		ORB orb = ComponentClientSingleton.getInstance().getORB();
		m_startTime = System.currentTimeMillis();
		for (int i = 0; i < loop; i++) {
			m_property.set_async(0, m_cbNotified._this(orb), descIn);
			try { Thread.sleep(delay); } catch(Exception e) {}
		}
		m_midTime = System.currentTimeMillis();
		synchronized (m_cbNotified) {
			if (!m_cbNotified.isDone()) 
				try { m_cbNotified.wait(); } catch (InterruptedException ie) {}
			m_endTime = System.currentTimeMillis();
		}
		return m_cbNotified.isDone();
	}

	/**
	 * TEST_2_2_3_* & TEST_3_2_3_*
	 * @param loop
	 * @param delay
	 * @return
	 */
	public boolean actionAsync(long loop, long delay) {
		m_cbNotified.setReturns(loop);
		ORB orb = ComponentClientSingleton.getInstance().getORB();
		m_startTime = System.currentTimeMillis();
		for(int i = 0; i < loop; i++) {
			m_SBC.action(m_cbNotified._this(orb), descIn);
			try { Thread.sleep(delay); } catch(Exception e) {}
		}
		m_midTime = System.currentTimeMillis();
		synchronized (m_cbNotified)
		{
			if (!m_cbNotified.isDone()) 
				try { m_cbNotified.wait(); } catch (InterruptedException ie) {}
			m_endTime = System.currentTimeMillis();
		}
		return m_cbNotified.isDone();
	}

	/**
	 * TEST_2_3_1 & TEST_3_3_1 (initialization)
	 * @param time
	 */
	public void startMonitor(long time) {
		m_lastTime = 0;
		m_cbReturned.clearQueue();
		m_monitor = m_property.create_monitor(m_cbReturned._this(ComponentClientSingleton.getInstance().getORB()), descIn2);
		m_monitor.set_timer_trigger(time * 10000); // time is in ms
	}
	
	/**
	 * TEST_2_3_1 & TEST_3_3_1 (finalization)
	 *
	 */
	public void stopMonitor() {
		if (m_monitor != null) {
			m_monitor.destroy();
			m_monitor = null;
		}
	}
	
	/**
	 * TEST_2_3_1 & TEST_3_3_1
	 * @return
	 */
	public boolean getNextMonitorResponse() {
		if (m_monitor == null) return false;
		m_startTime = m_lastTime;
		synchronized (m_cbReturned) {
			while (m_cbReturned.isEmptyQueue()) {
				try { m_cbReturned.wait(); } catch (InterruptedException e) {}
			}
		}
		m_endTime = m_cbReturned.nextTime();
		m_lastTime = m_endTime;
		return true;
	}
	
	/**
	 * TEST_2_3_2 & TEST_3_3_2
	 * @param loop
	 * @param delayIn100ns
	 * @return
	 */
	public boolean monitors(long loop, long delayIn100ns) {
		ORB orb = ComponentClientSingleton.getInstance().getORB();
		Monitorlong monitor;
		monitor = m_property.create_monitor(m_cbReturned._this(orb), descIn);
		monitor.set_timer_trigger(delayIn100ns);
		synchronized (m_cbReturned)
		{
			m_cbReturned.setReturns(loop);
			m_startTime = System.currentTimeMillis();
			try { m_cbReturned.wait(); } catch (InterruptedException ie) {}
			m_endTime = System.currentTimeMillis();
		}
		monitor.destroy();
		return m_cbReturned.isDone();
	}

	/**
	 * TEST_2_3_3
	 * @param loop
	 */
	public void createMonitor(long loop) {
		Monitorlong monitors[] = new Monitorlong[(int)loop];
		ORB orb = ComponentClientSingleton.getInstance().getORB();
		m_startTime = System.currentTimeMillis();
		for (int i = 0; i < loop; i++)
			monitors[i] = m_property.create_monitor(m_cbReturned._this(orb), descIn);
		m_midTime = System.currentTimeMillis();
		for (int i = 0; i < loop; i++)
			monitors[i].destroy();
		m_endTime = System.currentTimeMillis();
	}

	/**
	 * TEST_2_6 & TEST_3_6
	 * @param loop
	 */
	public void getCOB(long loop) {
		SimpleBACIComponent[] SBC = new SimpleBACIComponent[(int)loop];
		ComponentClient cc = ComponentClientSingleton.getInstance();
		m_startTime = System.currentTimeMillis();
		try {
			for (int i = 0; i < loop; i++)
				SBC[i] = alma.perftest.SimpleBACIComponentHelper.narrow(cc.getContainerServices().getComponent(m_deviceName));
		} catch (Exception e) {}
		m_endTime = System.currentTimeMillis();
	}

	public static void main(String[] args) {
		System.setProperty("ACS.manager", "corbaloc::localhost:3000/Manager");
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out
			.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
		String clientName = "SimpleBACIComponentClient";
		BCTClient sbcc = null;
		try {
			ComponentClientSingleton.prepareInstance(null, managerLoc, clientName);
			sbcc = new BCTClient();
			int n = 0;
//			System.out.println(sbcc.getPropertyAsync(3, 1000));
			sbcc.getPropertyAsync(256, 0);
//			sbcc.startMonitor(256);
			System.out.println("Time for 100 is: " + (sbcc.getEndTime() - sbcc.getStartTime()));
/*			sbcc.startMonitor(1000);
			sbcc.getNextMonitorResponse();
			for (int i = 0; i < 1; i++) {
				sbcc.getNextMonitorResponse();
				System.out.println("Time is: " + (sbcc.getEndTime() - sbcc.getStartTime()));
			}
			sbcc.stopMonitor();*/
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
