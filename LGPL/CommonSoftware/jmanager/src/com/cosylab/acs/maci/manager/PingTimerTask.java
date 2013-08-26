/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.manager;

import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.HandleHelper;
import com.cosylab.acs.maci.RemoteTimeoutException;
import com.cosylab.acs.maci.RemoteTransientException;

import alma.acs.alarmsystem.source.AlarmSource;
import alma.alarmsystem.source.ACSFaultState;

/**
 * Implementation of ping task executed by <code>java.util.Timer</class>.
 * 
 * Manager pings its clients (both GUI clients, as well as Containers) repeatedly to verify that they still exist.
 * The return value of <code>Client#ping()</code> can be either "true", indicating that everything is OK with the client,
 * or "false", indicating that client is malfunctioning.
 * 
 * If <code>RemoteTransientException</code> or <code>RemoteTimeoutException</code> exception is thrown, the Manager should retry the ping several times,
 * and only then shall the client be assumed to be malfunctioning.
 * If another exception is thrown, the client may be immediately assumed to be malfunctioning.
 * Once the client is found to be malfunctioning, the Manager makes an implicit logout of the client.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class PingTimerTask extends TimerTask 
{

	/**
	 * Maximum number of consequential code>RemoteTransientException</code>
	 * exception catches logging the client out.
	 */
	private static final int MAX_TRANSIENT_COUNT = 3;

	/**
	 * Counter for consequential <code>RemoteTransientException</code>
	 * exception catches to <code>Client#ping</code> method.
	 */
	private int transientCount;

	/**
	 * Manager to which the client is logged in.
	 */
	private ManagerImpl manager;

	/**
	 * Monitored client's info.
	 */
	private ClientInfo clientInfo;

	/**
	 * Logger.
	 */
	private Logger logger;
	
	/**
	 * Alarm System Interface.
	 */
	private AlarmSource alarmSource;

	/**
	 * Constructs a ping task which monitors client's state.
	 * @param	manager	manager to which the client is logged in
	 * @param 	logger logger.
	 * @param	clientInfo	info of the client to be monitored
	 * @param	alarmSource	interface to send alarms
	 * @param	activeAlarm	set of active alarms
	 */
	public PingTimerTask(ManagerImpl manager, Logger logger, ClientInfo clientInfo, AlarmSource alarmSource)
	{
		super();
		
		assert (manager != null);
		assert (logger != null);
		assert (clientInfo != null);
		
		this.manager = manager;
		this.logger = logger;
		this.clientInfo = clientInfo;
		this.alarmSource = alarmSource;
		
		this.transientCount = 0;
	}

	/**
	 * Terminates this task and logs the client out.
	 */
	private void logout()
	{
		// do not throw any exceptions here...
		try
		{
			// cancel this task
			cancel();
			
			// and logout
			manager.logout(clientInfo.getHandle());
	
		}
		catch (Throwable th)
		{
			// noop
		}
	}

	/**
	 * @see java.lang.Runnable#run()
	 */
	public void run()
	{
		try
		{
			
			logger.finest("Invoking ping on "+HandleHelper.toString(clientInfo.getHandle())+"].");

			// malfunctioning client check
			if (clientInfo.getClient().ping() == false)
			{
				logger.info("Client '"+clientInfo.getName()+"' ["+HandleHelper.toString(clientInfo.getHandle())+"] announced itself as malfunctioning.");

				// An alarm is raised for those clients that define an alarm interface
				raise_alarm(clientInfo.getName());

				logout();
			}
			
			// reset alarm state, if neccesary
			clear_alarm(clientInfo.getName());
			
			// reset transientCount to zero
			transientCount = 0;
		}
		catch (RemoteTransientException rte)
		{
			//logger.log(Level.INFO, "Invoking client '"+clientInfo.getName()+"' ["+HandleHelper.toString(clientInfo.getHandle())+"] ping method thrown transient exception.", rte);

			// client not reachable
			transientCount++;
			if (transientCount >= MAX_TRANSIENT_COUNT)
			{
				logger.info("Client '"+clientInfo.getName()+"' ["+HandleHelper.toString(clientInfo.getHandle())+"] is unreachable, logging it out.");

				// An alarm is raised for those clients that define an alarm interface
				raise_alarm(clientInfo.getName());

				logout();
			}
		
		}
		catch (RemoteTimeoutException rtoe)
		{
			//logger.log(Level.INFO, "Invoking client '"+clientInfo.getName()+"' ["+HandleHelper.toString(clientInfo.getHandle())+"] ping method thrown timeout exception.", rtoe);
			
			// client not reachable
			transientCount++;
			if (transientCount >= MAX_TRANSIENT_COUNT)
			{
				logger.info("Client '"+clientInfo.getName()+"' ["+HandleHelper.toString(clientInfo.getHandle())+"] ping method timed-out several times, logging it out.");

				// An alarm is raised for those clients that define an alarm interface
				raise_alarm(clientInfo.getName());

				logout();
			}
		
		}
		catch (Throwable ex)
		{
			logger.log(Level.INFO, "Invoking client '"+clientInfo.getName()+"' ping method thrown an exception, logging it out.", ex);
			//logger.info("Invoking client '"+clientInfo.getName()+"' ping method threw an unknown exception, logging it out.");

			// An alarm is raised for those clients that define an alarm interface
			raise_alarm(clientInfo.getName());

			// malfunctioning client
			logout();
		}

	}

	/**
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("PingTimerTask = { ");
		sbuff.append("manager = '");
		sbuff.append(manager);
		sbuff.append(", clientInfo = '");
		sbuff.append(clientInfo);
		sbuff.append(", transientCount = '");
		sbuff.append(transientCount);
		sbuff.append("' }");
		return new String(sbuff);
	}

	// ALARM SYSTEM codes
	
	protected final static String FAULT_FAMILY = "Manager";
	protected  final static int FAULT_CODE = 1;

	/**
	 * Raise alarm.
	 *
	 * @param faultMember
	 */
	private void raise_alarm(String faultMember) {
		if (!manager.hasActiveAlarm(clientInfo.getName()))
			send_alarm(faultMember, true);
	}

	/**
	 * Clear alarm.
	 *
	 * @param faultMember
	 */
	private void clear_alarm(String faultMember) {
		if (manager.hasActiveAlarm(clientInfo.getName()))
			send_alarm(faultMember, false);
	}

	/**
	 * Convenience method for send_alarm with given state.
	 *
	 * @param faultMember
	 * @param state
	 */
	private void send_alarm(String faultMember, boolean raise) {
		
		// if no alarm system initialized ignore
		if (alarmSource == null) {
			return;
		}

		try {
			alarmSource.setAlarm(FAULT_FAMILY, faultMember, FAULT_CODE, raise);

			// save alarm state
			if (raise) {
				manager.alarmRaised(faultMember);
			}
			else {
				manager.alarmCleared(faultMember);
			}
			
		} catch (Throwable th) {
			// do nothing, alarm did not work
		}
	}
}
