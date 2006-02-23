/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.manager;

import java.util.TimerTask;
import java.util.logging.Level;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.core.defaults.MessageLogEntry;
import abeans.core.defaults.ExceptionIgnorer;
import abeans.pluggable.acs.logging.LoggingLevel;

import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.HandleHelper;
import com.cosylab.acs.maci.Manager;
import com.cosylab.acs.maci.RemoteTimeoutException;
import com.cosylab.acs.maci.RemoteTransientException;

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
public class PingTimerTask extends TimerTask implements Identifiable
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
	private Manager manager;

	/**
	 * Monitored client's info.
	 */
	private ClientInfo clientInfo;

	/**
	 * Identifier.
	 */
	private transient Identifier id = null;

	/**
	 * Constructs a ping task which monitors client's state.
	 * @param	manager	manager to which the client is logged in
	 * @param	clientInfo	info of the client to be monitored
	 */
	public PingTimerTask(Manager manager, ClientInfo clientInfo)
	{
		super();
		
		assert (manager != null);
		assert (clientInfo != null);
		
		this.manager = manager;
		this.clientInfo = clientInfo;

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "PingTimerTask", new Object[] { 
				manager == null ? "null" : manager.toString(), 
				clientInfo == null ? "null" : clientInfo.toString() }).dispatch(); 

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
			if (isDebug())
				new MessageLogEntry(this, "logout", new Object[0]).dispatch(); 
	
			// cancel this task
			cancel();
			
			// and logout
			manager.logout(clientInfo.getHandle());
	
			if (isDebug())
				new MessageLogEntry(this, "logout", "Exiting.", Level.FINEST).dispatch();
				
		}
		catch (Exception ex)
		{
		}
	}

	/**
	 * Override cancel metod adding trace logs.
	 */
	public boolean cancel()
	{
		boolean retVal = super.cancel();
		// do not throw any exceptions here...
		
		// TODO it would be nice to release (set to null) clientInfo reference here
		// but be careful when setting it to null

		try
		{
			if (isDebug())
				new MessageLogEntry(this, "cancel", new Object[0]).dispatch(); 
	
		}
		catch (Exception ex) {}

		return retVal;
	}

	/**
	 * @see java.lang.Runnable#run()
	 */
	public void run()
	{
		try
		{
			
			if (isDebug())
				new MessageLogEntry(this, "run", "Invoking ping on "+HandleHelper.toString(clientInfo.getHandle())+"].", LoggingLevel.DEBUG).dispatch();

			// malfunctioning client check
			if (clientInfo.getClient().ping() == false)
			{
				if (isDebug())
					new MessageLogEntry(this, "run", "Client '"+clientInfo.getName()+"' ["+HandleHelper.toString(clientInfo.getHandle())+"] announced itself as malfunctioning.", LoggingLevel.INFO).dispatch();
					
				logout();
			}
			
			// reset transientCount to zero
			transientCount = 0;
		}
		catch (RemoteTransientException rte)
		{
			if (isDebug())
				new MessageLogEntry(this, "run", "Invoking client '"+clientInfo.getName()+"' ["+HandleHelper.toString(clientInfo.getHandle())+"] ping method thrown transient exception.", rte, LoggingLevel.INFO).dispatch();

			// client not reachable
			transientCount++;
			if (transientCount >= MAX_TRANSIENT_COUNT)
				logout();
		
		}
		catch (RemoteTimeoutException rtoe)
		{
			if (isDebug())
				new MessageLogEntry(this, "run", "Invoking client '"+clientInfo.getName()+"' ["+HandleHelper.toString(clientInfo.getHandle())+"] ping method thrown timeout exception.", rtoe, LoggingLevel.INFO).dispatch();

			// client not reachable
			transientCount++;
			if (transientCount >= MAX_TRANSIENT_COUNT)
				logout();
		
		}
		catch (Exception ex)
		{
                        if (isDebug())
                                new MessageLogEntry(this, "run", "Invoking client '"+clientInfo.getName()+"' ping method thrown an exception, logging it out.", ex, LoggingLevel.INFO).dispatch();
			// we handled from exception
			new ExceptionIgnorer(ex);

			// malfunctioning client
			logout();
		}

	}

	/*************************** [ Abeans methods ] ******************************/

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		if (id == null)
			id = new IdentifierSupport("PingTimerTask for '"+clientInfo.getName()+"' ["+HandleHelper.toString(clientInfo.getHandle())+"]",
										"PingTimerTask", Identifier.PLUG);
		
		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return true;
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

}
