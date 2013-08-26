/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.manager;

import java.util.TimerTask;

import com.cosylab.acs.maci.Client;
import com.cosylab.acs.maci.ClientInfo;

/**
 * <code>ClientInfo</code> class containing <code>TimerTask<code> object.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class TimerTaskClientInfo extends ClientInfo
{
	
	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -114967372164415230L;

	transient private TimerTask task = null;

	/**
	 * Constructor for PingableClientInfo.
	 * @param handle
	 * @param name
	 * @param client
	 */
	public TimerTaskClientInfo(int handle, String name, Client client)
	{
		super(handle, name, client);
	}

	/**
	 * Returns the task.
	 * @return TimerTask
	 */
	public TimerTask getTask()
	{
		return task;
	}

	/**
	 * Sets the task.
	 * @param task The task to set
	 */
	public void setTask(TimerTask task)
	{
		this.task = task;
	}

}
