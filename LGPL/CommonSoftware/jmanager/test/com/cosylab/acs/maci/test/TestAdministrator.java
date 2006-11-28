/*
 * @@COPYRIGHT@@
 */
package com.cosylab.acs.maci.test;

import java.util.ArrayList;

import com.cosylab.acs.maci.ContainerInfo;
import com.cosylab.acs.maci.Administrator;
import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.RemoteException;

/**
 * description
 * 
 * @author Jernej Kamenik
 * @version @@VERSION@@
 */
public class TestAdministrator extends TestClient implements Administrator
{

	protected ArrayList clientLoggedInNotifications = null;
	protected ArrayList clientLoggedOutNotifications = null;
	protected ArrayList containerLoggedInNotifications = null;
	protected ArrayList containerLoggedOutNotifications = null;
	
	/**
	 * Constructor for TestAdministrator.
	 * @param name
	 * @param reply
	 */
	public TestAdministrator(String name, String reply, boolean monitorNotifications)
	{
		super(name, reply);
		if (monitorNotifications) {
			clientLoggedInNotifications = new ArrayList();
			clientLoggedOutNotifications = new ArrayList();
			containerLoggedInNotifications = new ArrayList();
			containerLoggedOutNotifications = new ArrayList();
		}
	}

	/**
	 * Constructor for TestAdministrator.
	 * @param name
	 */
	public TestAdministrator(String name)
	{
		this(name, "S", false);
	}

	/**
	 * Constructor for TestAdministrator.
	 * @param name
	 */
	public TestAdministrator(String name, boolean monitorNotifications)
	{
		this(name, "S", monitorNotifications);
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#containerLoggedIn(com.cosylab.acs.maci.ContainerInfo)
	 */
	public void containerLoggedIn(ContainerInfo info) throws RemoteException
	{
		if (containerLoggedInNotifications != null)
		{
			synchronized (containerLoggedInNotifications)
			{
				// store handles...
				containerLoggedInNotifications.add(new Integer(info.getHandle()));
				containerLoggedInNotifications.notifyAll();
			}
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#containerLoggedOut(int)
	 */
	public void containerLoggedOut(int handle) throws RemoteException
	{
		if (containerLoggedOutNotifications != null)
		{
			synchronized (containerLoggedOutNotifications)
			{
				containerLoggedOutNotifications.add(new Integer(handle));
				containerLoggedOutNotifications.notifyAll();
			}
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#clientLoggedIn(com.cosylab.acs.maci.ClientInfo)
	 */
	public void clientLoggedIn(ClientInfo info) throws RemoteException
	{
		if (clientLoggedInNotifications != null)
		{
			synchronized (clientLoggedInNotifications)
			{
				clientLoggedInNotifications.add(info);
				clientLoggedInNotifications.notifyAll();
			}
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#clientLoggedOut(int)
	 */
	public void clientLoggedOut(int handle) throws RemoteException
	{
		if (clientLoggedOutNotifications != null)
		{
			synchronized (clientLoggedOutNotifications)
			{
				clientLoggedOutNotifications.add(new Integer(handle));
				clientLoggedOutNotifications.notifyAll();
			}
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#components_released(int, int)
	 */
	public void components_released(int[] clients, int[] components) throws RemoteException
	{
		// noop
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#components_requested(int, int)
	 */
	public void components_requested(int[] clients, int[] components)
		throws RemoteException
	{
			// noop
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString()
	{
		return name;
	}

	/**
	 * @return Returns the clientLoggedInNotifications.
	 */
	public ArrayList getClientLoggedInNotifications() {
		return clientLoggedInNotifications;
	}
	/**
	 * @return Returns the clientLoggedOutNotifications.
	 */
	public ArrayList getClientLoggedOutNotifications() {
		return clientLoggedOutNotifications;
	}
	/**
	 * @return Returns the containerLoggedInNotifications.
	 */
	public ArrayList getContainerLoggedInNotifications() {
		return containerLoggedInNotifications;
	}
	/**
	 * @return Returns the containerLoggedOutNotifications.
	 */
	public ArrayList getContainerLoggedOutNotifications() {
		return containerLoggedOutNotifications;
	}
}
