/*
 * @@COPYRIGHT@@
 */
package com.cosylab.acs.maci.test;

import java.util.ArrayList;

import com.cosylab.acs.maci.Administrator;
import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.ClientType;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.ContainerInfo;
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
	public TestAdministrator(String name, ClientType type, boolean monitorNotifications)
	{
		super(name, type);
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
		this(name, ClientType.ADMINISTRATOR, false);
	}

	/**
	 * Constructor for TestAdministrator.
	 * @param name
	 */
	public TestAdministrator(String name, boolean monitorNotifications)
	{
		this(name, ClientType.ADMINISTRATOR, monitorNotifications);
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#containerLoggedIn(com.cosylab.acs.maci.ContainerInfo, long, long)
	 */
	public void containerLoggedIn(ContainerInfo info, long timeStamp, long executionId) throws RemoteException
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
	 * @see com.cosylab.acs.maci.Administrator#containerLoggedOut(int, long)
	 */
	public void containerLoggedOut(int handle, long timeStamp) throws RemoteException
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
	 * @see com.cosylab.acs.maci.Administrator#clientLoggedIn(com.cosylab.acs.maci.ClientInfo, long, long)
	 */
	public void clientLoggedIn(ClientInfo info, long timeStamp, long executionId) throws RemoteException
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
	 * @see com.cosylab.acs.maci.Administrator#clientLoggedOut(int, long)
	 */
	public void clientLoggedOut(int handle , long timeStamp) throws RemoteException
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
	 * @see com.cosylab.acs.maci.Administrator#components_released(int, int, long)
	 */
	public void components_released(int[] clients, int[] components, long timeStamp) throws RemoteException
	{
		// noop
		// TODO @todo test this
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#components_requested(int, int, long)
	 */
	public void components_requested(int[] clients, int[] components, long timeStamp)
		throws RemoteException
	{
			// noop
			// TODO @todo test this
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#component_activated(com.cosylab.acs.maci.ComponentInfo, long, long)
	 */
	public void component_activated(ComponentInfo info, long timeStamp, long executionId) throws RemoteException {
		// noop
		// TODO @todo test this
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#component_deactivated(int, long)
	 */
	public void component_deactivated(int handle, long timeStamp) throws RemoteException {
		// noop
		// TODO @todo test this
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
