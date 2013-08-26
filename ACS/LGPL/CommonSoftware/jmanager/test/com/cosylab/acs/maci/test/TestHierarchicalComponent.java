package com.cosylab.acs.maci.test;

import java.net.URI;

import alma.maciErrType.wrappers.AcsJNoPermissionEx;

import com.cosylab.acs.maci.ComponentStatus;
import com.cosylab.acs.maci.Client;
import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.Manager;
import com.cosylab.acs.maci.RemoteException;
import com.cosylab.acs.maci.StatusHolder;

/**
 * Test implementation of hierarchical Component.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class TestHierarchicalComponent extends TestComponent
{
	
	private Manager manager;
	private Client client;
	private ClientInfo clientInfo;
	private String[] subComponents;
	private boolean passComponentHandle;
	private boolean activateOnActivation;
	
	/**
	 * Constructor for TestHierarchicalComponent.
	 * @param name
	 * @param manager
	 * @param subComponents	components to be activated as childer in construct method
	 */
	public TestHierarchicalComponent(String name, Manager manager, String[] subComponents, boolean passComponentHandle, boolean activateOnActivation)
	{
		super(name);

		assert(manager != null);
		assert(subComponents != null);
		
		this.manager = manager;
		this.subComponents = subComponents;
		this.passComponentHandle = passComponentHandle;
		this.activateOnActivation = activateOnActivation;
		
	}


	/**
	 * @see com.cosylab.acs.maci.Component#construct()
	 */
	public void construct() throws RemoteException
	{
		try
		{
			if (!activateOnActivation)
				activateSubComponents();
		} catch (Exception ex) {
			throw new RemoteException("Failed to construct(), error: " + ex.toString(), ex);
		}
	}

	/**
	 * Activates subcomponents.
	 */
	private void activateSubComponents() throws Exception {

		if (!passComponentHandle)
		{
			client = new TestClient(name+"Client");
			clientInfo = manager.login(client);
		}

		for (int i = 0; i < subComponents.length; i++)
		{

			try
			{
				StatusHolder status = new StatusHolder();
				
				if (!passComponentHandle)
					manager.getComponent(clientInfo.getHandle(), new URI(subComponents[i]), true, status);
				else
					manager.getComponent(this.getHandle(), new URI(subComponents[i]), true, status);
				
				if (status.getStatus() != ComponentStatus.COMPONENT_ACTIVATED) {
					//System.out.println("Failed to activate component '"+subComponents[i]+"' - status: "+status.getStatus()+".");
					throw new RemoteException("Failed to activate component '"+subComponents[i]+"' - status: "+status.getStatus()+".");
				}
			}
			catch (Exception ex)
			{
				//System.out.println("Failed to activate component '"+subComponents[i]+"'.");
				throw ex;
			}
		}
	}

	/**
	 * Deactivates subcomponents.
	 */
	private void deactivateSubComponents() {
		// deactivation ??!!!
	}

	/**
	 * @see com.cosylab.acs.maci.Component#destruct()
	 */
	public void destruct() throws RemoteException
	{
		if (!activateOnActivation)
			deactivateSubComponents();
		
		try {
			if (!passComponentHandle)
				manager.logout(clientInfo.getHandle());
		} catch (AcsJNoPermissionEx e) {
			/// @TODO What to do here?
			e.printStackTrace();
		}
	}

	/**
	 * @see com.cosylab.acs.maci.test.TestComponent#activate()
	 */
	public void activate() throws Exception {
		super.activate();

		if (activateOnActivation)
			activateSubComponents();
	}

	/**
	 * @see com.cosylab.acs.maci.test.TestComponent#deactivate()
	 */
	public void deactivate() {
		super.deactivate();

		if (activateOnActivation)
			deactivateSubComponents();
	}

}
