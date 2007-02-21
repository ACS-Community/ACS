/*
 * Created on 2.10.2006
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.acs.maci.test;

import alma.maciErrType.wrappers.AcsJNoPermissionEx;

import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.Daemon;
import com.cosylab.acs.maci.Manager;
import com.cosylab.acs.maci.RemoteException;

/**
 * @author msekoranja
 *
 * @todo To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TestDaemon implements Daemon {

	private boolean alwaysFail;
	private Manager manager;
	
	public TestDaemon(Manager manager, boolean alwaysFail)
	{
		this.manager = manager;
		this.alwaysFail = alwaysFail;
	}
	
	/* (non-Javadoc)
	 * @see com.cosylab.acs.maci.Daemon#startContainer(java.lang.String, java.lang.String, short, java.lang.String)
	 */
	public void startContainer(String containerType, final String containerName,
			short instanceNumber, String flags) throws RemoteException {
		
		if (alwaysFail)
			return;
		
		new Thread(new Runnable()
		{
			public void run()
			{
				try { Thread.sleep(1000); } catch (InterruptedException e) {}
				
				TestDynamicContainer tdc = new TestDynamicContainer(containerName, manager);
				try {
					ClientInfo info = manager.login(tdc);
					tdc.setHandle(info.getHandle());
				} catch (AcsJNoPermissionEx e) {
					/// @todo Error handling when catching exceptions 
					e.printStackTrace();
				}
							}
		}, "Conatiner starter").start();
	}

}
