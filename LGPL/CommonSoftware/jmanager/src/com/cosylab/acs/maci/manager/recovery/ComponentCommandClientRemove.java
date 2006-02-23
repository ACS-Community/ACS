package com.cosylab.acs.maci.manager.recovery;

import java.io.Serializable;

import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.manager.ManagerImpl;

import org.prevayler.Command;
import org.prevayler.PrevalentSystem;

/**
 * @author dragan
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class ComponentCommandClientRemove implements Command {
	
	private final int handle;
	private final int clientHandle;
	
	/**
	 * Constructor for AddCOBCommand.
	 */
	public ComponentCommandClientRemove(int handle, int clientHandle) {
		super();
		this.handle = handle;
		this.clientHandle = clientHandle;
	}

	/**
	 * @see Command#execute(PrevalentSystem)
	 */
	public Serializable execute(PrevalentSystem system) throws Exception {
		ComponentInfo cobInfo = (ComponentInfo)((ManagerImpl)system).getComponents().get(handle);
		cobInfo.getClients().remove(clientHandle);
		return null;
	}
}
