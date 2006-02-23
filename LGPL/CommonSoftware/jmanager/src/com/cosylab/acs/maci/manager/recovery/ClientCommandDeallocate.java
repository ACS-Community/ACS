package com.cosylab.acs.maci.manager.recovery;

import java.io.Serializable;

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
public class ClientCommandDeallocate implements Command {
	
	private final int handle;
	
	/**
	 * Constructor for AddCOBCommand.
	 */
	public ClientCommandDeallocate(int handle) {
		super();
		this.handle = handle;
	}

	/**
	 * @see Command#execute(PrevalentSystem)
	 */
	public Serializable execute(PrevalentSystem system) throws Exception {
		((ManagerImpl)system).getClients().deallocate(handle);
		return null;
	}
}
