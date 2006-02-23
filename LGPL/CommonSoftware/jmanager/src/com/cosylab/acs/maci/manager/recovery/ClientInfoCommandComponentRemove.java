package com.cosylab.acs.maci.manager.recovery;

import java.io.Serializable;

import com.cosylab.acs.maci.ClientInfo;
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
public class ClientInfoCommandComponentRemove implements Command {
	
	private final int id;
	private final int handle;
	
	/**
	 * Constructor for AddCOBCommand.
	 */
	public ClientInfoCommandComponentRemove(int id, int handle) {
		super();
		this.id = id;
		this.handle = handle;
	}

	/**
	 * @see Command#execute(PrevalentSystem)
	 */
	public Serializable execute(PrevalentSystem system) throws Exception {
		ClientInfo clientInfo = ((ManagerImpl)system).getClientInfo(id);
		clientInfo.getComponents().remove(handle);
		return null;
	}
}
