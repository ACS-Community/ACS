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
public class ClientInfoCommandComponentAdd implements Command {
	
	private final int id;
	private final int handle;
	
	/**
	 * Constructor for AddCOBCommand.
	 */
	public ClientInfoCommandComponentAdd(int id, int handle) {
		super();
		this.id = id;
		this.handle = handle;
	}

	/**
	 * @see Command#execute(PrevalentSystem)
	 */
	public Serializable execute(PrevalentSystem system) throws Exception {

		/*
		// parse handle part
		int handle	= id & HandleConstants.HANDLE_MASK;

		// info to be returned
		ClientInfo info = null;
		
		switch	(id & HandleConstants.TYPE_MASK)
		{
			case HandleConstants.CLIENT_MASK:
				info = (ClientInfo)((ManagerImpl)system).getClients().get(handle);
				break;

			case HandleConstants.ADMINISTRATOR_MASK:
				info = (ClientInfo)((ManagerImpl)system).getAdministrators().get(handle);
				break;
		}

		if (info != null)
			info.getComponents().add(handle);
		 */

		ClientInfo clientInfo = ((ManagerImpl)system).getClientInfo(id);
		clientInfo.getComponents().add(handle);
		return null;
	}
}
