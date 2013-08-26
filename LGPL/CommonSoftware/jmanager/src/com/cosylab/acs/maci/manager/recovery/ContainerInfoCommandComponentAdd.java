package com.cosylab.acs.maci.manager.recovery;

import java.io.Serializable;

import com.cosylab.acs.maci.ContainerInfo;
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
public class ContainerInfoCommandComponentAdd implements Command {
	
	private final int handle;
	private final int cobHandle;
	
	/**
	 * Constructor for AddCOBCommand.
	 */
	public ContainerInfoCommandComponentAdd(int handle, int cobHandle) {
		super();
		this.handle = handle;
		this.cobHandle = cobHandle;
	}

	/**
	 * @see Command#execute(PrevalentSystem)
	 */
	public Serializable execute(PrevalentSystem system) throws Exception {
		ContainerInfo containerInfo = (ContainerInfo)((ManagerImpl)system).getContainers().get(handle);
		containerInfo.getComponents().add(cobHandle);
		return null;
	}
}
