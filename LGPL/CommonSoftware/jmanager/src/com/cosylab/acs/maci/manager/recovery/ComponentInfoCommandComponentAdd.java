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
public class ComponentInfoCommandComponentAdd implements Command {
	
	private final int hid;
	private final int handle;
	
	/**
	 * Constructor for AddCOBCommand.
	 */
	public ComponentInfoCommandComponentAdd(int hid, int handle) {
		super();
		this.hid = hid;
		this.handle = handle;
	}

	/**
	 * @see Command#execute(PrevalentSystem)
	 */
	public Serializable execute(PrevalentSystem system) throws Exception {
		ComponentInfo componentInfo = (ComponentInfo)((ManagerImpl)system).getComponents().get(hid);
		componentInfo.getComponents().add(handle);
		return null;
	}
}
