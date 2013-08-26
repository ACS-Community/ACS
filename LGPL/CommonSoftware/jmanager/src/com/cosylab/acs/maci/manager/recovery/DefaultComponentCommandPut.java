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
public class DefaultComponentCommandPut implements Command {

	private final String type;
	private final ComponentInfo componentInfo;

	/**
	 */
	public DefaultComponentCommandPut(String type, ComponentInfo componentInfo) {
		super();
		this.type = type;
		this.componentInfo = componentInfo;
	}

	/**
	 * @see Command#execute(PrevalentSystem)
	 */
	public Serializable execute(PrevalentSystem system) throws Exception {
		((ManagerImpl) system).getDefaultComponents().put(type, componentInfo);
		return null;
	}

}