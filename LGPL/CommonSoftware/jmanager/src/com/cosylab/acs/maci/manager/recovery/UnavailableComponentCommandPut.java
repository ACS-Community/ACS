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
public class UnavailableComponentCommandPut implements Command {

	private final String name;
	private final ComponentInfo cobInfo;

	/**
	 */
	public UnavailableComponentCommandPut(String name, ComponentInfo cobInfo) {
		super();
		this.name = name;
		this.cobInfo = cobInfo;
	}

	/**
	 * @see Command#execute(PrevalentSystem)
	 */
	public Serializable execute(PrevalentSystem system) throws Exception {
		((ManagerImpl) system).getUnavailableComponents().put(name, cobInfo);
		return null;
	}

}