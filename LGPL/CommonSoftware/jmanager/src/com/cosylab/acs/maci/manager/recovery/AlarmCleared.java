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
public class AlarmCleared implements Command {

	private final String faultMember;
	
	/**
	 * Constructor for AlarmRaised.
	 */
	public AlarmCleared(String faultMember) {
		super();
		this.faultMember = faultMember;
	}

	/**
	 * @see Command#execute(PrevalentSystem)
	 */
	public Serializable execute(PrevalentSystem system) throws Exception {
		return ((ManagerImpl)system).getActiveAlarms().remove(faultMember);
	}

}
