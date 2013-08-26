package com.cosylab.acs.maci.manager.recovery;

import java.io.Serializable;

import com.cosylab.acs.maci.HandleConstants;
import com.cosylab.acs.maci.manager.ManagerImpl;
import com.cosylab.acs.maci.manager.ManagerImpl.WhyUnloadedReason;

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
public class ComponentCommandDeallocate implements Command {

	private final int handle;
	private final int fullHandle;
	private final WhyUnloadedReason reason;
	private final boolean depreallocate;

	/**
	 * Constructor for COBCommandAckAlloc.
	 */
	public ComponentCommandDeallocate(int handle, int fullHandle, WhyUnloadedReason reason) {
		super();
		this.handle = handle;
		this.fullHandle = fullHandle;
		this.reason = reason;
		this.depreallocate = false;
	}

	public ComponentCommandDeallocate(int handle, int fullHandle, WhyUnloadedReason reason, boolean depreallocate) {
		super();
		this.handle = handle;
		this.fullHandle = fullHandle;
		this.reason = reason;
		this.depreallocate = depreallocate;
	}

	/**
	 * @see Command#execute(PrevalentSystem)
	 */
	public Serializable execute(PrevalentSystem system) throws Exception {
		((ManagerImpl)system).logHandleRelease(fullHandle, reason);
		((ManagerImpl)system).getComponents().deallocate(handle, depreallocate);
		return null;
	}

}
