/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.ACS.MasterComponentImpl;

import java.util.logging.Logger;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBstringSeq;
import alma.ACS.CBstringSeqHelper;
import alma.ACS.CBstringSeqPOA;
import alma.ACS.MonitorstringSeq;
import alma.ACS.OffShoot;
import alma.ACS.ROstringSeq;
import alma.ACSErr.ACSErrTypeOK;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ACSErrTypeOK.ACSErrOK;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJCompletion;
import alma.acs.genfw.runtime.sm.AcsStateUtil;

/**
 * Convenience implementation of a subsystem state change listener. 
 * Encapsulates the activation as a callback object, the registration as a monitor of the 
 * master component state property, and the reception of state change notifications.
 * 
 * Can be subclassed (see {@link #stateChangedNotification(String[])}).
 *  
 * @author hsommer
 * created Apr 30, 2004 11:08:27 AM
 */
public class StateChangeListener extends CBstringSeqPOA
{
    private ROstringSeq statesProperty;
	private MonitorstringSeq monitor;
	private ContainerServices contSrv;
	private final MyStateChangeSemaphore stateChangeSemaphore;
	protected final Logger logger;

	
	public StateChangeListener(Logger logger)
	{
		this.logger = logger;		
		
		stateChangeSemaphore = new MyStateChangeSemaphore();
	}
	
    private class MyStateChangeSemaphore extends StateChangeSemaphore  
	{
    	public MyStateChangeSemaphore() {
    		super(logger);
		}
		synchronized void externalStateChangedNotify()  {
			stateChangedNotify();
		}
	}

	/**
	 * Creates a semaphore that can be used to wait for a given number of state changes.
	 * Useful to wait with sending the next event until a previous action state has finished
	 * its /do method, and moved on to the next state.
	 */
	public StateChangeSemaphore getStateChangeSemaphore() 
	{
		return stateChangeSemaphore;
	}
	
	
	/**
	 * Creates a monitor for <code>statesProperty</code> from this instance.
	 * 
	 * @param statesProperty
	 * @param contSrv
	 * @return  usually not needed
	 * @throws Exception
	 */
	public MonitorstringSeq createMonitor(ROstringSeq statesProperty, ContainerServices contSrv) throws Exception
	{
		this.contSrv = contSrv;
		
		if (monitor != null) {
			destroyMonitor();
		}
		
		this.statesProperty = statesProperty;
		
		// register this callback object with CORBA
		OffShoot offshoot = contSrv.activateOffShoot(this);
		
		CBstringSeq cbStringSeq = CBstringSeqHelper.narrow(offshoot);
		
		// register callback CORBA object with the statesProperty as a monitor  
		monitor = statesProperty.create_monitor(cbStringSeq, new CBDescIn());
		
		// baci.idl: "On creation, the only trigger present will be the timer trigger. 
		// Calling the set_value_trigger method determines the behaviour of the value trigger. 
		// The enable parameter determines whether the value trigger is active or not ." 
		// Strings have no triggerable value, thus don't care 
		monitor.set_value_trigger(new String[0], true);
		// baci.idl: "Timer trigger can be disabled by passing the value 0 for timer parameter." 
		monitor.set_timer_trigger(0);
		
		return monitor;
	}
	
	/**
	 * 
	 * @throws Exception
	 */
	public void destroyMonitor() throws Exception
	{
		if (monitor != null) {
			monitor.destroy();
			monitor = null;
			contSrv.deactivateOffShoot(this);
		}
	}
	
	
	/**
	 * @see alma.ACS.CBstringSeqOperations#working(java.lang.String[], alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public final void working(String[] value, Completion completion, CBDescOut desc)
	{
		stateChangeSemaphore.externalStateChangedNotify();
		stateChangedNotification(value);
	}

	
	/**
	 * Subclasses may override this and do something useful with the new state...
	 * @param newState
	 */
	protected void stateChangedNotification(String[] newStateHierarchy)
	{
		logNotification(newStateHierarchy, null);
	}
	
	/**
	 * @see alma.ACS.CBstringSeqOperations#done(java.lang.String[], alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public void done(String[] value, Completion completion, CBDescOut desc)
	{
		logger.fine("Component property 'hierarchicalState' has been released (callback method 'done' was called).");
	}

	
	/**
	 * Logs a state change notification.
	 * @param value  the new state hierarchy
	 * @param completion  an optional completion 
	 */
	protected void logNotification(String[] value, Completion completion)
	{
		String msg = "hierarchical state = '";
		for (int i = 0; i < value.length; i++) {
			msg += value[i];
			if (i < value.length -1) {
				msg += '/';
			}
		}
		msg += "'. ";
		
		if (completion != null) {
			msg += "Completion=";
			AcsJCompletion compl = AcsJCompletion.fromCorbaCompletion(completion);
			if (compl.isError()) {
				msg += compl.getAcsJException().toString();
			}
			else {
				msg += "ok";
			}
		}
		
		logger.finer(msg);
	}


	/**
	 * @see alma.ACS.CallbackOperations#negotiate(long, alma.ACS.CBDescOut)
	 */
	public boolean negotiate(long timeout, CBDescOut desc)
	{
		return false;
	}


	/**
	 * Reads the current state hierarchy.
	 * @return State hierarchy with outmost state first
	 * @throws AcsJIllegalStateEventEx if the state can't be read ; @TODO: use better fitting ex (don't want to create one now right before the release)
	 */
	public String[] getCurrentState() throws AcsJIllegalStateEventEx { 
		CompletionHolder ch = new CompletionHolder();
		String[] statesHierarchy = statesProperty.get_sync(ch);
		
		AcsJCompletion statesSyncCompletion = AcsJCompletion.fromCorbaCompletion(ch.value);
		if (statesSyncCompletion.isError() ||  
		        statesSyncCompletion.getType() != ACSErrTypeOK.value ||
		        statesSyncCompletion.getCode() != ACSErrOK.value ||
		        statesHierarchy == null ) {		 
		   throw new AcsJIllegalStateEventEx("Failed to retrieve current subsystem state.");
		}
		
		return statesHierarchy;
	}	

	
	/**
	 * Helper method for the repeated task of getting the current state hierarchy and 
	 * comparing it against the expected hierarchy.
	 * @return true if the current state hierarchy is equal to <code>expectedHierarchy</code>.
	 */
	public boolean verifyCurrentState(String[] expectedHierarchy) 
	{ 
	    boolean ret = false;
	    
	    String[] actualHierarchy = null;
	    try {
	    	actualHierarchy = getCurrentState();
			String expectedPath = AcsStateUtil.stateHierarchyNamesToString(expectedHierarchy);
			String actualPath = AcsStateUtil.stateHierarchyNamesToString(actualHierarchy);
			if (actualPath.equals(expectedPath)) {
				ret = true;
				//logger.info("current state hierarchy '" + actualPath + "' as expected"); 
			}
			else {
				logger.info("current state hierarchy '" + actualPath + "' differs from expected '" + expectedPath + "'.");
			}	    	
	    }
	    catch (Exception ex) {
	    	; // ret=false
	    }
		return ret;
	}		

}
