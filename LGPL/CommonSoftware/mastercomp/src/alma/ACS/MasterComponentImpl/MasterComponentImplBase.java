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

import java.util.logging.Level;

import alma.ACS.MasterComponentOperations;
import alma.ACS.ROstringSeq;
import alma.ACS.ROstringSeqHelper;
import alma.ACS.ROstringSeqPOATie;
import alma.ACS.MasterComponentImpl.statemachine.AlmaSubsystemActions;
import alma.ACS.MasterComponentImpl.statemachine.AlmaSubsystemContext;
import alma.ACS.MasterComponentPackage.SubsystemStateEvent;
import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.ROstringSeqImpl;
import alma.ACS.jbaci.DataAccess;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.IllegalStateEventEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJCompletion;
import alma.acs.exceptions.AcsJException;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.genfw.runtime.sm.AcsStateChangeListener;
import alma.acs.genfw.runtime.sm.AcsStateIllegalEventException;
import alma.acs.genfw.runtime.sm.AcsStateUtil;

/**
 * Base class for subsystem master components.
 * <p>
 * Each ALMA subsystem ('subsystem' in its technical meaning) must provide such a 
 * master component for administrational purposes.
 * It's strongly recommended to do this in Java and use <code>MasterComponentImplBase</code>
 * as the base class for the component implementation. 
 * <p>
 * This class provides the transition logic for the various states.
 * Subclasses only need to implement the actions for the various transitions
 * or, more accurately, for the activity states.
 * (see method {@link #getActionHandler() getActionHandler}; 
 * as an example, please see {@link alma.ACS.MasterComponentImpl.TestMasterComponentImpl TestMasterComponent}.
 *     
 * @author hsommer
 * created Mar 16, 2004 3:02:37 PM
 */
public abstract class MasterComponentImplBase extends CharacteristicComponentImpl 
		implements MasterComponentOperations, AcsStateChangeListener
{
	/**
	 * all nested states contained in one property for atomic access, 
	 * listed from top-level state down to substates.
	 */
	private ROstringSeq m_currentStateHierarchy;

	private DataAccess m_currentStateHierarchyDataAccess;

	private AlmaSubsystemContext m_stateMachine;


	/******************* [ Lifecycle implementations ] *******************/
	
	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException {

		super.initialize(containerServices);
		
		try
		{
			ROstringSeqImpl currentStateHierarchyImpl = new ROstringSeqImpl("currentStateHierarchy", this);
			m_currentStateHierarchyDataAccess = currentStateHierarchyImpl.getDataAccess();
			ROstringSeqPOATie currentStateHierarchyTie = new ROstringSeqPOATie(currentStateHierarchyImpl);
			m_currentStateHierarchy = ROstringSeqHelper.narrow(this.registerProperty(currentStateHierarchyImpl, currentStateHierarchyTie));
		}
		catch (Throwable th)
		{
			throw new ComponentLifecycleException("Failed to create ACS property for nested component states.", th); 
		}
		
		AlmaSubsystemActions actionHandler = getActionHandler();
		if (actionHandler == null) {
			throw new ComponentLifecycleException("Action handler was null. Check implementation of method getActionHandler in concrete master component implementation class.");
		}
		m_stateMachine = new AlmaSubsystemContext(actionHandler, m_logger);
		m_stateMachine.addAcsStateChangeListener(this);

		try {
			updateStateHierarchy();
		}
		catch (AcsJException e) {
			throw new ComponentLifecycleException("failed to initialize state property", e);
		}
	}

	
	
	/*********************** [ MasterComponent interface ] ***********************/

	/**
	 * @see alma.ACS.MasterComponentOperations#currentStateHierarchy()
	 * @see #updateStateHierarchy()
	 */
	public final ROstringSeq currentStateHierarchy() {
		return m_currentStateHierarchy;
	}
	
	/**
	 * Dispatches the event to the corresponding state machine method.
	 * <p>
	 * Relies on CORBA to queue concurrent calls, which this synchronized implementation forcefully serializes. 
	 * Note that calls should always return quickly, since the real work is done asynchronously in activity states.
	 * 
	 * @see alma.ACS.MasterComponentOperations#doTransition(alma.ACS.MasterComponentPackage.SubsystemStateEvent)
	 */
	public final synchronized void doTransition(SubsystemStateEvent event)
			throws IllegalStateEventEx 
	{
		if (event == null) {
			throw (new AcsJIllegalStateEventEx("Paramter 'event' must not be null.")).toIllegalStateEventEx();
		}
		
		String eventName = event2Name(event);
		m_logger.fine("Received event " + eventName);
		
		try {
			switch (event.value()) {
				case SubsystemStateEvent._SUBSYSEVENT_INITPASS1 :
					m_stateMachine.initPass1();
				break;
				case SubsystemStateEvent._SUBSYSEVENT_INITPASS2 :
					m_stateMachine.initPass2();
				break;
				case SubsystemStateEvent._SUBSYSEVENT_REINIT :
					m_stateMachine.reinit();
				break;
				case SubsystemStateEvent._SUBSYSEVENT_START :
					m_stateMachine.start();
				break;
				case SubsystemStateEvent._SUBSYSEVENT_STOP :
					m_stateMachine.stop();
				break;
				case SubsystemStateEvent._SUBSYSEVENT_SHUTDOWNPASS1 :
					m_stateMachine.shutdownPass1();
				break;
				case SubsystemStateEvent._SUBSYSEVENT_SHUTDOWNPASS2 :
					m_stateMachine.shutdownPass2();
				break;
				case SubsystemStateEvent._SUBSYSEVENT_ERROR :
					m_stateMachine.error();
				break;

				default :
					m_logger.warning("unexpected SubsystemStateEvent with integer value " + event.value() +
							" found; most likely " + MasterComponentImplBase.class.getName() + " must be updated.");
			}
		}
		catch (AcsStateIllegalEventException ex) {
			m_logger.log(Level.WARNING, "Illegal event.", ex);
			AcsJIllegalStateEventEx jex = new AcsJIllegalStateEventEx(ex);
			throw jex.toIllegalStateEventEx();
		}
	}
	

	/*********************** [ to be implemented by subclasses ] ***********************/
	
	/**
	 * Subsystem state machine actions, to be implemented by concrete subclasses.
	 */
	protected abstract AlmaSubsystemActions getActionHandler();

	
	/*********************** [ miscellaneous ] ***********************/
	
	/**
	 * Will be called by the state machine implementation for every state change.
	 * <p>
	 * Normally you should not overload this method; if you do, make sure to call {@link #updateStateHierarchy()} 
	 * so that the state change propagates to the field which is visible from the outside of this component.
	 * 
	 * @see alma.ACS.MasterComponentImpl.statemachine.AcsStateChangeListener#stateChangedNotify(alma.ACS.MasterComponentImpl.statemachine.AcsState[], alma.ACS.MasterComponentImpl.statemachine.AcsState[])
	 */
	public void stateChangedNotify(AcsState[] oldStateHierarchy, AcsState[] currentStateHierarchy) {
		try {
			updateStateHierarchy();
		}
		catch (Throwable thr) {
			// todo: figure out something better 
			m_logger.log(Level.WARNING, "failed to update the state hierarchy -- current subsystem state might not be given correctly.", thr);
		}
		String oldHi = AcsStateUtil.stateHierarchyToString(oldStateHierarchy);
		String newHi = AcsStateUtil.stateHierarchyToString(currentStateHierarchy);
		m_logger.info("subsystem state has changed from " + oldHi + " to " + newHi);
	}

	/**
	 * sets the property value of <code>currentStateHierarchy</code>
	 * to match the current (sub-)states from the state machine.
	 */
	public void updateStateHierarchy() throws AcsJException 
	{
		AcsState[] stateHierarchy = m_stateMachine.getCurrentTopLevelState().getStateHierarchy();
		String newState = AcsStateUtil.stateHierarchyToString(stateHierarchy);
		
		// convert to String[]
		String[] stateNameHierarchy = new String[stateHierarchy.length];
		for (int i = 0; i < stateHierarchy.length; i++) {
			stateNameHierarchy[i] = stateHierarchy[i].stateName();
		}
		// set Baci property
		CompletionHolder ch = new CompletionHolder();
		m_currentStateHierarchyDataAccess.set(stateNameHierarchy, ch);		
		
		// optimization usually leaves out the completion if ok
		if (ch.value != null) {
			AcsJCompletion compl = AcsJCompletion.fromCorbaCompletion(ch.value);
			if (compl.isError()) {
				m_logger.log(Level.WARNING, "failed to update state property!", compl.getAcsJException());
				throw compl.getAcsJException();
			}
			else {
				m_logger.finest("Changed state property to '" + newState + "'.");
			}
		}
		else {
			m_logger.finest("Changed state property to '" + newState + "'.");
		}
	}
	
	
	/**
	 * Returns an "inofficial" string version of a <code>SubsystemStateEvent</code>.
	 * This can be used for logging etc. Note that in IDL, the enum does not define a string representation.
	 * @param event  a subsystem state event as declared in IDL
	 * @return informal name of the event, or <code>null</code> if the event is unknown.
	 */
	static String event2Name(SubsystemStateEvent event) {
		if (event == null) {
			return null;
		}
		switch (event.value()) {
			case SubsystemStateEvent._SUBSYSEVENT_INITPASS1:
				return "initpass1";
			case SubsystemStateEvent._SUBSYSEVENT_INITPASS2:
				return "initpass2";
			case SubsystemStateEvent._SUBSYSEVENT_REINIT:
				return "reinit";
			case SubsystemStateEvent._SUBSYSEVENT_START:
				return "start";
			case SubsystemStateEvent._SUBSYSEVENT_STOP:
				return "stop";
			case SubsystemStateEvent._SUBSYSEVENT_SHUTDOWNPASS1:
				return "shutdownpass1";
			case SubsystemStateEvent._SUBSYSEVENT_SHUTDOWNPASS2:
				return "shutdownpass2";
			case SubsystemStateEvent._SUBSYSEVENT_ERROR:
				return "error";
			default: return null;
		}
	}
}
