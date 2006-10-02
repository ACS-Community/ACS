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

import java.util.LinkedList;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ACS.ACSComponent;
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

	private SubsysResourceMonitor subsysComponentMonitor;
	
	private StateChangeNotificationChecker stateChangeNotificationChecker; 
	
    
	/******************* [ Lifecycle implementations ] *******************/
	
	/**
	 * Master component subclasses must call <code>super.initialize()</code> if they override this method!
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException {

		super.initialize(containerServices);
		subsysComponentMonitor = new SubsysResourceMonitor(m_logger);
		
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

		
		if (StateChangeNotificationChecker.monitorStateChangeNotification) {
			try {
				stateChangeNotificationChecker = new StateChangeNotificationChecker(m_logger);
				stateChangeNotificationChecker.createMonitor(m_currentStateHierarchy, m_containerServices);
			} catch (Exception e) {
				m_logger.log(Level.WARNING, "Master component " + name() + 
						" failed to monitor its state change notification although this was requested by the property '" + StateChangeNotificationChecker.PROPERTYNAME + "'.", e);
			}
		}
		
		try {
			updateStateHierarchy();
		}
		catch (AcsJException e) {
			throw new ComponentLifecycleException("failed to initialize state property", e);
		}
	}
	

	
	/**
	 * Master component subclasses must call <code>super.cleanUp()</code> if they override this method!
	 * @see alma.ACS.impl.CharacteristicComponentImpl#cleanUp()
	 */
	public void cleanUp() {
		try {
			if (stateChangeNotificationChecker != null) {
				stateChangeNotificationChecker.destroyMonitor();
			}
		} catch (Exception e) {
			m_logger.log(Level.WARNING, "Failed to destroy the monitor object that checks state change notification", e);
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
			AcsJIllegalStateEventEx ex = new AcsJIllegalStateEventEx();
			ex.setEvent("null");
			throw ex.toIllegalStateEventEx();
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
		catch (AcsJIllegalStateEventEx ex) {
			m_logger.log(Level.WARNING, "Illegal event '" + ex.getEvent() + "' received in state '" + ex.getState() + "'.", ex);
			throw ex.toIllegalStateEventEx();
		}
	}
	

	/*********************** [ to be implemented by subclasses ] ***********************/
	
	/**
	 * Subsystem state machine actions, to be implemented by concrete subclasses.
	 */
	protected abstract AlmaSubsystemActions getActionHandler();

	
	/*********************** [ miscellaneous ] ***********************/
	
	/**
	 * Subclasses can request to have the "health" of a given subsystem component monitored,
	 * and that the mastercomponent state be set to "Error" in case problems are detected with this component.
	 * Note that this is a simplified version of {@link #monitorComponent(ACSComponent, ComponentErrorHandler)},
	 * which does not require the subclass to provide an error handler.
	 * @since ACS 6.0
	 */
	protected final void monitorComponent(ACSComponent component) {
		monitorComponent(component, null);
	}
	
	/**
	 * Subclasses can request to have the "health" of a given subsystem component monitored,
	 * and that the provided <code>ResourceErrorHandler</code> be called in case problems are detected.
	 * @param component  the component that should be monitored
	 * @param err  a custom error handler. If <code>null</code>, then default error handler will be used,
     *             and component will go to error state automatically.
	 * @since ACS 6.0
	 */
	protected final <T extends ACSComponent> void monitorComponent(T component, SubsysResourceMonitor.ResourceErrorHandler<T> err) {
		if (component == null) {
			throw new NullPointerException("component must not be null");
		}
		
		SubsysResourceMonitor.ResourceChecker<T> checker = new SubsysResourceMonitor.ComponentChecker<T>(component);
		monitorResource(checker, err);
	}
	
	
	/**
     * Subclasses can request to have the "health" of a given subsystem component monitored,
     * and that the provided <code>ResourceErrorHandler</code> be called in case problems are detected.
     * @param component  the component that should be monitored
     * @param err  a custom error handler. If <code>null</code>, then default error handler will be used,
     *             and component will go to error state automatically.
	 * @param <T>  The resource type 
	 * @param checker  the checker that encapsulates the specifics of the methods to be checked.
	 * @param err  custom error handler for this type of resource, or default handler if <code>null</code> is passed.
     * @since ACS 6.0
	 */
	protected final <T> void monitorResource(SubsysResourceMonitor.ResourceChecker<T> checker, SubsysResourceMonitor.ResourceErrorHandler<T> err) {
        if (err == null) {
            // use default error handler that sets the subsystem state to ERROR
            err = new SubsysResourceMonitor.ResourceErrorHandler<T>() {
                public void resourceUnreachable(T resource) {
                    try {
                        m_logger.log(Level.INFO, "Found unreachable component, will go to ERROR state.");
                        doTransition(SubsystemStateEvent.SUBSYSEVENT_ERROR);
                    } catch (Throwable thr) {
                        m_logger.log(Level.WARNING, "exception caught while going to ERROR state.", thr);
                    }
                }
                public void badState(T resource, String stateName) {
                    try {
                        m_logger.log(Level.INFO, "Found component in bad state '" + stateName + "', will go to ERROR state.");
                        doTransition(SubsystemStateEvent.SUBSYSEVENT_ERROR);
                    } catch (Throwable thr) {
                        m_logger.log(Level.WARNING, "exception caught while going to ERROR state.", thr);
                    }
                }
            };
        }
		subsysComponentMonitor.monitorResource(checker, err);
	}
	
	
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
	 * Sets the property value of <code>currentStateHierarchy</code>
	 * to match the current (sub-)states from the state machine.
	 * <p>
	 * This method should not be overloaded by subclasses! We just don't make it final to meet special testing needs. 
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
				
		// set Baci property and announce the notification to the checker object beforehand if checking is enabled
		if (StateChangeNotificationChecker.monitorStateChangeNotification) {
			stateChangeNotificationChecker.announceStateChangeNotification(stateNameHierarchy);
		}
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
	
	
	/**
	 * This class can be used by tests to compare the externally visible state change notifications
	 * that are sent by the ROstringSeq Baci property with the internally triggered notifications.
	 * To enable this feature, the property <code>acs.mastercomp.monitorStateChangeNotification</code>
	 * must be set to <code>true</code>.
	 * <p>
	 * We suspect that in some cases the baci property fires too often, which would be documented with a SEVERE log.
	 */
	private static class StateChangeNotificationChecker extends StateChangeListener {
		
		public static final String PROPERTYNAME = "acs.mastercomp.monitorStateChangeNotification";
		public static final boolean monitorStateChangeNotification = Boolean.getBoolean(PROPERTYNAME);
		
		private LinkedList<String> announcedStateNameHierarchy = new LinkedList<String>();
		
		StateChangeNotificationChecker(Logger logger) {
			super(logger);
		}

		synchronized void announceStateChangeNotification(String[] stateNameHierarchy) {	
			String statesFlat = AcsStateUtil.stateHierarchyNamesToString(stateNameHierarchy);
			announcedStateNameHierarchy.add(statesFlat);
			logger.finest("StateChangeNotificationChecker: jbaci notification for '" + statesFlat + "' is announced.");
		}
		
		protected synchronized void stateChangedNotification(String[] newStateHierarchy) {
			String newStatesFlat = AcsStateUtil.stateHierarchyNamesToString(newStateHierarchy);
			// now check if this external notification corresponds to an announced state change
			boolean matched = false;
			for (int i=0; i < announcedStateNameHierarchy.size(); i++) {
				String announcedState = announcedStateNameHierarchy.get(i);
				if (newStatesFlat.equals(announcedState)) {
					if (i == 0) {
						// this is the good case in which the messages announced  first internally is also received first externally
						logger.finest("StateChangeNotificationChecker received jbaci notification for '" + newStatesFlat + "' as expected.");
					}
					else {
						logger.warning("StateChangeNotificationChecker received jbaci notification for '" + newStatesFlat + "' before " + i + " other expected notifications.");						
					}
					announcedStateNameHierarchy.remove(i);
					matched = true;
					break;
				}
			}
			if (!matched) {
				logger.severe("The auto-monitoring of mastercomp state change notifications has detected an unexpected notification for '" + newStatesFlat + "'.");
			}
		}				
	}	
	
}
