/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.nc.sm.generic;

import java.io.IOException;
import java.net.URL;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.scxml.Context;
import org.apache.commons.scxml.Evaluator;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCXMLExecutor;
import org.apache.commons.scxml.TriggerEvent;
import org.apache.commons.scxml.env.SimpleDispatcher;
import org.apache.commons.scxml.env.Tracer;
import org.apache.commons.scxml.env.jexl.JexlContext;
import org.apache.commons.scxml.env.jexl.JexlEvaluator;
import org.apache.commons.scxml.io.SCXMLParser;
import org.apache.commons.scxml.model.CustomAction;
import org.apache.commons.scxml.model.ModelException;
import org.apache.commons.scxml.model.SCXML;
import org.apache.commons.scxml.model.Transition;
import org.apache.commons.scxml.model.TransitionTarget;
import org.xml.sax.SAXException;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;
import alma.acs.nc.sm.generic.AcsScxmlActionDispatcher.ActionExceptionHandler;


/**
 * Class that encapsulates the Scxml engine for ACS.
 * It exposes only a subset of the many features offered by SCXML but also 
 * adds functionality. The focus is on compile time checking,
 * connection of action handler code with other pieces of software,
 * and exception handling. We hide dynamic changes to the state model and asynchronous
 * (queued) processing of multiple events.
 * <p>
 * The code has been taken from ESO SM framework class SMEngine.``
 * <p>
 * @TODO: Make the enum parameters distinguishable by introducing interfaces for action and signal enums. 
 *        Also consider making the SM state an enum type instead of String.
 * <p>
 * This class is thread safe with respect to sending events and checking the current state. 
 * The internally called methods {@link SCXMLExecutor#triggerEvent(TriggerEvent)} and 
 * {@link SCXMLExecutor#getCurrentStatus()} are synchronized.
 * In addition to the synchronization done by the underlying SCXMLExecutor we synchronize
 * {@link #fireSignal(Enum)} also in this class, so that the 'isFinal' call that delivers the return value
 * is guaranteed to run right after the signal was processed.
 * 
 * @param <S> The SM-specific signal enum.
 * @param <A> The SM-specific action enum.
 */
public class AcsScxmlEngine<S extends Enum<S>, A extends Enum<A>>
{
	private final Logger logger;
	private final AcsScxmlActionDispatcher<A> actionDispatcher;
	private final Class<S> signalType;
	private final Tracer errorTracer; // TODO: Allow user to supply own impl
	private final Evaluator exprEvaluator;
	private final EventDispatcher eventDispatcher;
	private final Context exprContext;
	private volatile SCXMLExecutor exec;
	private SCXML scxml;

	/**
	 * @param scxmlFileName The qualified xml file name, e.g. "/alma/acs/nc/sm/EventSubscriberStates.xml",
	 *                      in the form that {@link Class#getResource(String)} can use to load the scxml
	 *                      definition file from the classpath. 
	 * @param logger
	 * @param actionDispatcher 
	 * @param signalType enum class, needed to convert signal names to enum values.
	 * @throws IllegalArgumentException if any of the args are <code>null</code> or if the <code>actionDispatcher</code>
	 *                                  is not complete for all possible actions.
	 */
	public AcsScxmlEngine(String scxmlFileName, Logger logger, AcsScxmlActionDispatcher<A> actionDispatcher, Class<S> signalType) {

		this.logger = logger;
		this.actionDispatcher = actionDispatcher;
		this.signalType = signalType;
		
		// TODO decide if we want to insist here, or let the user check this beforehand
		if (!actionDispatcher.isActionMappingComplete()) {
			throw new IllegalArgumentException("actionDispatcher is not complete.");
		}

		errorTracer = new Tracer(); // create error tracer
		exprEvaluator = new JexlEvaluator(); // Evaluator evaluator = new ELEvaluator();
		eventDispatcher = new SimpleDispatcher(); // create event dispatcher
		exprContext = new JexlContext(); // set new context
	
		// Adding AcsScxmlActionDispatcher to the SM root context 
		// so that the generated action classes can get it from there and can delegate action calls.
		exprContext.set(AcsScxmlActionDispatcher.class.getName(), actionDispatcher);

		try {
			// load the scxml model
			loadModel(scxmlFileName);

			startExecution();

		} catch (Exception ex) {
			logger.log(Level.SEVERE, "Failed to load or start the state machine.", ex); // TODO
		}

	}


	/**
	 * Loads the SCXML model from an XML file stored inside of a jar file on the classpath.
	 * <p>
	 * TODO: define and throw exception in case of load/parse failure.
	 * 
	 * @param scxmlFileName The qualified xml file name, e.g. "/alma/acs/nc/sm/generated/EventSubscriberSCXML.xml"
	 */
	public void loadModel(final String scxmlFileName) {

		try {
			// TODO: Pass InputSource instead of String,
			// because the xml file may be inside a component impl jar file
			// which is not visible to the classloader of this generic SMEngine class.
			URL scxmlUrl = getClass().getResource(scxmlFileName);

			if (scxmlUrl == null) {
				logger.severe("Failed to load the scxml definition file '" + scxmlFileName + "' from the classpath.");
				// TODO ex;
			}
			
			List<CustomAction> scxmlActions = actionDispatcher.getScxmlActionMap();
			scxml = SCXMLParser.parse(scxmlUrl, errorTracer, scxmlActions);
			logger.fine("Loaded SCXML file " + scxmlUrl.toString() + "...");
		} catch (ModelException e) {
			logger.severe("Could not load model: " + e.getMessage());
		} catch (SAXException e) {
			logger.severe("Could not load model: " + e.getMessage());
		} catch (IOException e) {
			logger.severe("Could not load model: " + e.getMessage());
		}
	}

	/**
	 * Starts SCXML execution.
	 * <p>
	 * TODO: define and throw exception in case of model failure.
	 */
	public void startExecution() {

		try {
			exec = new SCXMLExecutor(exprEvaluator, eventDispatcher, errorTracer);

			// make sure scxml is a valid SCXML doc -> ToBeDone

			exec.addListener(scxml, errorTracer);
			exec.setRootContext(exprContext);

			exec.setStateMachine(scxml);
//			@TODO: When do we need a java invoker?
//			exec.registerInvokerClass("java", SMJavaInvoker.class);
			exec.go();
		} catch (ModelException e) {
			logger.severe("Could not start SM execution: " + e.getMessage());
		}

		logger.fine("Started SM execution ...");
	}

	/**
	 * Retrieves the current state as a string.
	 * @return The state name(s). 
	 *         Hierarchical states are separated by "::", with outer state first, e.g. "EnvironmentCreated::Connected::Suspended".
	 *         Parallel states are separated by " ".
	 */
	public String getCurrentState() {
		@SuppressWarnings("unchecked")
		Set<TransitionTarget> activeStates = exec.getCurrentStatus().getStates();

		StringBuilder sb = new StringBuilder();
		Iterator<TransitionTarget> iter = activeStates.iterator();
		while (iter.hasNext()) {
			sb.append(iter.next().getId());
			if (iter.hasNext()) {
				sb.append(' ');
			}
		}
		return sb.toString();
	}

	/**
	 * Checks if a given state is active. 
	 * The matching against the current state is done via String comparison, so that
	 * especially for hierarchical states it makes sense to call this method
	 * asking only for the outer state name(s).
	 * <p>
	 * TODO: Protect against mismatches that can occur if one state name includes another state name as a substring, 
	 *       e.g. by splitting names at "::" and comparing those fragements.
	 *  
	 * @param stateName The state name (fragment). 
	 *         Hierarchical states are separated by "::", with outer state first, e.g. "EnvironmentCreated::Connected".
	 * @return <code>true</code> if the given state is active.
	 */
	public synchronized boolean isStateActive(String stateName) {
		@SuppressWarnings("unchecked")
		Set<TransitionTarget> activeStates = exec.getCurrentStatus().getStates();
		
		for (TransitionTarget tt : activeStates) {
			if (tt.getId().indexOf(stateName) >= 0) {
				return true;
			}
		}
		return false;
	}

	
	/**
	 * Exposes the underlying SCXMLExecutor engine,
	 * for more specialized calls that we don't put in API methods for.
	 */
	public SCXMLExecutor getEngine() {
		return exec;
	}

	/**
	 * Sends a signal (event) to the state machine.
	 * 
	 * The call is synchronous and returns only when the state machine has gone through all transitions/actions, 
	 * where of course a /do activity would still continue to run asynchronously.
	 * Note that the underlying SCXML engine also supports asynchronous sending of multiple events
	 * at a time, but we do not expose this feature in ACS.
	 * <p>
	 * TODO: How can the client find out whether the signal was applicable 
	 *       for the current state or was ignored?
	 *       
	 * @return True - if all the states are final and there are not events
	 *         pending from the last step. False - otherwise.
	 */
	public synchronized boolean fireSignal(S signal) {
		TriggerEvent evnt = new TriggerEvent(signal.name(), TriggerEvent.SIGNAL_EVENT, null);
		try {
			exec.triggerEvent(evnt);
		} catch (ModelException e) {
			logger.info(e.getMessage());
		}
		return exec.getCurrentStatus().isFinal();
	}
	
	/**
	 * Simply stores the first exception it receives for later use.
	 * This means that if we have multiple transitions for an event, 
	 * and more than one transition throws an exception, then it is the first exception 
	 * that will get thrown to the user. 
	 */
	private static class MyActionExceptionHandler implements ActionExceptionHandler {
		volatile AcsJStateMachineActionEx theEx;
		
		@Override
		public void setActionException(AcsJStateMachineActionEx ex) {
			if (theEx == null) {
				theEx = ex;
			}
		}
	}
	
	/**
	 * Synchronous event handling as in {@link #fireSignal(Enum)}, 
	 * but possibly with exceptions for the following cases:
	 * <ul>
	 *   <li> The <code>signal</code> gets checked if it can be handled by the current state(s);
	 *        an <code>AcsJIllegalStateEventEx</code> exception is thrown if not.
	 *   <li> If an executed action throws a AcsJStateMachineActionEx exception, that exception gets thrown here.
	 *        Depending on the concrete state machine, an additional response to the error may be 
	 *        that the SM goes to an error state, due to an internal event triggered by the action.
	 *   <li> <code>ModelException</code>, as thrown by {@link SCXMLExecutor#triggerEvent}, unlikely
	 *        with our static use of the SCXML engine.
	 * </ul>
	 * @param signal
	 * @return True - if all the states are final and there are not events
	 *         pending from the last step. False - otherwise.
	 * @throws AcsJIllegalStateEventEx
	 * @throws AcsJStateMachineActionEx
	 * @throws ModelException
	 */
	public synchronized boolean fireSignalWithErrorFeedback(S signal) 
			throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx, ModelException {
		
		// check if signal is OK, throw exception if not.
		Set<S> applicableSignals = getApplicableSignals();
		if (!applicableSignals.contains(signal)) {
			AcsJIllegalStateEventEx ex = new AcsJIllegalStateEventEx();
			ex.setEvent(signal.name());
			ex.setState(getCurrentState());
			throw ex;
		}
		
		// Register error callback with action dispatcher.
		// This is only thread safe because this method is synchronized and we 
		// execute only one event at a time.
		MyActionExceptionHandler handler = new MyActionExceptionHandler();
		actionDispatcher.setActionExceptionHandler(handler);
		try {
			TriggerEvent evnt = new TriggerEvent(signal.name(), TriggerEvent.SIGNAL_EVENT, null);
			exec.triggerEvent(evnt);
	
			if (handler.theEx != null) {
				throw handler.theEx;
			} 
			else {
				// either there was no action associated with the event,
				// or all actions executed without exception.
				return exec.getCurrentStatus().isFinal();
			}
		} finally {
			actionDispatcher.setActionExceptionHandler(null);
		}
	}

	/**
	 * Gets the signals that would trigger transitions for the current state.
	 * <p>
	 * When actually sending such signals later on, the SM may have moved to a different state.
	 * To prevent this, you can synchronize on this AcsScxmlEngine, which will block concurrent calls to {@link #fireSignal(Enum)}.
	 * <p>
	 * This method can be useful for displaying applicable signals in a GUI,
	 * or to reject signals (with exception etc) that do not "fit" the current state
	 * (while normally such signals would be silently ignored). 
	 * The latter gets used in {@link #fireSignalWithErrorFeedback(Enum)}.
	 * 
	 * @see org.apache.commons.scxml.semantics.SCXMLSemanticsImpl#enumerateReachableTransitions(SCXML, Step, ErrorReporter)
	 */
	public synchronized Set<S> getApplicableSignals() {
		Set<String> events = new HashSet<String>();
		
		@SuppressWarnings("unchecked")
		Set<TransitionTarget> stateSet = new HashSet<TransitionTarget>(exec.getCurrentStatus().getStates());
		LinkedList<TransitionTarget> todoList = new LinkedList<TransitionTarget>(stateSet);

		while (!todoList.isEmpty()) {
			TransitionTarget tt = todoList.removeFirst();
			@SuppressWarnings("unchecked")
			List<Transition> transitions = tt.getTransitionsList();
			for (Transition t : transitions) {
				String event = t.getEvent();
				events.add(event);
			}
			TransitionTarget parentTT = tt.getParent();
			if (parentTT != null && !stateSet.contains(parentTT)) {
				stateSet.add(parentTT);
				todoList.addLast(parentTT);
			}
		}
		
		// convert signal names to enum constants
		Set<S> ret = new HashSet<S>();
		for (String signalName : events) {
			S signal = Enum.valueOf(signalType, signalName);
			ret.add(signal);
		}
		return ret;
	}
}
