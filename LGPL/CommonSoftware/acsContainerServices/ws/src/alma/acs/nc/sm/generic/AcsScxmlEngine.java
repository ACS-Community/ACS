/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * "@(#) $Id$" 
 *
 * who                when       what
 * ----------------  ----------  ----------------------------------------------
 * COMODO            2012-09-05  Created.
 * 
 */

package alma.acs.nc.sm.generic;

import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
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
import org.apache.commons.scxml.model.TransitionTarget;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;


/**
 * Class that encapsulates the Scxml engine for ACS.
 * <p>
 * The code has been taken from ESO SM framwork class SMEngine.``
 * <p>
 * @TODO: Make the enums distinguishable by introducing interfaces for action and signal enums. 
 *        Also consider making the SM state an enum type instead of String.
 * 
 * @param <S> The SM-specific signal enum.
 * @param <A> The SM-specific action enum.
 */
public class AcsScxmlEngine<S extends Enum<S>, A extends Enum<A>>
{
	private final Logger logger;
	private final Tracer errorTracer;
	private final Evaluator exprEvaluator;
	private final EventDispatcher eventDispatcher;
	private final Context exprContext;
	private final AcsScxmlActionDispatcher<A> actionDispatcher;
	private volatile SCXMLExecutor exec;
	private SCXML scxml;

	/**
	 * @param scxmlFileName The qualified xml file name, e.g. "/alma/acs/nc/sm/EventSubscriberStates.xml",
	 *                      in the form that {@link Class#getResource(String)} can use to load the scxml
	 *                      definition file from the classpath. 
	 * @param logger
	 * @param actionDispatcher 
	 * @throws IllegalArgumentException if any of the args are <code>null</code> or if the <code>actionDispatcher</code>
	 *                                  is not complete for all possible actions.
	 */
	public AcsScxmlEngine(String scxmlFileName, Logger logger, AcsScxmlActionDispatcher<A> actionDispatcher) {

		this.logger = logger;
		this.actionDispatcher = actionDispatcher;
		
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
	 * Loads the SCXML model from an XML file stored in a jar file on the classpath.
	 * @param scxmlFileName The qualified xml file name, e.g. "alma.acs.nc.sm.EventSubscriberStates.xml"
	 */
	public void loadModel(final String scxmlFileName) {

		try {
			// TODO: Pass InputSource instead of String,
			// because the xml file may be inside a component impl jar file
			// which is not visible to the classloader of this generic SMEngine class.
			URL scxmlUrl = getClass().getResource(scxmlFileName);

			if (scxmlUrl == null) {
				logger.severe("Failed to load the scxml definition file '" + scxmlFileName + "' from the classpath.");
			}
			
			
			ErrorHandler myXmlErrorHandler = new ErrorHandler() {
				@Override
				public void error(SAXParseException ex) {
					logger.log(Level.SEVERE, "SAX error", ex);
				}

				@Override
				public void fatalError(SAXParseException ex) {
					logger.log(Level.SEVERE, "SAX fatalError", ex);
				}

				@Override
				public void warning(SAXParseException ex) {
					logger.warning(ex.getMessage());
				}
			};
			
			List<CustomAction> scxmlActions = actionDispatcher.getScxmlActionMap();
			scxml = SCXMLParser.parse(scxmlUrl, myXmlErrorHandler, scxmlActions);
			logger.info("Loaded SCXML file " + scxmlUrl.toString() + "...");
		} catch (ModelException e) {
			logger.severe("Could not load model: " + e.getMessage());
		} catch (SAXException e) {
			logger.severe("Could not load model: " + e.getMessage());
		} catch (IOException e) {
			logger.severe("Could not load model: " + e.getMessage());
		}
	}

	/**
	 * Start SCXML execution
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

		logger.info("Started SM execution ...");
	}

	/**
	 * Retrieve current state as string.
	 */
	public String getCurrentState() {

		// Set<TransitionTarget> activeStates = exec.getCurrentStatus().getAllStates();
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
	 * Exposes the underlying SCXMLExecutor engine,
	 * for more specialized calls that we don't put in API methods for.
	 */
	public SCXMLExecutor getEngine() {
		return exec;
	}

	/**
	 * Sends a signal (event) to the state machine.
	 * 
	 * @return True - if all the states are final and there are not events
	 *         pending from the last step. False - otherwise.
	 */
	public boolean fireSignal(S signal) {
		TriggerEvent evnt = new TriggerEvent(signal.name(), TriggerEvent.SIGNAL_EVENT, null);
		try {
			exec.triggerEvent(evnt);
		} catch (ModelException e) {
			logger.info(e.getMessage());
		}
		return exec.getCurrentStatus().isFinal();
	}


}
