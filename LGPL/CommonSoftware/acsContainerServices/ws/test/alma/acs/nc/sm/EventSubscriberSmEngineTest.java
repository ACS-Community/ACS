package alma.acs.nc.sm;


import static alma.acs.nc.sm.generated.EventSubscriberSignal.cleanUpEnvironment;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.resume;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.setUpEnvironment;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.startReceivingEvents;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.stopReceivingEvents;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.suspend;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.util.Collection;
import java.util.Set;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.TriggerEvent;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.logging.testsupport.JUnit4StandaloneTestBase;
import alma.acs.nc.sm.generated.EventSubscriberActionDispatcher;
import alma.acs.nc.sm.generated.EventSubscriberAllActionsHandler;
import alma.acs.nc.sm.generated.EventSubscriberSignal;
import alma.acs.nc.sm.generic.AcsScxmlEngine;
import alma.acs.util.StopWatch;



/**
 * A stand-alone test for the generated SCXML-based subscriber state machine.
 */
public class EventSubscriberSmEngineTest extends JUnit4StandaloneTestBase 
{
	private MyActionHandler actionHandler;
	private EventSubscriberStateMachine engine;

	/**
	 * Optionally throws an exception in {@link #createEnvironmentAction(EventDispatcher, ErrorReporter, SCInstance, Collection)}, 
	 * which gets used in {@link EventSubscriberSmEngineTest#testActionException()}.
	 */
	private static class MyActionHandler implements EventSubscriberAllActionsHandler {
		
		public volatile boolean throwExceptionInCreateEnvironment = false;
		
		@Override
		public void createEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
				throws AcsJStateMachineActionEx {
			if (throwExceptionInCreateEnvironment) {
				throw new AcsJStateMachineActionEx();
			}
		}
		@Override
		public void destroyEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
			// nothing
		}
		@Override
		public void createConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
			// nothing
		}
		@Override
		public void destroyConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
			// nothing
		}
		@Override
		public void suspendAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
			// nothing
		}
		@Override
		public void resumeAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
			// nothing
		}
	}
	

	@Before
	public void setUp() throws Exception {
		super.setUp();
		configureLogging(AcsLogLevelDefinition.INFO);
		
		StopWatch sw = new StopWatch();
		
		// we use empty handlers, except for environmentActionHandler
		actionHandler = new MyActionHandler();
		engine = new EventSubscriberStateMachine(logger, new EventSubscriberActionDispatcher(actionHandler, logger));
		
		// usually it's quite a bit faster, but for some reason the NRI test on te48 takes ~1.2 seconds
		assertThat("SM creation should take less than a 1.4 second.", sw.getLapTimeMillis(), lessThan(1400L));
	}

	@After
	public void tearDown() throws Exception {
		// TODO: nothing to clean up in the engine??
		super.tearDown();
	}

	private void configureLogging(AcsLogLevelDefinition localLevel) {
		String scxmlLoggerName = "scxml@" + testMethodName;
		LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();
		logConfig.setMinLogLevelLocal(localLevel, scxmlLoggerName);
		logConfig.setMinLogLevel(AcsLogLevelDefinition.OFF, scxmlLoggerName);
		ClientLogManager.getAcsLogManager().suppressRemoteLogging();
	}

	
	/**
	 * Got through the SM's lifecycle. 
	 * Everything neat, no exceptions expected.
	 */
	@Test
	public void testSimpleTransitions() throws Exception {
		assertThat(engine.getCurrentState(), equalTo("EnvironmentUnknown"));
		
		assertThat("SM should be in a non-final state.", engine.setUpEnvironment(), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Disconnected"));
		
		assertThat(engine.startReceivingEvents(), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Connected::Active"));

		assertThat(engine.suspend(), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Connected::Suspended"));
		
		assertThat(engine.resume(), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Connected::Active"));

		assertThat(engine.suspend(), is(false));
		// If the following fails because the states were renamed, make sure to fix also AcsEventSubscriberImplBase#isSuspended()
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Connected::Suspended"));
		
		assertThat(engine.resume(), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Connected::Active"));

		assertThat(engine.stopReceivingEvents(), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Disconnected"));

		assertThat(engine.cleanUpEnvironment(), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentUnknown"));
	}

	/**
	 * Verifies the behavior of the SM for signals not applicable for the current state:
	 * <ul>
	 *   <li> {@link AcsScxmlEngine#fireSignal(Enum)} and its wrapper methods such as 
	 *        {@link EventSubscriberStateMachine#stopReceivingEvents()} should silently ignore the event.
	 *   <li> {@link AcsScxmlEngine#fireSignalWithErrorFeedback(Enum)} should throw AcsJIllegalStateEventEx.
	 * </ul>
	 */
	@Test
	public void testIllegalSignal() throws Exception {
		// here the wrong event should be just ignored
		assertThat(engine.getCurrentState(), equalTo("EnvironmentUnknown"));
		assertThat(engine.fireSignal(resume), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentUnknown"));
		
		// here we expect AcsJIllegalStateEventEx
		try {
			engine.fireSignalWithErrorFeedback(stopReceivingEvents);
			fail("AcsJIllegalStateEventEx expected.");
		} catch (AcsJIllegalStateEventEx ex) {
			assertThat(ex.getState(), equalTo("EnvironmentUnknown"));
			assertThat(ex.getEvent(), equalTo(stopReceivingEvents.name()));
		}
		assertThat(engine.getCurrentState(), equalTo("EnvironmentUnknown"));
	}

	/**
	 * Tests {@link AcsScxmlEngine#getApplicableSignals()} for different states.
	 */
	@Test
	public void testGetApplicableSignals() {
		Set<EventSubscriberSignal> signals = engine.getScxmlEngine().getApplicableSignals();
		assertThat(signals, contains(setUpEnvironment));
		engine.fireSignal(setUpEnvironment);
		signals = engine.getScxmlEngine().getApplicableSignals();
		assertThat(signals, containsInAnyOrder(cleanUpEnvironment, startReceivingEvents)); // verifies also set size == 2
	}

	/**
	 * Verifies the behavior of the SM for an action that throws AcsJStateMachineActionEx:
	 * <ul>
	 *   <li> {@link AcsScxmlEngine#fireSignalWithErrorFeedback(Enum)} should throw this AcsJStateMachineActionEx
	 *        but nonetheless move on to the target state unless the SM foresees internal error transitions.
	 *   <li> {@link AcsScxmlEngine#fireSignalWith(Enum)} should log a warning message (not yet automatically verified).
	 * </ul>
	 */
	@Test
	public void testActionException() throws Exception {
		// provoke an exception in the create action
		actionHandler.throwExceptionInCreateEnvironment = true;
		
		try {
			engine.fireSignalWithErrorFeedback(setUpEnvironment);
			fail("AcsJStateMachineActionEx expected");
		} catch (AcsJStateMachineActionEx ex) {
			// good
		}
		// The state changes, even when the action fails. Restore to initial state.
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Disconnected"));
		engine.fireSignal(cleanUpEnvironment);

		// Without exception feedback, we expect just a warning log:
		// WARNING [testActionException] Handler class alma.acs.nc.sm.EventSubscriberSmEngineTest$MyEnvironmentActionHandler failed to execute action createEnvironment
		// alma.acs.nc.sm.generic.AcsJStateMachineActionEx
		//    at alma.acs.nc.sm.EventSubscriberSmEngineTest$MyEnvironmentActionHandler.create(EventSubscriberSmEngineTest.java:68)
		engine.fireSignal(setUpEnvironment);
	}
	
	/**
	 * Tests {@link AcsScxmlEngine#isStateActive(String)} for different states.
	 */
	@Test
	public void testIsStateActive() {
		assertThat(engine.getScxmlEngine().isStateActive("EnvironmentUnknown"), is(true));
		assertThat(engine.getScxmlEngine().isStateActive("EnvironmentCreated"), is(false));
		assertThat(engine.getScxmlEngine().isStateActive("Disconnected"), is(false));
		
		engine.fireSignal(setUpEnvironment);
		assertThat(engine.getScxmlEngine().isStateActive("EnvironmentUnknown"), is(false));
		assertThat(engine.getScxmlEngine().isStateActive("EnvironmentCreated"), is(true));
		assertThat(engine.getScxmlEngine().isStateActive("Disconnected"), is(true));
		assertThat(engine.getScxmlEngine().isStateActive("EnvironmentCreated::Disconnected"), is(true));
	}

	/**
	 * Cycles 5.000 times between suspended and resumed state
	 * and asserts that the state machine overhead be less than 0.6 ms per event.
	 * This is done both with and without the check for allowed events.
	 */
	@Test
	public void testTransitionPerformance() throws Exception {
		
		// Configure logger "scxml@testTransitionPerformance" to higher than the INFO setting 
		// used in the other tests, because otherwise the SM is much too slow.
		configureLogging(AcsLogLevelDefinition.WARNING);
		
		final int cycles = 5000;
		
		engine.fireSignal(setUpEnvironment);
		engine.fireSignal(startReceivingEvents);
		
		StopWatch sw = new StopWatch();
		for (int i = 0; i < cycles; i++) {
			engine.fireSignal(suspend);
			engine.fireSignal(resume);
		}
		long elapsed = sw.getLapTimeMillis();
		logger.info("suspend/resume cycles: cycles = " + cycles + ", elapsed time ms = " + elapsed);
		// 5.000 cycles mean 10.000 signals/transitions, so 6000 ms max means <= 0.6 ms per transition.
		// Usually its around 1000 ms, but NRI machine te48 has gotten slow and reports failures regularly.
		assertThat("SM transitions too slow.", elapsed, lessThan(6000L)); 
		
		// Now do the same again, but including the check for applicable events
		// It seems that this second run is even faster, in spite of the additional check.
		// JIT optimization or some initial setup seem to distort the performance results.
		sw = new StopWatch();
		engine.getScxmlEngine();
		for (int i = 0; i < cycles; i++) {
			engine.fireSignalWithErrorFeedback(EventSubscriberSignal.suspend);
			engine.fireSignalWithErrorFeedback(EventSubscriberSignal.resume);
		}
		elapsed = sw.getLapTimeMillis();
		logger.info("suspend/resume cycles with event checks: cycles = " + cycles + ", elapsed time ms = " + elapsed);
		assertThat("SM transitions too slow.", elapsed, lessThan(6000L)); 
	}

}
