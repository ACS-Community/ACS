package alma.acs.nc.sm;


import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertThat;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.StopWatch;



/**
 * A stand-alone test for the generated SCXML-based subscriber state machine.
 */
public class EventSubscriberSmEngineTest
{
	private AcsLogger logger;
	private EventSubscriberStateMachine engine;
	
	@Before
	public void setUp() throws Exception {
		fixLogging();
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getClass().getSimpleName(), false);
		
		StopWatch sw = new StopWatch();
		engine = new EventSubscriberStateMachine(logger);
		assertThat("SM creation should take less than a second.", sw.getLapTimeMillis(), lessThan(1000L));
	}

	@After
	public void tearDown() throws Exception {
		// TODO: nothing to clean up in the engine??
	}

	private void fixLogging() {
		// TODO: This Log factory should always be enabled, by a logging framework class from module acsjlog 
		System.setProperty(org.apache.commons.logging.LogFactory.FACTORY_PROPERTY, 
				alma.acs.logging.adapters.CommonsLoggingFactory.class.getName());
		
		// Configure scxml log levels
		System.setProperty("ACS.log.minlevel.namedloggers", "scxml@EventSubscriberSmEngineTest=4,4");
	}
	
	/**
	 */
	@Test
	public void testSimpleTransitions() {
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
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Connected::Suspended"));
		
		assertThat(engine.resume(), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Connected::Active"));

		assertThat(engine.stopReceivingEvents(), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentCreated::Disconnected"));

		assertThat(engine.cleanUpEnvironment(), is(false));
		assertThat(engine.getCurrentState(), equalTo("EnvironmentUnknown"));
	}
}
