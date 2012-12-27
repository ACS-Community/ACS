package alma.acs.logging.testsupport;

import org.junit.Rule;
import org.junit.rules.TestName;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

/**
 * Convenience base class for stand-alone tests using JUnit 4 or higher (those that do not use the ACS runtime).
 * It creates a logger using the name of the currently executing test method.
 */
public class JUnit4StandaloneTestBase
{
	/**
	 * This field is the JUnit4 "workaround" way to obtain the name of the current test method.
	 */
	@Rule
	public TestName testName = new TestName();
	
	/**
	 * Gets derived from {@link #testName}.
	 */
	protected String testMethodName;

	/**
	 * Created in {@link #setUp()}, using <code>getLoggerForApplication</code>.
	 * The logger name is set to {@link #testMethodName}, which also determines the 
	 * application name that gets appended to the name of framework loggers (e.g. "scxml@myTestMethodName").
	 */
	protected AcsLogger logger;

	public JUnit4StandaloneTestBase() throws Exception {
	}

	/**
	 * Subclass should override this method, annotate it with <code>@Before</code>,
	 * and call <code>super.setUp()</code>.
	 */
	public void setUp() throws Exception {
		testMethodName = testName.getMethodName();
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(testMethodName, false);
		logger.info("----------------- " + getClass().getSimpleName() + "#" + testMethodName + " ----------------- ");
	}

	/**
	 * Subclass should override this method, annotate it with <code>@After</code>,
	 * and call <code>super.tearDown()</code>.
	 */
	public void tearDown() throws Exception {
	}
}
