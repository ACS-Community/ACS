package alma.AcsNCTraceLog;
import junit.framework.TestCase;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

/**
 * Simple test for typesafe log
 *
 * @author yatagai
 *
 */
public class TypeSafeLogTest extends TestCase
{
	protected AcsLogger m_logger;
	public TypeSafeLogTest(String name) throws Exception {
		super(name);
	}

	@Override
	protected void setUp() throws Exception
	{
		m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getName(), true);
		m_logger.info("-------------------------------");
		m_logger.info("ComponentClientTestCase#setUp()");
	}
	
	/**
	 *  Simply create an instance for each type, and show logs
	 */
	public void testSimpleLog() {
		(new LOG_NC_ChannelCreated_ATTEMPT(m_logger)).log();
		(new LOG_NC_ChannelCreated_OK(m_logger)).log();
		(new LOG_NC_ChannelDestroyed_OK(m_logger)).log();
		(new LOG_NC_SubscriptionConnect_FAIL(m_logger)).log();
		(new LOG_NC_SubscriptionConnect_OK(m_logger)).log();
		(new LOG_NC_SubscriptionDisconnect_FAIL(m_logger)).log();
		(new LOG_NC_SubscriptionDisconnect_OK(m_logger)).log();
	}
}
