package alma.acs.logging.adapters;

import java.util.logging.Level;
import java.util.logging.LogRecord;

import junit.framework.TestCase;

/**
 * Currently only tests the blanking of illegal message characters as described in COMP-3243.
 * @TODO: add test for level-based filtering and level-adjustments.
 * 
 * @author hsommer
 */
public class JacORBFilterTest extends TestCase
{
	private JacORBFilter jacORBFilter;
	
	@Override
	protected void setUp() throws Exception {
		jacORBFilter = new JacORBFilter();
	}
	
	public void testBlankIllegalMessageChars() {
		String msg = "My Message";
		LogRecord lr = new LogRecord(Level.INFO, msg);
		
		// legal message
		assertTrue(jacORBFilter.isLoggable(lr));
		assertEquals(msg, lr.getMessage());
		System.out.println("Decent message OK");
		
		// empty message
		lr.setMessage("");
		assertTrue(jacORBFilter.isLoggable(lr));
		assertEquals("", lr.getMessage());
		System.out.println("Empty message OK");
		
		// illegal message
		msg = "Illegal \u0000haracter message";
		lr.setMessage(msg);
		assertTrue(jacORBFilter.isLoggable(lr));
		assertEquals("Illegal #haracter message", lr.getMessage());
		System.out.println("Illegal message changed to '" + lr.getMessage() + "'");
	}
}
