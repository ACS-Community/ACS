package alma.acs.profiling.orb;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import alma.acs.logging.ClientLogManager;


public class OrbProfilerParserTest
{
	private Logger logger;

	@Rule 
	public TestName name = new TestName();

	@Before
	public void setUp() throws Exception {
		this.logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(name.getMethodName(), false);
	}

	/**
	 * Tests parsing of single lines, currently only for logs of "requestStarted" and "requestFinished"
	 */
	@Test
	public void testParseLine() throws Exception {
		OrbProfilerParser parser = new OrbProfilerParser(logger); 
		
		// requestStarted
		ProfilerMessage ret1 = parser.parseLine("2011-09-12T15:40:47.568 INFO [CDB-RDB] requestStarted(1, dalPOA, _is_a)");
		assertNotNull(ret1);
		assertEquals(ProfilerMessage.Type.REQUEST_STARTED, ret1.type);
		assertEquals(1315834847568L, ret1.timestamp); // needs UTC correction
		assertEquals(1, ret1.requestId);
		assertEquals("dalPOA", ret1.poaName);
		assertEquals("_is_a", ret1.operation);

		// requestFinished
		ProfilerMessage ret2 = parser.parseLine("2011-09-12T15:40:47.572 INFO [CDB-RDB] requestFinished(33, dalPOA, _is_a) in 4 ms");
		assertNotNull(ret2);
		assertEquals(ProfilerMessage.Type.REQUEST_FINISHED, ret2.type);
		assertEquals(1315834847572L, ret2.timestamp); // needs UTC correction
		assertEquals(33, ret2.requestId);
		assertEquals("dalPOA", ret2.poaName);
		assertEquals("_is_a", ret2.operation);
		assertEquals(4, ret2.timeElapsedMillis);
	}

	@Test
	public void testParseProfilerOutput() throws IOException {
		OrbProfilerParser parser = new OrbProfilerParser(logger);
		List<ProfilerMessage> messages = parser.parse(new File("hibernateCdbJDal-2011-09-12T153856.txt"));
		assertEquals(82698, messages.size()); // this number was checked independently, using grep
	}
}
