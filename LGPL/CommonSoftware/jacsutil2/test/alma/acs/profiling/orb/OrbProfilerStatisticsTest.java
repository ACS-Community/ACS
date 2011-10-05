package alma.acs.profiling.orb;


import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import alma.acs.algorithms.DataBinner;
import alma.acs.algorithms.DataBinner.BinnedTimeValues;
import alma.acs.algorithms.DataBinner.TimeValue;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.IsoDateFormat;


public class OrbProfilerStatisticsTest
{
	private Logger logger;

	@Rule 
	public TestName name = new TestName();

	@Before
	public void setUp() throws Exception {
		this.logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(name.getMethodName(), false);
	}

	@Test
	public void testRounding() throws Exception {
		OrbProfilerStatistics stat = new OrbProfilerStatistics(null, logger);
		assertEquals(0, stat.floor(0, 1));
		assertEquals(0, stat.floor(0, 5));
		assertEquals(0, stat.floor(9, 10));
		assertEquals(10, stat.floor(10, 10));
		assertEquals(10, stat.floor(11, 10));
//		assertEquals(10, stat.ceiling(9, 10));
//		assertEquals(10, stat.ceiling(10, 10));
//		assertEquals(20, stat.ceiling(11, 10));
	}
	
	@Test
	public void testOperations() throws Exception {
		OrbProfilerParser parser = new OrbProfilerParser(logger);
		List<ProfilerMessage> messages = parser.parse(new File("hibernateCdbJDal-2011-09-12T153856.txt"));
		OrbProfilerStatistics stat = new OrbProfilerStatistics(messages, logger);
		String[] operations = stat.getOperations().toArray(new String[]{});
		assertArrayEquals(new String[] {"_is_a", "add_change_listener", "listen_for_changes", "get_DAO", "get_DAO_Servant", 
							"get_string", "get_long", "list_daos", "get_string_seq", "remove_change_listener", "get_field_data", "_non_existent"}, 
							operations);
	}
	

	
	@Test
	public void testFinishedRequestBinning() throws Exception {
		OrbProfilerParser parser = new OrbProfilerParser(logger);
		List<ProfilerMessage> messages = parser.parse(new File("hibernateCdbJDal-2011-09-12T153856.txt"));
		OrbProfilerStatistics stat = new OrbProfilerStatistics(messages, logger);
		
		List<TimeValue<Integer>> callsGetDao = stat.getFinishedRequests("get_DAO");
		assertEquals(8415, callsGetDao.size());
//		System.out.println("*** Last 10 get_DAO calls with response times in ms ***");
//		for (int i = callsGetDao.size()-11; i < callsGetDao.size(); i++) {
//			TimeValue call = callsGetDao.get(i);
//			System.out.println(timeString(call.timeMillis) + "\t(=" + call.timeMillis + ")\t" + call.value);
//		}
		
		final int binIntervalMillis = 1000;
		DataBinner binner = new DataBinner();
		List<BinnedTimeValues<Integer>> binnedData = binner.binTimedData(callsGetDao, binIntervalMillis);
//		System.out.println("*** Binned get_DAO calls per " + binIntervalMillis + " ms ***");
//		for (BinnedTimeValues binnedTimeValues : binnedData) {
//			// todo Calculate max or average
//			String msg = timeString(binnedTimeValues.timeMillis) + '\t' + binnedTimeValues.binnedData.size(); // + '\t' + binnedTimeValues.binnedData;
//			System.out.println(msg);
//		}
		assertEquals(4281, binnedData.size()); // 4281 one-second bins
		assertEquals("2011-09-12T15:40:47.500", timeString(binnedData.get(0).timeMillis)); // center time of first bin
		assertEquals(3, binnedData.get(0).binnedData.size()); // first bin must have 3 calls
		assertEquals("2011-09-12T15:40:47.605", timeString(binnedData.get(0).binnedData.get(0).timeMillis)); // first call in first bin
		assertEquals("2011-09-12T15:40:48.500", timeString(binnedData.get(1).timeMillis)); // center time of second bin
		assertEquals(4, binnedData.get(1).binnedData.size()); // second bin must have 4 calls
		assertEquals("2011-09-12T15:40:49.500", timeString(binnedData.get(2).timeMillis)); // center time of third bin
		assertEquals(0, binnedData.get(2).binnedData.size()); // third bin must be empty
	}
	
	@Test
	public void testConcurrentCalls() throws Exception {
		OrbProfilerParser parser = new OrbProfilerParser(logger);
		List<ProfilerMessage> messages = parser.parse(new File("hibernateCdbJDal-2011-09-12T153856.txt"));
		Collections.sort(messages);
//		for (int i = 0; i < 100; i++) {
//			ProfilerMessage msg = messages.get(i);
//			System.out.println(timeString(msg.timestamp) + "\t" + msg.type.name()  + "\t" + msg.operation);
//		}
		
		OrbProfilerStatistics stat = new OrbProfilerStatistics(messages, logger);
		List<TimeValue<Integer>> concCalls = stat.getConcurrentCalls();
		
		// todo: evaluate, output etc
	}
	
	private String timeString(long timeMillis) {
		return IsoDateFormat.formatDate(new Date(timeMillis));
	}
	
	
}
