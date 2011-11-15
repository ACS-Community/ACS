package alma.acs.manager.logparser;


import static org.junit.Assert.assertEquals;

import java.io.File;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.commons.math.stat.descriptive.DescriptiveStatistics;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import alma.acs.algorithms.DataBinner;
import alma.acs.algorithms.DataBinner.BinnedTimeValues;
import alma.acs.algorithms.DataBinner.TimeValue;
import alma.acs.logging.ClientLogManager;
import alma.acs.manager.logparser.ManagerStdoutParser.ComponentRequest;
import alma.acs.manager.logparser.ManagerStdoutParser.ComponentRequestKey;
import alma.acs.util.IsoDateFormat;


/**
 * Test for manager stdout log parser. 
 * This test can be modified to analyze other manager output data for debugging.
 */
public class ManagerStdoutParserTest
{
	private Logger logger;
	
	@Rule 
	public TestName name = new TestName();

	@Before
	public void setUp() throws Exception {
		this.logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(name.getMethodName(), false);
		logger.info("--------- " + name.getMethodName() + " ---------");
	}

	@Test
	public void testParseLine() throws Exception {
		ManagerStdoutParser parser = new ManagerStdoutParser(logger);
		Map<ComponentRequestKey, List<ComponentRequest>> pendingRequests = new HashMap<ComponentRequestKey, List<ComponentRequest>>();
		List<ComponentRequest> finishedRequests = new ArrayList<ComponentRequest>();
		parser.parseLine("2011-09-30T21:34:20.084 INFO [Manager] 'Python Client' requested component 'curl:///CONTROL/DV15/Mount'.", pendingRequests, finishedRequests);
		parser.parseLine("2011-09-30T21:38:20.008 FINE [Manager] 'CCLSimpleClient' requested non-sticky component 'curl:///CONTROL/DA43/WVR'.", pendingRequests, finishedRequests);
		assertEquals(2, pendingRequests.keySet().size());
		assertEquals(0, finishedRequests.size());
		
		parser.parseLine("2011-09-30T21:34:20.085 INFO [Manager] Component 'curl:///CONTROL/DV15/Mount' provided to 'Python Client'.", pendingRequests, finishedRequests);
		assertEquals(1, pendingRequests.keySet().size());
		assertEquals(1, finishedRequests.size());
		ComponentRequest request = finishedRequests.get(0);
		assertEquals("Python Client", request.key.clientName);
		assertEquals("curl:///CONTROL/DV15/Mount", request.key.curl);
		assertEquals("2011-09-30T21:34:20.084", toISO(request.timeRequested));
		assertEquals("2011-09-30T21:34:20.085", toISO(request.timeProvided));
		logger.info("Got ComponentRequest as expected: " + request);
		pendingRequests.clear();
		finishedRequests.clear();
		
		// Unmatched 'provided to', should also log a WARNING which we currently don't check here
		parser.parseLine("2011-09-30T21:34:20.085 INFO [Manager] Component 'curl:///CONTROL/DV15/Mount' provided to 'Python Client'.", pendingRequests, finishedRequests);
		assertEquals(0, pendingRequests.keySet().size());
		assertEquals(0, finishedRequests.size());
	}
	
	@Test
	public void testParseManagerOutput() throws Exception, ParseException {
		String fileName = "sampleOutput/acsManager_2011-09-30_17.45.21.295";
		ManagerStdoutParser parser = new ManagerStdoutParser(logger);
		List<ComponentRequest> requests = parser.parse(new File(fileName));
		
		// some statistics for AIV-6529
		long t0 = IsoDateFormat.parseIsoTimestamp("2011-10-01T02:50:00.000").getTime();
		long t1 = IsoDateFormat.parseIsoTimestamp("2011-10-01T03:30:00.000").getTime();
		
		List<TimeValue<ComponentRequest>> data = new ArrayList<TimeValue<ComponentRequest>>();
		for (ComponentRequest r : requests) {
			if (r.timeRequested >= t0 && r.timeRequested <= t1) {
				data.add(new TimeValue<ComponentRequest>(r.timeRequested, r));
//				System.out.println(toISO(r.timeRequested) + ", " + (r.timeProvided-r.timeRequested) + ", " + r.key.curl.substring(8) + ", " + r.key.clientName);
			}
		}
		Collections.sort(data);
		
		DecimalFormat df = new DecimalFormat();
		df.setMaximumFractionDigits(2);
		int binIntervalSec = 5;
		DataBinner binner = new DataBinner();
		List<BinnedTimeValues<ComponentRequest>> binned = binner.binTimedData(data, binIntervalSec * 1000);
//		System.out.println("Time interval\t#Requests by 'Python Client'/" + binIntervalSec + 
//				"s\t#Requests by other clients/" + binIntervalSec + "s" + "\t" + "Average response time/ms");
		System.out.println("Time interval\t#Requests/" + binIntervalSec + 
				"s interval\tAverage response time/ms");
		for (BinnedTimeValues<ComponentRequest> binnedTimeValues : binned) {
			long timeCurrentBin = binnedTimeValues.timeMillis;
			List<TimeValue<ComponentRequest>> valuesCurrentBin = binnedTimeValues.binnedData;
			int numCallsPythonClient = 0;
			int numCallsOtherClients = 0;
			DescriptiveStatistics callTimeStats = new DescriptiveStatistics();
			for (TimeValue<ComponentRequest> timeValue : valuesCurrentBin) {
				ComponentRequest r = timeValue.value;
				if (r.key.clientName.equals("Python Client")) {
					numCallsPythonClient++;
				}
				else {
					numCallsOtherClients++;
				}
				callTimeStats.addValue(r.timeProvided - r.timeRequested); // duration in ms
			}
			System.out.println(toISO(timeCurrentBin) + "\t" + (numCallsPythonClient+numCallsOtherClients) + "\t" + df.format(callTimeStats.getMean()));
		}
	}

	
	private String toISO(long time) {
		return IsoDateFormat.formatDate(new Date(time));
	}
}
