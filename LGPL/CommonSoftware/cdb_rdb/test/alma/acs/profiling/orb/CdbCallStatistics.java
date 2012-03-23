package alma.acs.profiling.orb;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.commons.math.stat.descriptive.DescriptiveStatistics;

import alma.acs.algorithms.DataBinner;
import alma.acs.algorithms.DataBinner.BinnedTimeValues;
import alma.acs.algorithms.DataBinner.TimeValue;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.IsoDateFormat;

/**
 * Used for http://almasw.hq.eso.org/almasw/bin/view/JAO/ABMCDBRetrieval, COMP-6488, 
 * and possibly other logs that must be analyzed.
 * @author hsommer
 */
public class CdbCallStatistics
{
	private final Logger logger;
	private final File dataFile;
	private final List<ProfilerMessage> messages;
	private static final String delim = "\t"; // delimiter for output files
	private final DecimalFormat df2 = new DecimalFormat("0.00");
	
	public CdbCallStatistics(String dataFileName) throws IOException {
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(CdbCallStatistics.class.getSimpleName(), false);
		dataFile = new File(dataFileName);
		OrbProfilerParser parser = new OrbProfilerParser(logger);
		messages = parser.parse(dataFile);
	}

	public void getDaoCallsWithParameter() throws IOException {
		OrbProfilerStatistics stat = new OrbProfilerStatistics(messages, logger);
		List<TimeValue<Integer>> calls = stat.getFinishedRequests("get_DAO");

		BufferedReader reader = new BufferedReader(new FileReader(dataFile));
		String line = null;
		while ((line = reader.readLine()) != null) {
			int pos = line.indexOf("Returning XML record for:");
			if (pos > 0) {
				String xmlRecordPath = line.substring(pos + "Returning XML record for: ".length());
				System.out.println(xmlRecordPath);
				// TODO: deal with call and stack
			}
		}
		reader.close();
	}

	
	public void getCallTimeByMethodGroup() throws IOException {
		OrbProfilerStatistics stat = new OrbProfilerStatistics(messages, logger);
		
		Map<String, List<TimeValue<Integer>>> callGroups = new LinkedHashMap<String, List<TimeValue<Integer>>>();
		// Corba object methods (TODO: add others)
		callGroups.put("_is_a", stat.getFinishedRequests("_is_a"));
		// methods for CDB change listening
		List<TimeValue<Integer>> listenerCalls = new ArrayList<TimeValue<Integer>>();
		listenerCalls.addAll(stat.getFinishedRequests("add_change_listener"));
		listenerCalls.addAll(stat.getFinishedRequests("remove_change_listener"));
		listenerCalls.addAll(stat.getFinishedRequests("listen_for_changes"));
		Collections.sort(listenerCalls);
		callGroups.put("listener methods", listenerCalls);
		// the hefty XML requests
		callGroups.put("get_DAO", stat.getFinishedRequests("get_DAO"));
		// calls to a DAO remote object
		List<TimeValue<Integer>> daoCalls = new ArrayList<TimeValue<Integer>>();
		daoCalls.addAll(stat.getFinishedRequests("get_long"));
		daoCalls.addAll(stat.getFinishedRequests("get_double"));
		daoCalls.addAll(stat.getFinishedRequests("get_string"));
		daoCalls.addAll(stat.getFinishedRequests("get_field_data"));
		daoCalls.addAll(stat.getFinishedRequests("get_string_seq"));
		daoCalls.addAll(stat.getFinishedRequests("get_long_seq"));
		daoCalls.addAll(stat.getFinishedRequests("get_double_seq"));
		daoCalls.addAll(stat.getFinishedRequests("destroy"));
		Collections.sort(daoCalls);
		callGroups.put("daoCalls", daoCalls);
		// returns a DAO remote object
		callGroups.put("get_DAO_Servant ", stat.getFinishedRequests("get_DAO_Servant"));
		// listing of available nodes
		List<TimeValue<Integer>> listingCalls = new ArrayList<TimeValue<Integer>>();
		listingCalls.addAll(stat.getFinishedRequests("list_nodes"));
		listingCalls.addAll(stat.getFinishedRequests("list_daos"));
		Collections.sort(listingCalls);
		callGroups.put("list methods", listingCalls);
		// todo: Also use WDAL, WDAO methods
		
		File outFile = new File(getFileNameBase() + "_callTimeByMethodGroup.txt");
		PrintStream pr = new PrintStream(outFile);
		pr.println("rdbCDB operation" + delim +  "# calls" + delim + "average [ms]" + delim +  "max(100%) [ms]" + delim +  "max(99%) [ms]" + delim +  "stdev [ms]");
		for (String groupName : callGroups.keySet()) {
			List<TimeValue<Integer>> calls = callGroups.get(groupName);
			// call duration statistics
			DescriptiveStatistics callTimeStats = new DescriptiveStatistics();
			for (TimeValue<Integer> timeValue : calls) {
				callTimeStats.addValue(timeValue.value);
			}
			pr.println(groupName + delim + calls.size() + delim + df2.format(callTimeStats.getMean()) + delim + 
					(int)callTimeStats.getMax() + delim + (int)callTimeStats.getPercentile(99) + delim + 
					df2.format(callTimeStats.getStandardDeviation()));
		}
		pr.close();
		logger.info("Wrote " + outFile.getAbsolutePath());
	}
	
	/**
	 * @param operationName Can be null, to use calls to all operations.
	 */
	public void getCallFrequency(String operationName) throws FileNotFoundException {
		OrbProfilerStatistics stat = new OrbProfilerStatistics(messages, logger);
		final int binIntervalMillis = 1000;

		List<TimeValue<Integer>> allCalls = stat.getFinishedRequests(operationName);
		Collections.sort(allCalls);
		DataBinner binner = new DataBinner();
		List<BinnedTimeValues<Integer>> binnedAllCalls = binner.binTimedData(allCalls, binIntervalMillis);
		String outFileName = getFileNameBase() + "_callFrequency" + (operationName != null ? operationName : "") + ".txt";
		File outFile = new File(outFileName);
		PrintStream pr = new PrintStream(outFile);
		pr.println("time" + delim + "#calls/s" + delim + "duration max" + delim + "duration average");
		for (BinnedTimeValues<Integer> binnedTimeValues : binnedAllCalls) {
			List<TimeValue<Integer>> timeValuesPerBin = binnedTimeValues.binnedData;
			DescriptiveStatistics callTimeStats = new DescriptiveStatistics();
			for (TimeValue<Integer> timeValue : timeValuesPerBin) {
				callTimeStats.addValue(timeValue.value);
			}
			long numCallsPerBin = callTimeStats.getN(); // or timeValuesPerBin.size();
			pr.println(timeString(binnedTimeValues.timeMillis) + delim + 
					numCallsPerBin + delim +
					(numCallsPerBin > 0 ? df2.format(callTimeStats.getMax()) : 0) + delim +
					(numCallsPerBin > 0 ? df2.format(callTimeStats.getMean()) : 0) );
		}
		pr.close();
		logger.info("Wrote " + outFile.getAbsolutePath());
	}
	
	/**
	 * Produces a list of timestamps and the number of concurrent calls at that time.
	 */
	public void getConcurrentCalls() throws FileNotFoundException {
		OrbProfilerStatistics stat = new OrbProfilerStatistics(messages, logger);
		List<TimeValue<Integer>> concCalls = stat.getConcurrentCalls();
		File outFile = new File(getFileNameBase() + "_concurrentCallCount.txt");
		PrintStream pr = new PrintStream(outFile);
		pr.println("time" + delim + "# concurrent calls");
		for (TimeValue<Integer> timeValue : concCalls) {
			pr.println(timeString(timeValue.timeMillis) + delim + timeValue.value);
		}
		pr.close();
		logger.info("Wrote " + outFile.getAbsolutePath());
	}
	
	private String getFileNameBase() {
		String fn = dataFile.getAbsolutePath();
		int dotIndex = fn.lastIndexOf('.');
		if (dotIndex > 0 && dotIndex > fn.length() - 10) {
			fn = fn.substring(0, dotIndex);
		}
		return fn;
	}
	
	private String timeString(long timeMillis) {
		return IsoDateFormat.formatDate(new Date(timeMillis));
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			if (args.length != 1) {
				throw new IllegalArgumentException("Data file name must be specified!");
			}
			CdbCallStatistics inst = new CdbCallStatistics(args[0]);
//			inst.getDaoCallsWithParameter();
			inst.getCallTimeByMethodGroup();
			inst.getCallFrequency(null);
			inst.getCallFrequency("get_DAO");
			inst.getConcurrentCalls();
			
			System.out.println("Done.");
		}
		catch (Throwable t) {
			t.printStackTrace();
		}
	}
}
