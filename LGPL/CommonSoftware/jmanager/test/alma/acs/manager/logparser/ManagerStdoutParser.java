package alma.acs.manager.logparser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import org.apache.commons.lang.builder.CompareToBuilder;

import alma.acs.util.IsoDateFormat;
import alma.acs.util.StopWatch;

/**
 * Parses logs like
 * 2011-09-30T21:34:20.084 INFO [Manager] 'Python Client' requested component 'curl:///CONTROL/DV15/Mount'.
 * 2011-09-30T21:34:20.085 INFO [Manager] Component 'curl:///CONTROL/DV15/Mount' provided to 'Python Client'.
 * @author hsommer
 */
public class ManagerStdoutParser
{
	private final Logger logger;
	
	private final Pattern msgSplitSpace = Pattern.compile(" ");
	private final Pattern msgSplitQuotes = Pattern.compile("'");
	
	/**
	 * @param logger
	 * @param stdoutLogFile
	 * throws IllegalArgumentException if <code>stdoutLogFile</code> is missing or cannot be read.
	 */
	public ManagerStdoutParser(Logger logger) {
		this.logger = logger;
	}

	
	public List<ComponentRequest> parse(File stdoutLogFile) throws IOException, ParseException {
		if (stdoutLogFile == null) {
			throw new IllegalArgumentException("No log file specified (null).");
		}
		else if (!stdoutLogFile.exists() || !stdoutLogFile.canRead()) {
			throw new IllegalArgumentException("Log file " + stdoutLogFile.getAbsolutePath() + " does not exist or cannot be read.");
		}
		
		StopWatch sw = new StopWatch(logger);

		Map<ComponentRequestKey, List<ComponentRequest>> pendingRequests = new HashMap<ComponentRequestKey, List<ComponentRequest>>();
		List<ComponentRequest> finishedRequests = new ArrayList<ComponentRequest>();

		BufferedReader reader = new BufferedReader(new FileReader(stdoutLogFile));
		
		String line = null;
		int lineCount = 0;
		while ((line = reader.readLine()) != null) {
			lineCount++;
			parseLine(line, pendingRequests, finishedRequests);
		}
		reader.close();
		logger.info("Parsed file " + stdoutLogFile.getAbsolutePath() + " in " + sw.getLapTimeMillis() + " ms, found " + lineCount + " lines total, and " + finishedRequests.size() + " component activations.");
		
		// Check for unfinished requests (may be pending, or may have failed which we don't parse out yet)
		StringBuilder b = new StringBuilder();
		for (ComponentRequestKey key : pendingRequests.keySet()) {
			b.append(key.toString() + ": " + pendingRequests.get(key).size() + "\n");
		}
		logger.info("Unfinished or failed component requests: " + ( b.length() > 0 ? "\n" + b.toString() : "none"));
		
		return finishedRequests;
	}
	
	
	
	/**
	 * @param line
	 * @param pendingRequests List of pending requests, for efficiency implemented as a map where the client/CURL fields are used as key. 
	 * @param finishedRequests
	 * @throws ParseException
	 */
	protected void parseLine(String line, Map<ComponentRequestKey, List<ComponentRequest>> pendingRequests, List<ComponentRequest> finishedRequests) throws ParseException {
//		2011-09-30T21:34:20.084 INFO [Manager] 'Python Client' requested component 'curl:///CONTROL/DV15/Mount'.
//		2011-09-30T21:34:20.085 INFO [Manager] Component 'curl:///CONTROL/DV15/Mount' provided to 'Python Client'.
//		2011-09-30T21:38:20.008 FINE [Manager] 'CCLSimpleClient' requested non-sticky component 'curl:///CONTROL/DA43/WVR'.
//		2011-09-30T21:38:20.008 FINE [Manager] Non-sticky component 'curl:///CONTROL/DA43/WVR' provided to 'CCLSimpleClient'.
		
		String[] splitByQuotes = msgSplitQuotes.split(line);
		if (splitByQuotes.length == 5) {
			String action = splitByQuotes[2].trim();
			String[] words = msgSplitSpace.split(splitByQuotes[0]);
			if (words[2].equals("[Manager]")) {
				long time = IsoDateFormat.parseIsoTimestamp(words[0]).getTime();
				if (action.equals("requested component") || action.equals("requested non-sticky component")) {
					String clientName = splitByQuotes[1];
					String curl = splitByQuotes[3];
					ComponentRequest thisRequest = new ComponentRequest(time, clientName, curl);
					// Add this request to pendingRequests
					List<ComponentRequest> compRequests = pendingRequests.get(thisRequest.asComponentRequestKey()); 
					if (compRequests == null) {
						compRequests = new ArrayList<ComponentRequest>();
						pendingRequests.put(thisRequest.asComponentRequestKey(), compRequests);
					}
					else {
//						logger.info("Overlapping identical request " + thisRequest);
					}
					compRequests.add(thisRequest);
				}
				else if (action.equals("provided to")) {
					String clientName = splitByQuotes[3];
					String curl = splitByQuotes[1];
					List<ComponentRequest> compRequests = pendingRequests.get(new ComponentRequestKey(clientName, curl));
					if (compRequests == null || compRequests.isEmpty()) {
						logger.warning("Unmatched 'provided to' found at " + toISO(time) + "; clientName=" + clientName + ", CURL=" + curl);
					}
					else {
						ComponentRequest match = compRequests.remove(0);
						if (!compRequests.isEmpty()) {
							logger.warning("Ambiguous 'provided to' at " + toISO(time) + ", clientName=" + clientName + ", CURL=" + curl + 
									": Will match it with the first of " + (compRequests.size()+1) + " possibly matching requests, from " + toISO(match.timeRequested));
						}
						else {
							pendingRequests.remove(match.asComponentRequestKey());
						}
						match.timeProvided = time;
						finishedRequests.add(match);
					}
				}
			}
		}
	}
	
	private String toISO(long time) {
		return IsoDateFormat.formatDate(new Date(time));
	}
	
	
	public static class ComponentRequestKey implements Comparable<ComponentRequestKey>{
		
		protected final String clientName;
		protected final String curl;

		public ComponentRequestKey(String clientName, String curl) {
			this.clientName = clientName;
			this.curl = curl;
		}
		
		@Override
		public boolean equals(Object obj) {
			if (this == obj) return true;
			if (!(obj instanceof ComponentRequestKey)) return false;
			ComponentRequestKey other = (ComponentRequestKey) obj;
			return ( clientName.equals(other.clientName) && curl.equals(other.curl) );
		}
			
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + clientName.hashCode();
			result = prime * result + curl.hashCode();
			return result;
		}
		
		@Override
		public String toString() {
			return ("curl=" + curl + ", clientName=" + clientName );
		}

		@Override
		public int compareTo(ComponentRequestKey other) {
			return new CompareToBuilder()
				.append(this.clientName, other.clientName)
				.append(this.curl, other.curl)
				.toComparison();
		}
	}
	
	
	public static class ComponentRequest implements Comparable<ComponentRequest>
	{
		protected final ComponentRequestKey key;
		protected final long timeRequested;
		protected long timeProvided;

		public ComponentRequest(long timeRequested, String clientName, String curl) {
			this.key = new ComponentRequestKey(clientName, curl);
			this.timeRequested = timeRequested;
		}
		
		public ComponentRequestKey asComponentRequestKey() {
			return key;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj) return true;
			if (!(obj instanceof ComponentRequest)) return false;
			ComponentRequest other = (ComponentRequest) obj;
			return (key.equals(other.key) && timeRequested == other.timeRequested && timeProvided == other.timeProvided);
		}

		@Override
		public int hashCode() {
			return key.hashCode();
		}

		@Override
		public String toString() {
			return ("timeRequested=" + IsoDateFormat.formatDate(new Date(timeRequested)) + ", timeProvided="
					+ ( timeProvided > 0 ? IsoDateFormat.formatDate(new Date(timeProvided)) : "--" ) + 
					", curl=" + key.curl + 
					", clientName=" + key.clientName);
		}
		@Override
		public int compareTo(ComponentRequest other) {
			if (this.timeRequested < other.timeRequested) return -1;
			if (this.timeRequested > other.timeRequested) return 1;
			return this.key.compareTo(other.key);
		}
	}
}
