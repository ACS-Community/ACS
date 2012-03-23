/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.profiling.orb;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import alma.acs.util.IsoDateFormat;
import alma.acs.util.StopWatch;

/**
 * Parser for output from {@link AcsORBProfilerImplBase} and also for additional profiler output 
 * to be generated in the future.
 * @author hsommer
 */
public class OrbProfilerParser
{
	private final Logger logger;
	
	/**
	 * Uses delimiters ' ', ',', '(', ')' and reduces multiple delimiter occurrences.
	 * (Parsing is in total ~15% faster using a precompiled pattern for splitting the lines. Not that it matters, done just out of curiosity.)
	 */
	private final Pattern msgSplitPattern = Pattern.compile("[ ,()]+");
	
	/**
	 * @param logger
	 * @param stdoutLogFile
	 * throws IllegalArgumentException if <code>stdoutLogFile</code> is missing or cannot be read.
	 */
	public OrbProfilerParser(Logger logger) {
		this.logger = logger;
	}

	
	/**
	 * Parses the given file and returns the profiler messages extracted from the file as a list for further processing.
	 */
	public List<ProfilerMessage> parse(File stdoutLogFile) throws IOException {
		if (stdoutLogFile == null) {
			throw new IllegalArgumentException("No data file specified (null).");
		}
		else if (!stdoutLogFile.exists() || !stdoutLogFile.canRead()) {
			throw new IllegalArgumentException("Data file " + stdoutLogFile.getAbsolutePath() + " does not exist or cannot be read.");
		}
		
		StopWatch sw = new StopWatch(logger);

		List<ProfilerMessage> messages = new ArrayList<ProfilerMessage>();
		BufferedReader reader = new BufferedReader(new FileReader(stdoutLogFile));
		
		String line = null;
		int lineCount = 0;
		try {
			while ((line = reader.readLine()) != null) {
				lineCount++;
				ProfilerMessage msg = parseLine(line);
				if (msg == null) {
	//				System.out.println("Skipping line: " + line);
				}
				else {
					messages.add(msg);
				}
			}
		}
		finally {
			reader.close();
		}
		logger.info("Parsed file " + stdoutLogFile.getAbsolutePath() + " in " + sw.getLapTimeMillis() + " ms, found " + lineCount + " lines total, and " + messages.size() + " orb profiler messages.");
		
		return messages;
	}
	
	/**
	 * Parses a line of orb profiler stdout output. 
	 * Returns a {@link ProfilerMessage} instance if the line could be parsed, <code>null</code> otherwise.
	 */
	protected ProfilerMessage parseLine(String line) {
		String[] words = msgSplitPattern.split(line);
		ProfilerMessage ret = null;
		if (words.length >= 4) { // other lines are anyway not matching, prevents ArrayIndexOutOfetc 
			try {
				if (words[0].equals("connectionThreadPoolSizeChanged")) {
					// connectionThreadPoolSizeChanged: idleThreads=0, totalThreads=1, maxThreads=1000
				}
				else if (words[0].equals("undeliveredRequest")) {
					// undeliveredRequest: messageSize=xxx, poaName=yyy, operation=zzz
					
				}
				else if (words[0].equals("requestQueueSizeChanged")) {
					// requestQueueSizeChanged: requestId=172, poaName=ComponentPOA_DefaultComponentWithBadNulls, queueSize=0, maxQueueLength=100
					
				}
				else if (words[0].equals("threadPoolSizeChanged")) {
					// threadPoolSizeChanged: poaName=null, idleThreads=4, totalThreads=5, maxThreads=20
	
				}
				else if (words[1].equals("requestStarted")) {
					// 2012-03-20T16:47:58.833 requestStarted(30, dalPOA, get_string, 23)
					ret = new ProfilerMessage(ProfilerMessage.Type.REQUEST_STARTED);
					ret.timestamp = IsoDateFormat.parseIsoTimestamp(words[0]).getTime();
					ret.requestId = Integer.parseInt(words[2]);
					ret.poaName = words[3];
					ret.operation = words[4];
					ret.threadId = Long.parseLong(words[5]);
				}
				else if (words[1].equals("requestFinished")) {
					// 2012-03-20T16:47:58.833 requestFinished(30, dalPOA, get_string, 23) in 0 ms
					ret = new ProfilerMessage(ProfilerMessage.Type.REQUEST_FINISHED);
					ret.timestamp = IsoDateFormat.parseIsoTimestamp(words[0]).getTime();
					ret.requestId = Integer.parseInt(words[2]);
					ret.poaName = words[3];
					ret.operation = words[4];
					ret.threadId = Long.parseLong(words[5]);
					ret.timeElapsedMillis = Integer.parseInt(words[7]);
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return ret;
	}
}
