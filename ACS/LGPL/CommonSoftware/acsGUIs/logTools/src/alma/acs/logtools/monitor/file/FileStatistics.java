/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.logtools.monitor.file;

import com.cosylab.logging.engine.log.LogTypeHelper;

import alma.acs.logtools.monitor.LogDetailsDispatcher;
import alma.acs.logtools.monitor.LogNumbersListener;
import alma.acs.logtools.monitor.TotalStatsData;
import alma.acs.util.IsoDateFormat;

/**
 * Write the statistics on a set of files.
 * <P>
 * The number of logs per interval are immediately written on file.
 * The total numbers instead are written by the thread.
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public class FileStatistics extends Thread implements LogNumbersListener {
	
	/**
	 * A class to ensure mutual exclusion while updating/reading values
	 * @author acaproni
	 *
	 */
	private class StatsData extends TotalStatsData {
		
		/**
		 * Format the string of values to write on file
		 * 
		 * @return the string of values to write on file
		 */
		public synchronized String formatString() {
			// Build the string to write on file
			StringBuilder str = new StringBuilder(IsoDateFormat.formatCurrentDate());
			for (int t=0; t<totalLogs.length; t++) {
				str.append(String.format(" %d %.2f",totalLogs[t],logsTypeDistribution[t]));
			}
			str.append(String.format(" %d %d %d %d\n",
					numOfLogs,
					shortestLogSize,
					longestLogSize,
					errors));
			return str.toString();
		}
		
	}
	
	/**
	 * The interval of time (sec) between writing of total numbers
	 */
	private final int WRITE_TOT_INTERVAL=10;
	
	/**
	 * signal the thread to terminate
	 */
	private boolean terminateThread=false;
	
	/**
	 * The file to write the logs receives for each time interval
	 */
	private final FileHelper recvLogsFile;
	
	/**
	 * The file to write the statistics about the logs received since
	 * the application started.
	 */
	private final FileHelper totLogsStatsFile;
	
	/**
	 * The values of the totals numbers
	 */
	private final StatsData totData = new StatsData();
	
	/**
	 * constructor 
	 * 
	 * @param folder The folder to write files into 
	 */
	public FileStatistics(String folder) {
		super(FileStatistics.class.getName());
		if (folder==null || folder.isEmpty()) {
			throw new IllegalArgumentException("Invalid folder");
		}
		recvLogsFile= new FileHelper(folder, "logsFlow",generateRecvLogsHdr());
		totLogsStatsFile= new FileHelper(folder, "logsStats",generateTotLogsHdr());
		
		// start the thread
		setDaemon(true);
		start();
	}

	/**
	 * Notified every time interval about the number of
	 * logs of each type received during the interval.
	 * <P>
	 * The numbers are immediately written on file.
	 *  
	 * @see alma.acs.logtools.monitor.LogNumbersListener#recvLogs(int[], int)
	 */
	@Override
	public synchronized void recvLogs(int[] nums, int secs) {
		StringBuilder str = new StringBuilder(IsoDateFormat.formatCurrentDate());
		for (int n: nums) {
			float f=(float)n/(float)LogDetailsDispatcher.NUMBER_LISTENERS_INTERVAL;
			str.append(String.format(" %5d %.3f",n,f));
		}
		str.append('\n');
		recvLogsFile.put(str.toString());
	}

	/**
	 * Close the computation and frees the resources
	 */
	public void close() {
		terminateThread=true;
		interrupt();
		recvLogsFile.close();
		totLogsStatsFile.close();
	}
	
	/**
	 * Generate the header string for the received logs
	 * 
	 * @return
	 */
	private String generateRecvLogsHdr() {
		StringBuilder ret = new StringBuilder("# timestamp");
		for (LogTypeHelper type: LogTypeHelper.values()) {
			ret.append(' ');
			ret.append(type.toString());
			ret.append(' ');
			ret.append(type.toString());
			ret.append("/sec");
		}
		ret.append('\n');
		return ret.toString();
	}
	
	/**
	 * Generate the header string for the received logs
	 * 
	 * @return
	 */
	private String generateTotLogsHdr() {
		StringBuilder ret = new StringBuilder("# timestamp");
		for (LogTypeHelper type: LogTypeHelper.values()) {
			ret.append(' ');
			ret.append(type.toString());
			ret.append(" %");
			ret.append(type.toString());
		}
		ret.append(" totalLogs shortestSize longestSize errors");
		ret.append('\n');
		return ret.toString();
	}
	
	/**
	 * Notify the listener about the total number of logs 
	 * read since the application started.
	 * 
	 * @param d The total number of logs read since the application started
	 * @see LogNumbersListener
	 */
	@Override
	public void totalData(TotalStatsData d) {
		totData.updateAll(d);
	}

	/**
	 * The thread to write total values on file
	 */
	@Override
	public void run() {
		while (!terminateThread) {
			try {
				Thread.sleep(WRITE_TOT_INTERVAL*1000);
			} catch (InterruptedException ie) {
				continue;
			}
			
			String str=totData.formatString();
			totLogsStatsFile.put(str);
		}
	}
}
