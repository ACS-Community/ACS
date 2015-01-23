/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2014 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.alarmsystem.statistics;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;
import alma.acs.util.IsoDateFormat;

/**
 * <code>StatHashMap</code> encapsulate a HashMap where all the alarms
 * received during a time interval are stored.
 * The purpose of this class is to provide more detailed statistics then those
 * published in the log. 
 * <P>
 * <EM>Implementation note</EM><BR>
 * To generate and write statistics on file could be quite consuming.
 * To reduce the impact, when the time interval elapses, {@link #alarmsMap} point to a newly
 * created HashMap and the old one is used by a thread to calculate the statistics.
 * <BR>
 * All the maps are stored in the FIFO {@link StatHashMap#mapsQueue} queue. 
 * The thread waits for a new map in the queue the generate the statistics.
 * The size of the queue is limited to control the memory usage in the unlikely case
 * that generation of statistics lasts longer then a time interval.
 * 
 * @author acaproni
 * @since 2015.2
 */
public class StatHashMap implements Runnable {
	
	/** 
	 * A struct to hold the statistics at each iterations
	 * @author almadev
	 *
	 */
	private static class StatStruct {
		public StatStruct(long maxTotActiavations, long maxTotTerminations,
				Map<String, AlarmInfo> alarmsInfo) {
			super();
			this.numActiavations = maxTotActiavations;
			this.numTerminations = maxTotTerminations;
			this.alarmsInfo = alarmsInfo;
		}
		
		public final long numActiavations;
		public final long numTerminations;
		public final Map<String,AlarmInfo> alarmsInfo;
	}
	/**
	 * To get more useful statistics, the number of activations
	 * and terminations of each alarm in the last time interval
	 * is stored in this data structure.
	 * 
	 * @author acaproni
	 *
	 */
	private static class AlarmInfo {
		
		/**
		 * 
		 * @author The type of a valu to get
		 *
		 */
		private enum VALUE_TYPE {
			ACTIVATIONS,
			TERMINATIONS,
			OPERATIONS
		};
		
		/**
		 * The ID of the alarm whose statistics this object holds
		 */
		public final String alarmID;

		/**
		 *  Number of activations in the last time interval
		 */
		private long activations=0;
		
		/**
		 *  Number of terminations in the last time interval
		 */
		private long terminations=0;
		
		/**
		 * Constructor
		 * 
		 * @param alarmID the ID of the alarm
		 */
		public AlarmInfo(String alarmID) {
			if (alarmID==null || alarmID.isEmpty()) {
				throw new IllegalArgumentException("Invalid alarm ID");
			}
			this.alarmID=alarmID;
		}
		
		public void update(boolean active) {
			if (active) {
				activations++;
			} else {
				terminations++;
			}
		}
		
		/**
		 * Getter
		 * 
		 * @return The number of activations
		 */
		public long getActivations() {
			return activations;
		}

		/**
		 * Getter
		 * 
		 * @return The number of terminations
		 */
		public long getTerminations() {
			return terminations;
		}
		
		/**
		 * 
		 * @return The total number of activations and terminations executed on this alarm
		 *         during the time interval
		 */
		public long getTotalOperations() {
			return activations+terminations;
		}

		@Override
		public String toString() {
			return alarmID+": activations="+getActivations()+", terminations="+getTerminations();
		}
		
		public long getValue(VALUE_TYPE type) {
			if (type==null) {
				throw new IllegalArgumentException("The type can't be null");
			}
			if (type==VALUE_TYPE.ACTIVATIONS) {
				return getActivations();
			} 
			if (type==VALUE_TYPE.TERMINATIONS) {
				return getTerminations();
			}
			return getTotalOperations();
		}
		
	}

	/**
	 * A {@link AlarmInfo} comparator based on the number of activations 
	 * @author acaproni
	 *
	 */
	private static class ComparatorByActivations implements Comparator<AlarmInfo> {
		@Override
		public int compare(AlarmInfo o1, AlarmInfo o2) {
			return Long.valueOf(o1.getActivations()).compareTo(o2.getActivations());
		}
		
	}
	
	/**
	 * A {@link AlarmInfo} comparator based on the number of operations 
	 * (activations and terminations)
	 *  
	 * @author acaproni
	 *
	 */
	private static class ComparatorByOperations implements Comparator<AlarmInfo> {
		@Override
		public int compare(AlarmInfo o1, AlarmInfo o2) {
			return Long.valueOf(o1.getTotalOperations()).compareTo(Long.valueOf(o2.getTotalOperations()));
		}
	}
	
	/**
	 * A {@link AlarmInfo} comparator based on the number of terminations 
	 * @author acaproni
	 *
	 */
	private static class ComparatorByTerminations implements Comparator<AlarmInfo> {
		@Override
		public int compare(AlarmInfo o1, AlarmInfo o2) {
			return Long.valueOf(o1.getTerminations()).compareTo(o2.getTerminations());
		}
	}
	
	/**
	 * The logger 
	 */
	private final Logger logger;
	
	/**
	 * {@link #alarmsMap} records the activations/terminations of
	 * each alarm during the current time interval.
	 * It is used later on to calculate the statistics.
	 * <P>
	 * A new map is created for each time interval to minimize the impact in performances
	 * in {@link StatHashMap#processedFS(String, boolean)}. 
	 * <P>
	 * There is no need to synchronize this map because it is modified only
	 * by the synchronized {@link StatHashMap#processedFS(String, boolean)} method.
	 */
	private Map<String, AlarmInfo> alarmsMap = new HashMap<String, AlarmInfo>();
	
	/**
	 * The max number of maps to store in the {@link #mapsQueue}.
	 */
	private static final int MAXQUEUECAPACITY=10;
	
	/**
	 * When the time interval elapses the {@link StatHashMap#alarmsMap} is pushed
	 * into this queue and a new {@link StatHashMap#alarmsMap}.
	 * <P>
	 * The thread gets maps from this queue and generate the statistics.
	 */
	private final BlockingQueue<StatStruct> mapsQueue = new ArrayBlockingQueue<StatStruct>(MAXQUEUECAPACITY);
	
	/**
	 * The thread to calculate statistics
	 * @see #run()
	 */
	private final Thread thread = new Thread(this,this.getClass().getName());
	
	/**
	 * Statistics are calculated every time interval (in minutes)
	 */
	private final int timeInterval;
	
	/**
	 * Signal if the processing of statistics has been terminated
	 */
	private volatile boolean closed=false;
	
	/**
	 * The folder to write the files of statistics into
	 */
	private final File folder;
	
	/**
	 * The name of each file is composed of a prefix plus the index
	 * and the ".xml" extension.
	 */
	private static final String fileNamePrefix="AlarmSystemStats-";
	
	/**
	 * The name of each file of statistics is indexed with the following integer
	 */
	private int fileNumber=0;
	
	/**
	 * A new file is created when the size of actual one is greater then
	 * the value of this property 
	 */
	private static final long maxFileSize= 2000000000;
	
	private static final String closeXMLTag="</Statistics>\n";
	
	/**
	 * Constructor
	 * 
	 * @param lgger The logger
	 * @param intervalLength The length of each time interval (in minutes)
	 * @param folder The folder to write the files of statistics into
	 */
	public StatHashMap(Logger logger, int intervalLength, File folder) {
		if (logger==null) {
			throw new IllegalArgumentException("The logger can't be null");
		}
		this.logger=logger;
		if (intervalLength<0) {
			throw new IllegalArgumentException("The time interval must be greater or equal to 0");
		}
		this.timeInterval=intervalLength;
		if (folder==null) {
			throw new IllegalArgumentException("The logger can't be null");
		}
		if (!folder.isDirectory()) {
			throw new IllegalArgumentException(folder.getAbsolutePath()+ " not a directory");
		}
		if (!folder.canWrite()) {
			throw new IllegalArgumentException("Can't create files in "+folder.getAbsolutePath());
		}
		this.folder=folder;
		logger.log(AcsLogLevel.DEBUG,"Files with statistics will be saved in "+this.folder.getAbsolutePath());
	}
	
	/** 
	 * A new FS has been processed: if not already present,
	 * it is stored in the {@link #alarmsMap} map otherwise its state is updated. 
	 * 
	 * @param alarmID the ID of the FS
	 * @param active The activation state of the FS
	 */
	public void processedFS(String alarmID,boolean active) {
		if (closed || timeInterval==0) {
			return;
		}
		synchronized (this) {
			AlarmInfo info = alarmsMap.get(alarmID);
			if (info==null) {
				info = new AlarmInfo(alarmID);
				alarmsMap.put(alarmID, info);
			}
			info.update(active);	
		}
	}
	
	/**
	 * Create a new structure for the timer task:the eal calculation will be done there
	 * 
	 * @param activations Activations in the given interval
	 * @param terminations Terminations in the given interval
	 */
	public void calcStatistics(long activations, long terminations) {
		if (closed || timeInterval==0) {
			return;
		}
		// Create new map and release the lock so that processedFS does not block
		// impacting alarm server performances
		synchronized (this) {
			if (!mapsQueue.offer(new StatStruct(activations, terminations, alarmsMap))) {
				// Queue full: log a warning
				logger.warning("Alarm statistics lost for this time interval: the queue is full");
			}
			alarmsMap = new HashMap<String, AlarmInfo>();
		}
	}
	
	/**
	 * The thread picks maps from the {@link #mapsQueue} then calculate and write on file
	 * the statistics.
	 */
	public void run() {
		while (!closed) {
			// Get one map out of the queue
			StatStruct struct;
			try { 
				struct = mapsQueue.take();
			} catch (InterruptedException ie) {
				continue;
			}
			calculate(struct);
		}
	}
	
	/**
	 * Start the thread to evaluate the statistics
	 */
	public void start() {
		if (timeInterval!=0) {
			thread.setDaemon(true);
			thread.start();
		}
	}
	
	/**
	 * Stop the thread to calculate the statistics
	 * and free the resources.
	 */
	public void shutdown() {
		closed=false;
		if (timeInterval!=0) {
			thread.interrupt();
		}
	}
	
	/**
	 * Calculate the statistics of the passed time interval.
	 * <P>
	 * The figures calculated are:
	 * <UL>
	 * 	<LI>Total number of different alarms issued
	 * <LI>Total number of activations and terminations
	 * <LI>Total number of terminations
	 * 	<LI>Total number of activations
	 * 	<LI>Average number of activations and terminations per second
	 * 	<LI>The 5 alarms that has been changed (activated and/or terminated) more often
	 * 	<LI>The 5 alarms that has been activated more often
	 * 	<LI>The 5 alarms that has been terminated more often
	 * </UL>
	 * 
	 * @param statStruct The object with numbers for the statistics
	 */
	private void calculate(StatStruct statStruct) {
		// A collection to iterate over the values
		List<AlarmInfo> infos = new ArrayList<AlarmInfo>(statStruct.alarmsInfo.values());
		
		// The number of different alarms published in the interval
		int totAlarms = infos.size();
		
		
		// Total number of operations (i.e. activations and terminations)
		long totOperations=statStruct.numActiavations+statStruct.numTerminations;
		
		// Average number of operations per second
		double avgOpPerSecond=totOperations/(timeInterval*60);
		
		// Get the file to write the statistics
		BufferedWriter outF;
		try { 
			outF=openOutputFile();
		} catch (Throwable t) {
			logger.log(AcsLogLevel.ERROR, "Can't write on file: statistics lost", t);
			return;
		}
		
		// Build the string to write on disk
		String now = IsoDateFormat.formatCurrentDate();
		StringBuilder outStr = new StringBuilder("\t<Record TimeStamp=\""+now+"\">\n");
		outStr.append("\t\t<ProcessedAlarmTypes>");
		outStr.append(totAlarms);
		outStr.append("</ProcessedAlarmTypes>\n");
		outStr.append("\t\t<Activations>");
		outStr.append(statStruct.numActiavations);
		outStr.append("</Activations>\n");
		outStr.append("\t\t<Terminations>");
		outStr.append(statStruct.numTerminations);
		outStr.append("</Terminations>\n");
		outStr.append("\t\t<TotalAlarms>");
		outStr.append(totOperations);
		outStr.append("</TotalAlarms>\n");
		outStr.append("\t\t<AvgAlarmsPerSecond>");
		outStr.append(String.format("%.2f", avgOpPerSecond));
		outStr.append("</AvgAlarmsPerSecond>\n");
		
		outStr.append("\t\t<MostActivatedAlarms>\n");
		Collections.sort(infos, new ComparatorByActivations());
		appendListOfAlarms(infos,AlarmInfo.VALUE_TYPE.ACTIVATIONS,outStr,5);
		outStr.append("\t\t</MostActivatedAlarms>\n");
		
		
		outStr.append("\t\t<MostTerminatedAlarms>\n");
		Collections.sort(infos, new ComparatorByTerminations());
		appendListOfAlarms(infos,AlarmInfo.VALUE_TYPE.TERMINATIONS,outStr,5);
		outStr.append("\t\t</MostTerminatedAlarms>\n");
		
		outStr.append("\t\t<MostActivatedTerminatedAlarms>\n");
		Collections.sort(infos, new ComparatorByOperations());
		appendListOfAlarms(infos,AlarmInfo.VALUE_TYPE.OPERATIONS,outStr,5);
		outStr.append("\t\t</MostActivatedTerminatedAlarms>\n");

		outStr.append("\t</Record>\n");
		
		// This string appears at the end of the XML file
		outStr.append(closeXMLTag);
		
		try {
			outF.write(outStr.toString());
		} catch (Throwable t) {
			logger.log(AcsLogLevel.ERROR, "Failed to write record on file: statistics lost", t);
		} finally {
			try {
				outF.close();	
			} catch (Throwable t) {
				logger.log(AcsLogLevel.ERROR, "Failed to close the file", t);
			}
		}
		
		statStruct.alarmsInfo.clear();
		infos.clear();
	}
	
	/**
	 * Append lists of the top 5 five alarms of the passed list
	 * <P>
	 * The list reports the number of alarms with the highest numbers (alarms
	 * with the same values are grouped)
	 * 
	 * @param infos The list of alarms received in the time interval
	 * @param The type of value shown by the list
	 * @param strBuilder The builder to append the ID of the alarms
	 * @param depth The number of items to append
	 */
	private void appendListOfAlarms(
			List<AlarmInfo> infos,
			AlarmInfo.VALUE_TYPE type,
			StringBuilder strBuilder, int depth) {
		
		int count=0;
		long oldVal=0;
		int pos=infos.size()-1;
		while (count<=depth && pos>=0) {
			AlarmInfo alarm = infos.get(pos--);
			strBuilder.append("\t\t\t<ID value=\""+alarm.activations+"\">");
			strBuilder.append(alarm.alarmID);
			strBuilder.append("</ID>\n");
			if (oldVal!=alarm.getActivations()) {
				count++;
			}
			oldVal=alarm.getActivations();
		}
	}
	
	/**
	 * Open and create the output stream for writing statistics on file.
	 * <P>
	 * The file is opened for each writing and close immediately after.
	 * A new file is created, whenever the size of the actual file is
	 * greater then {@link #maxFileSize}.
	 * 
	 * @return The stream for writing into the file
	 * @throws IOException If can't open/create the file for writing
	 */
	private BufferedWriter openOutputFile() throws IOException {
		String actualFileName=fileNamePrefix+fileNumber+".xml";
		String folderName= folder.getAbsolutePath();
		if (!folderName.endsWith(""+File.separator)) {
			folderName=folderName+File.separator;
		}
		// Check the size of the file if it exists
		File f = new File(folderName+actualFileName);
		if (f.exists() && f.length()>maxFileSize) {
			fileNumber++;
			return openOutputFile();
		}
		BufferedWriter  ret;
		if (f.length()==0) {
			// New file
			ret  = new BufferedWriter(new FileWriter(f, true));
			ret.write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n");
			ret.write("<Statistics>\n");

		} else {
			// Remove the closing tag (closeXMLTag) before adding a new record
			RandomAccessFile temp = new RandomAccessFile(f, "rw");
			temp.setLength(temp.length()-closeXMLTag.length());
			temp.close();
			ret  = new BufferedWriter(new FileWriter(f, true));
		}
		return ret;
	}
}
