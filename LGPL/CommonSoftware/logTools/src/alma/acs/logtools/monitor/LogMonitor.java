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
package alma.acs.logtools.monitor;

import java.io.File;

import alma.acs.logtools.monitor.file.FileStatistics;
import alma.acs.logtools.monitor.gui.LogMonitorFrame;
import alma.acs.util.CmdLineArgs;
import alma.acs.util.CmdLineRegisteredOption;

/**
 * The <code>LogMonitor</code> tool connects to the logging NC and elaborates statistics on the
 * logs it receives.
 * <P>
 * The statistics are based on the type (i.e. level) of the logs and
 * not on their content.
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public class LogMonitor extends Thread {
	
	/**
	 * <code>true</code> if the user wants the output on files
	 */
	private boolean outputOnfiles=false;
	
	/**
	 * <code>true</code> if the user wants the panel
	 */
	private boolean gui=true; 
	
	/**
	 * The folder to write files into.
	 * Default is the current folder. 
	 */
	private String folder=".";
	
	/**
	 * The listener that generates statistics on file
	 * <P>
	 * it is <code>null</code> if the user does not want
	 * statistics on file.
	 */
	private final FileStatistics fileStatistics;
	
	/**
	 * The listener that display stats on GUI.
	 * <P>
	 * it is <code>null</code> if the user does not want
	 * the GUI.
	 */
	private final LogMonitorFrame logMonitorGUI;
	
	/**
	 * The dispatcher of numbers to listeners
	 */
	private final LogDetailsDispatcher logNumDispatcher;
	
	/**
	 * <code>true</code> if the application has been closed
	 */
	private boolean closed=false;;
	
	/**
	 * Constructor
	 * 
	 * @param args The command line params
	 */
	public LogMonitor(String[] args) {
		if (parseCmdLineArgs(args)) {
			System.exit(0);
		}
		// install the shutdown hook
		Runtime.getRuntime().addShutdownHook(this);
		
		if (outputOnfiles && folder.compareTo(".")!=0) {
			//Check if the folder exists
			File dir = new File(folder);
			if (!dir.isDirectory() || !dir.canWrite()) {
				System.err.println("Invalid folder: "+folder);
				System.exit(-1);
			}
		}
		if (!outputOnfiles && !gui) {
			System.out.println("No GUI and no file? Nonthing to do, exiting...");
			System.exit(-1);
		}
		if (outputOnfiles) {
			fileStatistics=new FileStatistics(folder);
		} else {
			fileStatistics=null;
		}
		if (gui) {
			logMonitorGUI=new LogMonitorFrame(this);
		} else {
			logMonitorGUI=null;
		}
		logNumDispatcher = new LogDetailsDispatcher();
		if (fileStatistics!=null) {
			logNumDispatcher.addNumsListener(fileStatistics);
		}
		if (logMonitorGUI!=null) {
			logNumDispatcher.addNumsListener(logMonitorGUI);
		}
		
		/**
		 * wait until the user closes the application
		 */
		while (!closed) {
			try {
				synchronized (this) {
					this.wait();
				}
			} catch (InterruptedException ie) {
				continue;
			}
		}
		System.out.println("Done :-)");
	}
	
	/**
	 * Terminates the application
	 */
	public void close() {
		if (closed) {
			return;
		}
		closed=true;
		System.out.println("Closing...");
		logNumDispatcher.close();
		if (fileStatistics!=null) {
			logNumDispatcher.removeNumsListener(fileStatistics);
			fileStatistics.close();
		}
		if (logMonitorGUI!=null) {
			logMonitorGUI.close();
		}
		synchronized (this) {
			this.notifyAll();
		}
	}
	
	/**
	 * Parse the command line
	 * 
	 * @param args The command line params
	 * @return true if the user asked for help
	 */
	private boolean parseCmdLineArgs(String[] args) {
		CmdLineArgs cmdLineArgs = new CmdLineArgs();
		CmdLineRegisteredOption dumpOpt = new CmdLineRegisteredOption("-d","--dump",0);
		cmdLineArgs.registerOption(dumpOpt);
		CmdLineRegisteredOption textOpt = new CmdLineRegisteredOption("-t","--text",0);
		cmdLineArgs.registerOption(textOpt);
		CmdLineRegisteredOption helpOpt = new CmdLineRegisteredOption("-h","--help",0);
		cmdLineArgs.registerOption(helpOpt);
		CmdLineRegisteredOption foldOpt = new CmdLineRegisteredOption("-f","--folder",0);
		cmdLineArgs.registerOption(foldOpt);
		cmdLineArgs.parseArgs(args);
		
		if (cmdLineArgs.isSpecified(helpOpt)) {
			LogMonitor.printUsage();
			return true;
		}
		if (cmdLineArgs.isSpecified(textOpt)) {
			gui=false;
		} else {
			gui=true;
		}
		if (cmdLineArgs.isSpecified(dumpOpt)) {
			outputOnfiles=true;
		} else {
			outputOnfiles=false;
		}
		if (cmdLineArgs.isSpecified(foldOpt)) {
			String[] val = cmdLineArgs.getValues(foldOpt);
			if (val==null || val.length<1) {
				throw new IllegalStateException("Wrong or missing time (minutes)");
			} 
			folder=val[0];
		}
		return false;
	}
	
	/**
	 * Print the usage message
	 */
	public static void printUsage() {
		StringBuilder str=new StringBuilder();
		str.append("acsLogMonitor USAGE: acsLogMonitor [-h|--help] [-d|--dump] [-f|--folder <dir>]");
		str.append(" [-t|--text]");
		str.append("acsLogMonitor elaborates statistics by reading logs from the logging NC\n");
		str.append("The calculation is based on the type of logs and not on their content.");
		str.append("Options:\n");
		str.append("  -h|--help: print this message and exit\n");
		str.append("  -d|--dump: dump stats on a set ot files\n");
		str.append("  -t|--text: execute in text mode i.e. no GUI\n");
		str.append("  -f|--folder <dir>: write files on <dir> folder (default is .)\n\n");
		System.out.println(str.toString());
	}
	
	/**
	 * The thread for the shutdown hook
	 */
	public void run() {
		close();
	}

	/**
	 * Starts the application
	 * 
	 * @param args Command line arguments
	 */
	public static void main(String[] args) {
		LogMonitor monitor = new LogMonitor(args);
	}
}
