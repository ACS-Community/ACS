/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2010 Copyright by ESO
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging.dialogs.main;

import com.cosylab.logging.engine.audience.Audience.AudienceInfo;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.LoggingClient;

import alma.acs.util.CmdLineArgs;
import alma.acs.util.CmdLineOption;
import alma.acs.util.CmdLineRegisteredOption;

/**
 * A class to parse the command line.
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public class CommandLineParser {
	
	/**
	 * <code>true</code> if the user wants the help
	 */
	private boolean help=false;
	
	/**
	 * <code>true</code> if the user wants to run jlog with no limits
	 * in the number of logs in the table
	 */
	private boolean unlimited=false;
	
	/**
	 * <code>true</code> if the user do not want to connect jlog to 
	 * an ACS session at startup
	 */
	private boolean doNotConnect=false;
	
	/**
	 * The name of the filter file to load.
	 * <P>
	 * it is <code>null</code> if there is no filter file name specified 
	 * in the command line.
	 */
	private String filterFileName=null;
	
	/**
	 * The name of the engine filter file to load.
	 * <P>
	 * it is <code>null</code> if there is no engine filter file name specified 
	 * in the command line.
	 */
	private String engineFilterFileName=null;
	
	/**
	 * The initial discard level
	 * <P>
	 * If it not set in the command line, the logging client starts
	 * with the default discard level.
	 */
	private LogTypeHelper discardLevel=LoggingClient.DEFAULT_DISCARDLEVEL;
	
	/**
	 * The file of logs to load at startup
	 * <P>
	 * it is <code>null</code> if there is no file name specified 
	 * in the command line.
	 */
	private String fileToLoad=null;
	
	/**
	 * The audience to set at startup
	 * <P>
	 * it is <code>null</code> if there is no audience specified 
	 * in the command line.
	 */
	private AudienceInfo audience=null;
	
	/**
	 * Constructor.
	 * 
	 * @param args The command line parameters
	 * @throws Exception In case of error parsing the command line
	 */
	public CommandLineParser(String[] args) throws Exception {
		parse(args);
		dumpOptions();
	}
	
	/**
	 * Print the standard usage message if the parameters in the command
	 * line are wrong.
	 *
	 * @param errorMsg An optional error message to print
	 */
	public static void printUsage(String errorMsg) {
		if (errorMsg!=null) {
			System.err.println("Error parsing the command line: "+errorMsg);
		}
		StringBuilder str = new StringBuilder("USAGE:\n");
		str.append("jlog [logFileName] ");
		str.append("[-h|--help] ");
		str.append("[(-f|--filter) <filterFileName>] ");
		str.append("[(-e|--engineFilter) <filterFileName>]");
		str.append("[(-d|--discard) (NONE|discard level)] ");
		str.append("[-dnc|--DoNotConnect] ");
		str.append("[-u|--unlimited] ");
		str.append("[(-l|--load) <logFileName>] ");
		str.append("[(-a|--audience) <audience>]\n");
		str.append("Options:\n");
		str.append("-h|--help: print this message and exit\n");
		str.append("-f|--filter <fileName>: load the filters in <fileName> in the table\n");
		str.append("-e|--engineFilter <fileName>: load the filters in <fileName> in the engine\n");
		str.append("-d|--discard <level>: set the discard level at startup; NONE means no discard level\n");
		str.append("  Valid discard levels are:");
		str.append("\tNone");
		for (LogTypeHelper logType: LogTypeHelper.values()) {
			str.append('\t');
			str.append(logType);
			str.append('\n');
		}
		str.append("-dnc|--DoNotConnect: do not connect to ACS\n");
		str.append("-u|--unlimited: do not limit the number of logs in the table\n");
		str.append("-l|--load <logFileName>: load the <logFileName> file of logs in the table\n");
		str.append("-a|--audience <audience>: set the audience\n");
		str.append("  Available audiences are:\n");
		for (String aName: AudienceInfo.getShortNames()) {
			str.append('\t');
			str.append(aName);
			str.append('\n');
		}
		str.append('\n');
		System.out.println(str.toString());
	}
	
	/**
	 * Parse the command line.
	 * 
	 * @throws Exception In case of error parsing the command line
	 */
	private void parse(String[] args) throws Exception {
		CmdLineArgs cmdLineArgs = new CmdLineArgs();
		// Register the options
		CmdLineRegisteredOption helpCmd = new CmdLineRegisteredOption("-h","--help",0);
		cmdLineArgs.registerOption(helpCmd);
		CmdLineRegisteredOption filterCmd = new CmdLineRegisteredOption("-f","--filter",0);
		cmdLineArgs.registerOption(filterCmd);
		CmdLineRegisteredOption engineFilterCmd = new CmdLineRegisteredOption("-e","--engineFilter",0);
		cmdLineArgs.registerOption(engineFilterCmd);
		CmdLineRegisteredOption discardCmd = new CmdLineRegisteredOption("-d","--discard",0);
		cmdLineArgs.registerOption(discardCmd);
		CmdLineRegisteredOption doNotConnectCmd = new CmdLineRegisteredOption("-dnc","--DoNotConnect",0);
		cmdLineArgs.registerOption(doNotConnectCmd);
		CmdLineRegisteredOption unlimitedCmd = new CmdLineRegisteredOption("-u","--unlimited",0);
		cmdLineArgs.registerOption(unlimitedCmd);
		CmdLineRegisteredOption loadCmd = new CmdLineRegisteredOption("-l","--load",0);
		cmdLineArgs.registerOption(loadCmd);
		CmdLineRegisteredOption audienceCmd = new CmdLineRegisteredOption("-a","--audience",0);
		cmdLineArgs.registerOption(audienceCmd);
		// Parse
		cmdLineArgs.parseArgs(args);
		// Get values
		if (cmdLineArgs.isSpecified(helpCmd)) {
			help=true;
		}
		if (cmdLineArgs.isSpecified(filterCmd)) {
			String[] val = cmdLineArgs.getValues(filterCmd);
			if (val==null || val.length<1) {
				throw new IllegalStateException("Filter file name missing");
			}
			filterFileName=val[0];
		}
		if (cmdLineArgs.isSpecified(engineFilterCmd)) {
			String[] val = cmdLineArgs.getValues(engineFilterCmd);
			if (val==null || val.length<1) {
				throw new IllegalStateException("Engine filter file name missing");
			}
			engineFilterFileName=val[0];
		}
		if (cmdLineArgs.isSpecified(discardCmd)) {
			String[] val = cmdLineArgs.getValues(discardCmd);
			if (val==null || val.length<1) {
				throw new IllegalStateException("Discard level missing ");
			}
			
			discardLevel= LogTypeHelper.fromLogTypeDescription(val[0]);
			if (discardLevel==null && !(val[0].compareToIgnoreCase("None")==0)) {
				throw new Exception("Invalid discard level "+val[0]);
			}
		}
		if (cmdLineArgs.isSpecified(doNotConnectCmd)) {
			doNotConnect=true;
		}
		if (cmdLineArgs.isSpecified(unlimitedCmd)) {
			unlimited=true;
		}
		if (cmdLineArgs.isSpecified(loadCmd)) {
			String[] val = cmdLineArgs.getValues(loadCmd);
			if (val==null || val.length<1) {
				throw new IllegalStateException("File name of logs to load mssing");
			}
			fileToLoad=val[0];
		}
		if (cmdLineArgs.isSpecified(audienceCmd)) {
			String[] val = cmdLineArgs.getValues(audienceCmd);
			if (val==null || val.length<1) {
				throw new IllegalStateException("Audience mssing");
			}
			audience=AudienceInfo.fromShortName(val[0]);
		}
	}
	
	/**
	 * Print a short resumen of the parameters read from the command line
	 * 
	 */
	private void dumpOptions() {
		StringBuilder ret =new StringBuilder("Command line options:\n");
		ret.append("File of logs: "+fileToLoad);
		ret.append('\n');
		if (audience==null) {
			ret.append("Audience: not set");
		} else {
			ret.append("Audience: "+audience.name);
		}
		ret.append('\n');
		ret.append("Filter file: "+filterFileName);
		ret.append('\n');
		ret.append("engine filter file: "+engineFilterFileName);
		ret.append('\n');
		ret.append("unlimited num. of logs in table: "+unlimited);
		ret.append('\n');
		ret.append("Do not connect to ACS: "+doNotConnect);
		ret.append('\n');
		ret.append("Discard level: "+discardLevel);
		ret.append('\n');
		System.out.println(ret.toString());
	}

	/**
	 * Getter
	 */
	public boolean getHelp() {
		return help;
	}

	/**
	 * Getter
	 */
	public boolean isUnlimited() {
		return unlimited;
	}

	/**
	 * Getter
	 */
	public boolean isDoNotConnect() {
		return doNotConnect;
	}

	/**
	 * Getter
	 */
	public String getFilterFileName() {
		return filterFileName;
	}

	/**
	 * Getter
	 */
	public String getEngineFilterFileName() {
		return engineFilterFileName;
	}

	/**
	 * Getter
	 */
	public LogTypeHelper getDiscardLevel() {
		return discardLevel;
	}

	/**
	 * Getter
	 */
	public String getFileToLoad() {
		return fileToLoad;
	}

	/**
	 * Getter
	 */
	public AudienceInfo getAudience() {
		return audience;
	}
}
