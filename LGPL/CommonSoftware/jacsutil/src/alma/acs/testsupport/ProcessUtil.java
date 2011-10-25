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
package alma.acs.testsupport;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.util.ProcessStreamGobbler;

/**
 * Helper class to be used by tests which need to verify that some other process exists 
 * or does not exist, or that need to kill some process.
 * <b>Don't use this class in operational code!</b>
 */
public class ProcessUtil
{
	private final Logger logger;
	private volatile boolean DEBUG = false;

	public ProcessUtil(Logger logger) {
		this.logger = logger;
	}

	/**
	 * Gets the process IDs of all java processes run by the current user on the local host, 
	 * whose main class is given as the parameter.
	 * <p>
	 * The "jps" command is used via Runtime.exec, which should work not only on Linux 
	 * but also on Windows.
	 * 
	 * @throws IOException
	 */
	public List<String> getJavaPIDs(Class<?> mainClass) throws IOException {
		Map<String, List<String>> pidMap = getJavaPIDs();
		List<String> pidList = pidMap.get(mainClass.getName());
		if (pidList == null) {
			pidList = new ArrayList<String>();
		}
		return pidList;
	}
	
	/**
	 * Returns true if one or more JVMs are running the given Java main class 
	 * on the local host as the current user.
	 * @throws IOException
	 */
	public boolean isJavaProcessRunning(Class<?> mainClass) throws IOException {
		return ((getJavaPIDs(mainClass)).size() > 0);
	}
	
	/**
	 * Kills a process with a given PID, which could be obtained from {@link #getJavaPIDs(Class)}.
	 * <p>
	 * @TODO Currently this method works only on Linux because it uses the "kill -9" command.
	 * 
	 * @TODO Currently the stdout and stderr from running "kill" are not gobbled because 
	 *       they are expected to be very small and thus won't block buffers etc. 
	 *       If they do, then use {@link ProcessStreamGobbler} also here. 
	 * 
	 * @param tough if true, a tough way of killing is used (kill -9)
	 * @return Exit value of the kill command
	 * @throws IOException 
	 * @throws InterruptedException 
	 */
	public int killProcess(String pid, boolean tough) throws IOException, InterruptedException {
		String command = "kill ";
		if (tough) {
			command += "-9 ";
		}
		command += pid;
		logger.info("Will kill process " + pid + " using command '" + command + "'.");
		
		Process killProc = Runtime.getRuntime().exec(command);
		return killProc.waitFor();
	}
	
	
	/**
	 * Gets a map with key=(running java main classes) and value=(list of the process IDs).
	 * Filters out sun.tools.jps.Jps which is the tool used to get the processes.
	 * @return Map<classname, pid-list>
	 * @throws IOException 
	 * @throws InterruptedException 
	 */
	protected Map<String, List<String>> getJavaPIDs() throws IOException {
		// The following command returns lines of the format
		// 23551 com.cosylab.acs.maci.manager.app.Manager
		// 29113 sun.tools.jps.Jps
		String command = "jps -l";
		Process proc = Runtime.getRuntime().exec(command);
		ProcessStreamGobbler gob = new ProcessStreamGobbler(proc, new DaemonThreadFactory(), true);
		gob.setDebug(DEBUG);
		try {
			// read stdout and stderr
			if (!gob.gobble(10, TimeUnit.SECONDS)) {
				throw new IOException("Failed to execute command '" + command + "' within 10 seconds");
			}
			if (gob.hasStreamReadErrors()) {
				throw new IOException("Failed to read output of command '" + command + "'");
			}
		} catch (InterruptedException ex) {
			throw new IOException("Thread reading output of command '" + command + "' got interrupted.");
		} 
		// evaluate jps output
		Map<String, List<String>> pidMap = new HashMap<String, List<String>>();
		
		List<String> outlines = gob.getStdout();
		String[] splitLine = null;
		for (String line : outlines) {
			if (line.length() > 0 && (splitLine = line.split(" ")).length == 2) {
				String cname = splitLine[1];
				if (!"sun.tools.jps.Jps".equals(cname)) {
					String pid = splitLine[0];
					List<String> pidList = pidMap.containsKey(cname) ? pidMap.get(cname) : new ArrayList<String>();
					pidList.add(pid);
					pidMap.put(cname, pidList);
				}
			}
			else {
				logger.info("jps returned unexpected line '" + line + "'");
			}
		}
		return pidMap;
	}
	
	public void setDebug(boolean DEBUG) {
		this.DEBUG = DEBUG;
	}
}

