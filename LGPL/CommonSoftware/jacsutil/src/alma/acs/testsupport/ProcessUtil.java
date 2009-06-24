package alma.acs.testsupport;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Helper class to be used by tests which need to verify that some other process
 * exists or does not exist, or that need to kill some process.
 * <b>Never use this class in operational code!</b>
 * <p>
 * @TODO If we see problems with blocking streams from Runtime.exec,
 * apply tricks from 
 * http://www.javaworld.com/javaworld/jw-12-2000/jw-1229-traps.html
 * http://www.velocityreviews.com/forums/t294249-running-special-programs-through-runtime-exec.html
 */
public class ProcessUtil
{
	private final Logger logger;

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
		logger.info("Will kill process using command '" + command + "'.");
		
		ProcessBuilder pb = new ProcessBuilder(command);
		Process killProc = pb.start();
		return killProc.waitFor();
	}
	
	
	/**
	 * Gets a map with running java main classes and list of the process IDs.
	 * Filters out sun.tools.jps.Jps which is the tool used to get the processes.
	 * @return Map<classname, pid-list>
	 * @throws IOException 
	 */
	protected Map<String, List<String>> getJavaPIDs() throws IOException {
		// The following command returns lines of the format
		// 23551 com.cosylab.acs.maci.manager.app.Manager
		// 29113 sun.tools.jps.Jps
		String command = "jps -l";

		Map<String, List<String>> pidMap = new HashMap<String, List<String>>();

		Runtime rt = Runtime.getRuntime();
		BufferedReader br = new BufferedReader(new InputStreamReader(rt.exec(command).getInputStream()));
		String line = null;
		String[] splitLine = null;
		while ((line = br.readLine()) != null) {
			if (line.length() > 0 && 
				(splitLine = line.split(" ")).length == 2) {
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
		br.close();
		return pidMap;
	}
	
}

