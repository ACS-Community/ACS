package alma.acs.testsupport;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.util.ProcessStreamGobbler;

public class ProcessUtilTest extends TestCase
{
	private ProcessUtil processUtil;
	private Logger logger;

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		logger = TestLogger.getLogger(ProcessUtilTest.class.getName());
		processUtil = new ProcessUtil(logger);
		processUtil.setDebug(false); // toggle this for debugging
	}
	
	/**
	 * Tests ProcesUtil with java processes
	 */
	public void testJava() throws Exception {
		
		Class<?> testClass = ProcessUtilTestDummy.class;
		assertFalse(processUtil.isJavaProcessRunning(testClass));
		
		// run dummy process
		Process proc = runDummyProcess();
		ProcessStreamGobbler gobbler = new ProcessStreamGobbler(proc, new DaemonThreadFactory(), true);
		assertFalse("Dummy java process is supposed to still run after 3.5 seconds", gobbler.gobble(3500, TimeUnit.MILLISECONDS));
		assertTrue(processUtil.isJavaProcessRunning(testClass));
		List<String> stdout = gobbler.getStdout();
		int msgCount = 0;
		for (String msg : stdout) {
			if (msg.startsWith("All is well ")) { // ignore other lines with acsStartJava script output
				msgCount++;
			}
		}
		assertEquals("Expected two 'All is well' messages in 3.5 seconds (after ~ 0 and 2 seconds).", 2, msgCount);
		List<String> stderr = gobbler.getStderr();
		assertEquals(0, stderr.size());
		
		// get PID of dummy process	
		List<String> pidList = processUtil.getJavaPIDs(testClass);
		assertEquals("Expected to find one running instance of " + testClass.getName() + ".", 1, pidList.size());
		logger.info("Found single dummy java class with PID=" + pidList.get(0));
		
		// kill process
		processUtil.killProcess(pidList.get(0), true);
		assertFalse(processUtil.isJavaProcessRunning(testClass));
	}
	
	private Process runDummyProcess() throws Exception {
		String command = "acsStartJava "+ ProcessUtilTestDummy.class.getName();
		Process proc = Runtime.getRuntime().exec(command);
		return proc;
	}
}
