/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.testsupport.tat;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import junit.runner.Version;

import org.junit.internal.JUnitSystem;
import org.junit.internal.TextListener;
import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.RunListener;

/**
 * Replacement for a standard JUnit runner such as JUnit3's <code>junit.textui.TestRunner</code>,
 * or JUnit4's {@link JUnitCore}, which helps avoid some issues with using JUnit embedded in ALMA TAT scripts.
 * <p>
 * Note that the intent is not to get rid of the output-based testing that TAT offers,
 * but to complement it with result-based testing as JUnit proposes it, 
 * and to make the latter easier in a TAT environment.
 * Both approaches have their merit, so chose one depending on what you want to test.
 * <p>
 * TATJUnitRunner internally calls {@link JUnitCore#run(Class...)}, controlling the {@link RunListener} output 
 * as well as <code>System.out</code> and <code>System.err</code>.
 * <p>
 * This class fixes the following issues
 * <ol>
 * <li>If the tests succeed, <code>JUnitCore</code> produces too much output,
 * 		leading to unnecessary maintenance work.<br>
 * 		<code>TATJUnitRunner</code> produces only very simple fixed output for successful tests,
 * 		which means it suppresses
 * 		<ul>
 * 		<li><code>JUnitCore</code> output
 * 		<li>any other output to <code>System.out</code>
 * 		<li>any other output to <code>System.err</code>
 * 		</ul>
 * 		Instead, it prints 2 fixed lines, as required by TAT / NRI for gathering test statistics.
 * 		Both <code>JUnitCore</code> and <code>TATJUnitRunner</code> 
 * 		return an exit code <code>0</code> to indicate a successful run.
 * <li>If a test fails (JUnit3: either "failure" or "error"),
 * 		<code>TestRunner</code> reports this in the execution summary, 
 * 		without giving enough details on the problem, e.g. the assertion
 * 		that caused the failure.<br>
 * 		<code>TATJUnitRunner</code> in this case becomes rather verbose
 * 		and dumps on <code>System.err</code>
 * 		<ul>
 * 		<li>the detailed test runner output.
 * 		<li>any output to <code>System.out</code> during test execution
 * 		<li>any output to <code>System.err</code> during test execution
 * 		</ul>
 * 		Both <code>JUnitCore</code> and <code>TATJUnitRunner</code> 
 * 		return an exit code <code>1</code> for a failure.
 * </ol>
 * For either failure or success, the following line is printed as the first line to stdout,
 * so that TAT or other tools can analyze how many tests have run, and how many succeeded: <br>
 * <code>TEST_RUNNER_REPORT success/total: int/int</code> <br>
 * where <code>int</code> is replaced by the respective integer number.
 * 
 * @author hsommer
 */
public class TATJUnitRunner 
{
	private PrintStream oldSysOut;
	private PrintStream oldSysErr;

	private File sysOutFile;
	private PrintStream newSysOut;

	private File sysErrFile;
	private PrintStream newSysErr;

	private File resultFile;
	private PrintStream resultOut;

	private JUnitCore delegate = new JUnitCore();
	
	/**
	 * true if the class under test has the {@link KeepTestOutput} annotation.
	 */
	private boolean keepTestOutput = false;


	private void createStreams(Class<?> testClass) throws IOException
	{
		sysOutFile = createTmpFile("sysout", testClass);
		newSysOut = new PrintStream(new FileOutputStream(sysOutFile, true));

		sysErrFile = createTmpFile("syserr", testClass);
		newSysErr = new PrintStream(new FileOutputStream(sysErrFile, true));
		
		resultFile = createTmpFile("result", testClass);
		resultOut = new PrintStream(new FileOutputStream(resultFile, true));
	}
	
	private void redirectSysStreams() throws IOException {
	
		oldSysOut = System.out;
		System.setOut(newSysOut);

		oldSysErr = System.err;
		System.setErr(newSysErr);
	}	

	
	private void restoreSysStreams() {
	
		System.setOut(oldSysOut);
		System.setErr(oldSysErr);
	}

	private void closeStreams() {
		newSysOut.close();
		newSysErr.close();
		resultOut.close();
	}
	


	/**
	 * Dumps the content of the specified file to System.err and, if all goes well, deletes that file.  
	 * @param source
	 * @param sectionHeader
	 */
	private void dumpAndDeleteCapturedFile(File source, String sectionHeader) {
		byte buffer[] = new byte[2048];
		int status = -1;
		boolean error = false;
		try {
			FileInputStream sourceStream = new FileInputStream(source);
			System.err.println(sectionHeader);
			System.err.println("<<<<<<<<<<<<<<<<<<<");
			do {
				try {
					status = sourceStream.read(buffer);
				} catch (IOException ex) {
					error = true;
					System.err.print("Error reading file: " + ex.getMessage());
				}
				if (status > 0)
					System.err.write(buffer, 0, status);
			} while (status > 0);
			System.err.println(">>>>>>>>>>>>>>>>>>>");
			System.err.println();
			try {
				sourceStream.close();
			} catch (IOException ex) {
				error = true;
				System.err.print("Couldn't close file: " + ex.getMessage());
			}
		} catch (FileNotFoundException ex) {
			error = true;
			System.err.print("Error opening file: " + ex.getMessage());
		}
		
		if (!error && !keepTestOutput) {
			source.delete();
		}
	}
	

	
	/**
	 * Modified version of {@link JUnitCore#runMain(JUnitSystem, String...)}.
	 * We restrict ourselves to running only one test class (or suite) at a time.
	 * This method is not thread-safe!
	 * @param testClass
	 * @return the result
	 * @throws IOException 
	 */
	public Result runMain(Class<?> testClass) throws IOException  {
		
		Result result = null;
		try {
			keepTestOutput = testClass.isAnnotationPresent(KeepTestOutput.class);
			createStreams(testClass);
			redirectSysStreams();
			JUnitSystem system = new JUnitSystem() {
				public void exit(int code) {
					System.exit(code);
				}
				public PrintStream out() {
					return resultOut;
				}
			};
			system.out().println("JUnit version " + Version.id());
			RunListener listener= new TextListener(system);
			delegate.addListener(listener); // or should we listen directly?
	
			result = delegate.run(testClass);
		}
		finally {
			restoreSysStreams();
			closeStreams();
		}
		
		
		// in either case print a summary which automated tools can analyze (SPR ALMASW2005121)
		int total = result.getRunCount();
		int success = total - result.getFailureCount();
		String runnerReport = "TEST_RUNNER_REPORT success/total: " + success + "/" + total;
		System.out.println(runnerReport);

		if (result.wasSuccessful()) {
			System.out.println("JUnit test run succeeded");
			if (!keepTestOutput) {
				sysOutFile.delete();
				sysErrFile.delete();
				resultFile.delete();
			}
		}
		else {
			System.err.println("JUnitRunner: Errors and/or failures during test execution!\n");
			// Dump the JUnit test runner output, the captured stdout, and the captured stderr of the text execution to System.err. 
			dumpAndDeleteCapturedFile(resultFile, "Test execution trace:");
			dumpAndDeleteCapturedFile(sysOutFile, "System.out during test execution:");
			dumpAndDeleteCapturedFile(sysErrFile, "System.err during test execution:");
		}

		return result;
	}

	
	/**
	 * @param prefix One of stdout, stderr, result
	 * @param testClass
	 * @return
	 * @throws IOException
	 */
	private File createTmpFile(String prefix, Class<?> testClass) throws IOException {
		File tmpFile = File.createTempFile(prefix + "-" + testClass.getSimpleName() + "-", ".log", new File(System.getProperty("user.dir")));
		return tmpFile;
	}
	
	
	/**
	 * @throws FileNotFoundException Hack, even other IOException will be wrapped by FileNotFoundException just for temporary backward compatibility. 
	 * @deprecated  This method is used for compatibility with the old JUnit-3 based version of this class. It will be removed after ACS 11.2.
	 */
	public static void run(Class testClass) throws FileNotFoundException {
		TATJUnitRunner inst = new TATJUnitRunner();
		try {
			inst.runMain(testClass);
		} catch (IOException ex) {
			ex.printStackTrace();
			throw new FileNotFoundException(ex.toString());
		}
	}
	
	
	public static void main(String[] args)
	{
		Result result = null;
		try {
			if (args.length < 1 || args[0] == null || args[0].length() == 0) {
				System.err.println("usage: JUnit4Runner testclassname");
			}
			else {
				TATJUnitRunner inst = new TATJUnitRunner();
				String testClassName = args[0];
				Class<?> testClass = Class.forName(testClassName);  // or should we first redirect the streams, in case of static initializers?
				result = inst.runMain(testClass);
			}
		} catch (Throwable thr) {
			thr.printStackTrace();
			System.exit(1); 
		}
		if (result.wasSuccessful()) {
			System.exit(0);
		}
		else {
			System.exit(1);
		}
	}

	/**
	 * Test classes can use this annotation to cause TATJUnitRunner to keep the 
	 * tmp files with the stdout, stderr and test output. 
	 * This has no influence on what output TAT sees, but allows checking the test output 
	 * which is normally not available any more after the test succeeds.
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(ElementType.TYPE)
	public static @interface KeepTestOutput {
	}

}
