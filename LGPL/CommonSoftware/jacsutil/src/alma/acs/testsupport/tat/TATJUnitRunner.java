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
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import junit.framework.Test;
import junit.framework.TestResult;
import junit.framework.TestSuite;
import junit.textui.ResultPrinter;
import junit.textui.TestRunner;

/**
 * Replacement for <code>junit.textui.TestRunner</code> 
 * that helps avoid some issues with using JUnit 
 * embedded in ALMA TAT scripts.
 * <p>
 * Note that the intent is not to get rid of the output-based testing that TAT offers,
 * but to complement it with result-based testing as JUnit proposes it, 
 * and to make the latter easier in a TAT environment.
 * Both approaches have their merit, so chose one depending on what you want to test.
 * <p>
 * Internally calls {@link TestRunner#doRun(Test)}, controlling the 
 * {@link ResultPrinter} output as well as <code>System.out</code> 
 * and <code>System.err</code>.
 * <p>
 * Fixes the following issues
 * <ol>
 * <li>If the tests succeed, <code>TestRunner</code> produces too much output,
 * 		which forces test developers to provide TAT with reference output files
 * 		of all successful JUnit test runs, which seems absurd given that JUnit
 * 		does all error checks already itself.<br>
 * 		<code>TATJUnitRunner</code> produces no output for successful tests,
 * 		which means it suppresses
 * 		<ul>
 * 		<li><code>TestRunner</code>s output
 * 		<li>any other output to <code>System.out</code>
 * 		<li>any other output to <code>System.err</code>
 * 		</ul>
 * 		Both <code>TestRunner</code> and <code>TATJUnitRunner</code> 
 * 		return an exit code <code>0</code> to indicate a successful run.
 * <li>If a test fails (either JUnit "failure" or "error"),
 * 		<code>TestRunner</code> reports this in the execution summary, 
 * 		without giving any details on the problem, e.g. the assertion
 * 		that caused the failure.<br>
 * 		<code>TATJUnitRunner</code> in this case becomes rather verbose
 * 		and dumps on <code>System.err</code>
 * 		<ul>
 * 		<li>the detailed <code>TestRunner</code> output 
 * 			using a <code>ResultPrinter</code>.
 * 		<li>any output to <code>System.out</code> during test execution
 * 		<li>any output to <code>System.err</code> during test execution
 * 		</ul>
 * 		Both <code>TestRunner</code> and <code>TATJUnitRunner</code> 
 * 		return an exit code <code>1</code> for a failure and
 * 		<code>2</code> for an error.
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
	private static PrintStream s_oldSysOut = System.out;
	private static PrintStream s_oldSysErr = System.err;

	private static FileOutputStream s_fileoutstream;
	private static FileOutputStream s_fileerrstream;

	private static FileOutputStream s_fileresultstream;

	private static String outFilename;
	private static String errFilename;
	private static String resFilename;

	private static void redirectSysOutputStreams() throws FileNotFoundException
	{
		s_oldSysOut = System.out;		
		s_fileoutstream = new FileOutputStream(outFilename);
		System.setOut(new PrintStream(s_fileoutstream,true));

		s_oldSysErr = System.err;
		s_fileerrstream = new FileOutputStream(errFilename);
		System.setErr(new PrintStream(s_fileerrstream,true));
	}
	
	private static void restoreSysOutputStreams()
	{
		System.setOut(s_oldSysOut);
		System.setErr(s_oldSysErr);
	}

	private static void closeFiles() {
		try {
			s_fileoutstream.close();
		} catch (IOException ex) {
			System.err.print("Couldn't close file:" + ex.getMessage());
		}
		try {
			s_fileerrstream.close();
		} catch (IOException ex) {
			System.err.print("Couldn't close file:" + ex.getMessage());
		}
		try {
			s_fileresultstream.close();
		} catch (IOException ex) {
			System.err.print("Couldn't close file:" + ex.getMessage());
		}
	}
	
	/**
	 * Runs a suite extracted from a TestCase subclass.
	 * 
	 * Redirects System.out and System.err so that successful test runs will not
	 * produce any output, while in case of failure/error they get dumped to
	 * System.err.
	 */
	public static void run(Class testClass) 
            throws FileNotFoundException
	{
		//s_resultstream = new StringOutputStream();
		s_fileresultstream = new FileOutputStream(resFilename);
		redirectSysOutputStreams();
	
		Test suite = null;
		
		// try static method suite()
		try
		{
			Method suiteMethod = testClass.getMethod("suite", (Class[]) null);
			if (Modifier.isStatic(suiteMethod.getModifiers()))
			{
				suite = (Test) suiteMethod.invoke(null, (Object[]) null);
			}
		}
		catch (Exception e)
		{
		}
	
		// if not suite(), then extract "testXXX" methods...
		if (suite == null)
		{
			suite = new TestSuite(testClass);
		}
		
		run(suite);	
	}

	
	/**
	 * Note that if this method should become public in the future (and thus can be called directly, 
	 * rather than from {@link #run(Class)}, the handling of the output streams
	 * and the like must be shared properly between the two run methods.  
	 * @param suite
	 */
	private static void run(Test suite)
	{
		ResultPrinter resultPrinter = new ResultPrinter(new PrintStream(s_fileresultstream,true));
		TestRunner testRunner = new TestRunner(resultPrinter);

		TestResult r = null;
		try
		{
			r = testRunner.doRun(suite);
		}
		catch (Exception ex)
		{
			// not for "Errors" as opposed to "Failures",
			// but really mean exceptions thrown inside the JUnit framework,
			// which should never happen
			restoreSysOutputStreams();
			System.err.println("Exception was thrown during test execution: ");
			ex.printStackTrace();

			closeFiles();
			traceOutput(false);

			System.exit(TestRunner.EXCEPTION_EXIT);
		}

		restoreSysOutputStreams();

		// in either case print a summary which automated tools can analyze (SPR ALMASW2005121)
		int total = r.runCount();
		int success = total - r.failureCount() - r.errorCount();
		String runnerReport = "TEST_RUNNER_REPORT success/total: " + success + "/" + total;
		System.out.println(runnerReport);

		if (r.wasSuccessful()) {
			System.out.println("JUnit test run succeeded");
			closeFiles();
			File file = new File(outFilename);
			file.delete();
			file = new File(errFilename);
			file.delete();
			file = new File(resFilename);
			file.delete();
		} 
		else {
			System.err
					.println("JUnitRunner: Errors and/or failures during test execution!\n");
			closeFiles();
			traceOutput(true);

			System.exit(TestRunner.FAILURE_EXIT);
		}
		
	}


	private static void traceOutput(boolean isFailure) {
		byte buffer[] = new byte[1024];
		int status = -1;
		boolean error = false;
		try {
			FileInputStream resultFile = new FileInputStream(resFilename);
			System.err.println("Test execution trace: ");
			System.err.println("<<<<<<<<<<<<<<<<<<<");
			//System.err.println(s_resultstream.toString());
			do {
				try {
					status = resultFile.read(buffer);
				} catch (IOException ex) {
					error = true;
					System.err.print("Error reading file:" + ex.getMessage());
				}
				if (status > 0)
					System.err.write(buffer, 0, status);
			} while (status > 0);
			System.err.println(">>>>>>>>>>>>>>>>>>>");
			System.err.println();
			try {
				resultFile.close();
			} catch (IOException ex) {
				error = true;
				System.err.print("Couldn't close file:" + ex.getMessage());
			}
		} catch (FileNotFoundException ex) {
			error = true;
			System.err.print("Error opening file:" + ex.getMessage());
		}
		if (!error) {
			File file = new File(resFilename);
			file.delete();
		}
		error = false;

		try {
			FileInputStream stdoutFile = new FileInputStream(outFilename);
			System.err.println("System.out during test execution: ");
			System.err.println("<<<<<<<<<<<<<<<<<<<");
			//System.err.println(s_outstream.toString());
			do {
				try {
					status = stdoutFile.read(buffer);
				} catch (IOException ex) {
					error = true;
					System.err.print("Error reading file:" + ex.getMessage());
				}
				if (status > 0)
					System.err.write(buffer, 0, status);
			} while (status > 0);
			System.err.println(">>>>>>>>>>>>>>>>>>>");
			System.err.println();
			try {
				stdoutFile.close();
			} catch (IOException ex) {
				error = true;
				System.err.print("Couldn't close file:" + ex.getMessage());
			}

		} catch (FileNotFoundException ex) {
			error = true;
			System.err.print("Error opening file:" + ex.getMessage());
		}
		if (!error) {
			File file = new File(outFilename);
			file.delete();
		}
		error = false;

		try {
			FileInputStream stderrFile = new FileInputStream(errFilename);
			System.err.println("System.err during test execution: ");
			System.err.println("<<<<<<<<<<<<<<<<<<<");
			//System.err.println(s_errstream.toString());
			do {
				try {
					status = stderrFile.read(buffer);
				} catch (IOException ex) {
					error = true;
					System.err.print("Error reading file:" + ex.getMessage());
				}
				if (status > 0)
					System.err.write(buffer, 0, status);
			} while (status > 0);
			System.err.println(">>>>>>>>>>>>>>>>>>>");
			try {
				stderrFile.close();
			} catch (IOException ex) {
				error = true;
				System.err.print("Couldn't close file:" + ex.getMessage());
			}
		} catch (FileNotFoundException ex) {
			error = true;
			System.err.print("Error opening file:" + ex.getMessage());
		}
		if (!error) {
			File file = new File(errFilename);
			file.delete();
		}

	}


	public static void main(String[] args)
	{
		try
		{
			Class testClass = null;
			
			if (args.length < 1 || args[0] == null || 
				args[0].length() == 0)
			{
				System.err.println("usage: JUnitRunner testclassname");
			}
			else
			{
				String testClassName = args[0];
				outFilename = "stdout-" + testClassName + ".log";
				errFilename = "stderr-" + testClassName + ".log";
				resFilename = "result-" + testClassName + ".log";
				testClass = Class.forName(testClassName);
			}
			
			run(testClass);
		} catch (FileNotFoundException ex) {
			System.err.print("Error opening file:" + ex.getMessage());
			System.exit(TestRunner.EXCEPTION_EXIT);
		} catch (Throwable thr) {
			thr.printStackTrace();
			System.exit(TestRunner.EXCEPTION_EXIT);
		}
		
		System.exit(TestRunner.SUCCESS_EXIT);
	}
}
