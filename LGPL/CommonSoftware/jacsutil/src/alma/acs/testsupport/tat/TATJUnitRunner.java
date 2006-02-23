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

import java.io.PrintStream;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import junit.framework.Test;
import junit.framework.TestResult;
import junit.framework.TestSuite;
import junit.textui.ResultPrinter;
import junit.textui.TestRunner;

import alma.acs.util.StringOutputStream;

/**
 * Replacement for <code>junit.textui.TestRunner</code> 
 * that helps avoid some issues with using JUnit 
 * embedded in ALMA TAT scripts.
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

	private static StringOutputStream s_outstream;
	private static StringOutputStream s_errstream;
	
	private static StringOutputStream s_resultstream;
	

	private static void redirectSysOutputStreams()
	{
		s_oldSysOut = System.out;		
		s_outstream = new StringOutputStream();
		System.setOut(new PrintStream(s_outstream));

		s_oldSysErr = System.err;
		s_errstream = new StringOutputStream();
		System.setErr(new PrintStream(s_errstream));
	}
	
	private static void restoreSysOutputStreams()
	{
		System.setOut(s_oldSysOut);
		System.setErr(s_oldSysErr);
	}

	
	/**
	 * Runs a suite extracted from a TestCase subclass.
	 * 
	 * Redirects System.out and System.err 
	 * so that successful test runs will not produce any output,
	 * while in case of failure/error they get dumped to System.err. 
	 */
	public static void run(Class testClass) 
	{
		s_resultstream = new StringOutputStream();
		redirectSysOutputStreams();
	
		Test suite = null;
		
		// try static method suite()
		try
		{
			Method suiteMethod = testClass.getMethod("suite", null);
			if (Modifier.isStatic(suiteMethod.getModifiers()))
			{
				suite = (Test) suiteMethod.invoke(null, null);
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
		ResultPrinter resultPrinter = new ResultPrinter(
			new PrintStream(s_resultstream));
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
		}
		else {
			System.err.println("JUnitRunner: Errors and/or failures during test execution!\n");
			traceOutput(true);

			System.exit(TestRunner.FAILURE_EXIT); 
		}
		
	}


	private static void traceOutput(boolean isFailure)
	{
		System.err.println("Test execution trace: ");
		System.err.println("<<<<<<<<<<<<<<<<<<<");
		System.err.println(s_resultstream.toString());
		System.err.println(">>>>>>>>>>>>>>>>>>>");
		System.err.println();
		
		System.err.println("System.out during test execution: ");
		System.err.println("<<<<<<<<<<<<<<<<<<<");
		System.err.println(s_outstream.toString());
		System.err.println(">>>>>>>>>>>>>>>>>>>");
		System.err.println();

		System.err.println("System.err during test execution: ");
		System.err.println("<<<<<<<<<<<<<<<<<<<");
		System.err.println(s_errstream.toString());
		System.err.println(">>>>>>>>>>>>>>>>>>>");
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
				testClass = Class.forName(testClassName);
			}
			
			run(testClass);
		}
		catch (Throwable thr)
		{
			thr.printStackTrace();
			System.exit(TestRunner.EXCEPTION_EXIT);
		}
		
		System.exit(TestRunner.SUCCESS_EXIT);
	}
}
