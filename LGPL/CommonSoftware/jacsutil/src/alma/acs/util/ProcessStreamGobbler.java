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
package alma.acs.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Continuously reads the stdout and stderr streams of {@link Process} in separate threads, 
 * so that the OS will not block out the JVM, as it could otherwise happen if lines are read from these streams sequentially in one thread.
 * <p>
 * This class is only useful if the external process's output is either not interesting at all for the user,
 * or if the output should be read only after the process has terminated. 
 * Otherwise the streams should be read directly without using this class.
 * <p>
 * The {@link #gobble(long, TimeUnit)} call returns either when both stdout and stderr streams deliver a null, 
 * or if the timeout is reached. @TODO check if relying on the null from both streams is
 * as robust as {@link Process#waitFor()}.
 * 
 * @author hsommer
 */
public class ProcessStreamGobbler 
{
	private final Process proc;
	private final List<String> stdout;
	private final List<String> stderr;
	private final ThreadFactory tf;
	private GobblerRunnable runErr;
	private GobblerRunnable runOut;
	private volatile boolean DEBUG = false;

	/**
	 * @param proc The process whose streams we want to read.
	 * @param tf ThreadFactory to be used to create the two threads that read the stdout and stderr streams
	 * @param storeStreamContent If true, then the stdout and stderr content will be stored 
	 *        and can be read using {@link #getStdout()} and {@link #getStderr()} 
	 * @TODO: set max sizes for the buffers
	 */
	public ProcessStreamGobbler(Process proc, ThreadFactory tf, boolean storeStreamContent) {
		this.proc = proc;
		this.tf = tf;
		if (storeStreamContent) {
			stdout = new ArrayList<String>();
			stderr = new ArrayList<String>();
		}
		else {
			stdout = null;
			stderr = null;
		}
	}

	/**
	 * Starts fetching process output from stdout/stderr, storing it internally
	 * so that it can later be read via {@link #getStdout()} and {@link #getStderr()}.
	 * <p>
	 * Use this method if you do not want to wait for the process to end.
	 * The status can anyway be checked using {@link #hasTerminated()}.
	 * 
	 * @param timeout maximum time to wait for the process to finish
	 * @param unit unit for the timeout
	 * @return  <code>true</code> if returning because the process ended, 
	 *          otherwise <code>false</code> if the timeout applied (in which case the streams will continue to be read though).
	 * @throws InterruptedException 
	 */
	public void gobbleAsync() throws InterruptedException {
		gobble(-1, null);
	}

	/**
	 * Starts fetching process output from stdout/stderr, storing it internally
	 * so that it can later be read via {@link #getStdout()} and {@link #getStderr()}.
	 * <p>
	 * Use this method if you want to wait for the process to end, limited by a timeout.
	 * <p>
	 * @TODO: set max sizes for the buffers
	 * 
	 * @param timeout maximum time to wait for the process to finish
	 * @param unit unit for the timeout
	 * @return  <code>true</code> if returning because the process ended, 
	 *          otherwise <code>false</code> if the timeout applied (in which case the streams will continue to be read though).
	 * @throws InterruptedException 
	 */
	public boolean gobble(long timeout, TimeUnit unit) throws InterruptedException {
		ExecutorService exsrv = new ThreadPoolExecutor(0, 2, 0L, TimeUnit.MILLISECONDS,
				new LinkedBlockingQueue<Runnable>(), tf);

		runOut = new GobblerRunnable(proc.getInputStream(), stdout, "stdout", DEBUG);
		exsrv.submit(runOut);
		runErr = new GobblerRunnable(proc.getErrorStream(), stderr, "stderr", DEBUG);
		exsrv.submit(runErr);
		
		if (timeout > 0) {
			exsrv.shutdown();
			return exsrv.awaitTermination(timeout, unit);
		}
		else {
			return false;
		}
	}

	/**
	 * Returns the stdout data read, or null if storeStreamContent==false 
	 */
	public List<String> getStdout() {
		return stdout;
	}
	
	/**
	 * Returns the stderr data read, or null if storeStreamContent==false 
	 */
	public List<String> getStderr() {
		return stderr;
	}
	
	/**
	 * @return true if there was 1 or more errors reading the stdout or stderr stream.
	 * @throws IllegalStateException if called before {@link #gobble(long, TimeUnit)} or {@link #gobbleAsync()}.
	 */
	public boolean hasStreamReadErrors() {
		if (runOut == null || runErr == null) {
			throw new IllegalStateException("Cannot call this method before gobbling.");
		}
		return (runOut.hasReadError | runErr.hasReadError);
	}
	
	public boolean hasTerminated() {
		if (runOut == null || runErr == null) {
			throw new IllegalStateException("Cannot call this method before gobbling.");
		}
		return (runOut.hasTerminated | runErr.hasTerminated);
	}
	
	public void setDebug(boolean DEBUG) {
		this.DEBUG = DEBUG;
	}
	
	private static class GobblerRunnable implements Runnable {
		
		private final BufferedReader br;
		private final List<String> buffer;
		private boolean hasReadError;
		private boolean hasTerminated;
		private String name;
		private final boolean DEBUG;
		
		GobblerRunnable(InputStream stream, List<String> buffer, String name, boolean DEBUG) {
			br = new BufferedReader(new InputStreamReader(stream));
			this.buffer = buffer;
			hasReadError = false;
			hasTerminated = false;
			this.name = name;
			this.DEBUG = DEBUG;
		}

		public void run() {
			try {
				String line = null;
				while ((line = br.readLine()) != null) {
					if (DEBUG) {
						System.out.println(name + ": "+ line);
					}
					if (buffer != null) {
						buffer.add(line);
					}
				}
				if (DEBUG) {
					System.out.println(name + ": done reading");
				}
			} 
			catch (IOException ioe) {
				ioe.printStackTrace();
				hasReadError = true;
			}
			finally {
				// close the BufferedReader and underlying stream.
				// It should only be closed in this thread, as otherwise 
				// we can get a deadlock if this thread waits on br.readLine() and the other on 
				// the related lock in BufferedReader#close
				try {
					br.close();
					if (DEBUG) {
						System.out.println(name + ": streams closed.");
					}
				} 
				catch (IOException ex) {
					if (DEBUG) {
						System.out.println(name + ": streams failed to close.");
					}
				}
				finally {
					hasTerminated = true;
				}
			}
		}
	}
}
