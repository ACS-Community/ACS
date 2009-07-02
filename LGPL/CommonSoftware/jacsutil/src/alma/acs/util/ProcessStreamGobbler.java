package alma.acs.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
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
	 * @TODO: set max sizes for the buffers
	 * @param timeout
	 * @param unit
	 * @return  true if returning because of process ended, 
	 *          otherwise false if the timeout applied (in which case the streams will continue to be read though).
	 * @throws InterruptedException 
	 */
	public boolean gobble(long timeout, TimeUnit unit) throws InterruptedException {
		ExecutorService exsrv = Executors.newFixedThreadPool(2, tf);
		runOut = new GobblerRunnable(proc.getInputStream(), stdout, "stdout", DEBUG);
		exsrv.submit(runOut);
		runErr = new GobblerRunnable(proc.getErrorStream(), stderr, "stderr", DEBUG);
		exsrv.submit(runErr);
		exsrv.shutdown();
		return exsrv.awaitTermination(timeout, unit);
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
	
	public boolean hasStreamReadErrors() {
		return (runOut.hasReadError | runErr.hasReadError);
	}
	
	public void setDebug(boolean DEBUG) {
		this.DEBUG = DEBUG;
	}
	
	private static class GobblerRunnable implements Runnable {
		
		private final BufferedReader br;
		private final List<String> buffer;
		private boolean hasReadError;
		private String name;
		private final boolean DEBUG;
		
		GobblerRunnable(InputStream stream, List<String> buffer, String name, boolean DEBUG) {
			br = new BufferedReader(new InputStreamReader(stream));
			this.buffer = buffer;
			hasReadError = false;
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
				} catch (IOException ex) {
					if (DEBUG) {
						System.out.println(name + ": streams failed to close.");
					}
				}
			}
		}
	}
}
