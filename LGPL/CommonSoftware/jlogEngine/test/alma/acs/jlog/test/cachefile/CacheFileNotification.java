/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.jlog.test.cachefile;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.LCEngine;
import com.cosylab.logging.engine.cache.ILogQueueFileHandler;
import com.cosylab.logging.engine.log.ILogEntry;

import org.omg.CORBA.ORB;

import si.ijs.maci.Manager;

/**
 * <code>CacheFileNotification</code> is a debugging tool that listens to logs using {@link LCEngine} 
 * and aims to produce files of logs through the {@link ILogQueueFileHandler} interface.
 * <P>   
 * <code>CacheFileNotification</code> never terminates unless you press CTRL-C.
 * <P>
 * Cache files are created in the local folder with a integer identifier. 
 * When the dates of the logs are given by the {@link ILogQueueFileHandler}, then the files are renamed.
 * <P>
 * The size of the cache, {@link #maxFileSize}, is set by default to a little number 
 * because we are interested in producing a big number of files.
 * 
 * @author acaproni
 * @since ACS 11.2
 */
public class CacheFileNotification implements ILogQueueFileHandler, ACSRemoteLogListener {
	
	/**
	 * The component client to connect to ACS
	 */
	private final AdvancedComponentClient client;
	
	/**
	 * ContainerServices
	 */
	private final ContainerServices contSvcs;
	
	/**
	 * The engine to get logs and files of the cache.
	 */
	private final LCEngine lcEngine;
	
	/**
	 * The prefix of the name of each cache file before being renamed.
	 * The name of each file is composed of the prefix, the ID and the postfix.
	 */
	private final String fileNamesPrefix="./TempFileCache_";
	
	/**
	 * The post of the name of each cache file before being renamed
	 */
	private final String fileNamesPostfix=".xml";
	
	/**
	 * The ID of each file
	 */
	private final AtomicInteger fileId = new AtomicInteger(0);
	
	/**
	 * The max size of each cache file
	 */
	private final long maxFileSize = 8192;
	
	/**
	 * We want to show a message with the number of processed logs from time to time just to 
	 * show that the system is alive.
	 */
	private volatile long lastMsgTime;
	
	/**
	 * The number of logs processed in the past interval
	 */
	private volatile int processedLgs=0;
	
	/**
	 * To avoid the process to halt
	 */
	private final CountDownLatch latch = new CountDownLatch(1);
	
	/**
	 * The logger
	 */
	private final AcsLogger logger;
	
	public CacheFileNotification() throws Exception {
		String clientName = getClass().getName();
        logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(clientName, true);
        String managerLoc = System.getProperty("ACS.manager").trim();

        client = new AdvancedComponentClient(logger, managerLoc, clientName);
        contSvcs=client.getContainerServices();
        lcEngine = new LCEngine(this);
	}
	
	/**
	 * Start the computation by connecting the engine to the logging NC.
	 */
	private void start() throws Exception {
		ORB orb=client.getAcsCorba().getORB();
		Manager manager=client.getAcsManagerProxy().getManager();
		lcEngine.addLogListener(this);
		lcEngine.connect(orb, manager);
	}

	
	
	/**
	 * Stop before exiting
	 */
	private void stop() {
		logger.log(AcsLogLevel.INFO,"Shutting down");
		lcEngine.disconnect(true);
		try {
			client.tearDown();
		} catch(Throwable t) {
			t.printStackTrace();
		}
		latch.countDown();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.logging.engine.cache.ILogQueueFileHandler#getNewFile()
	 */
	@Override
	public File getNewFile() throws IOException {
		String newFileName=fileNamesPrefix+fileId.getAndIncrement()+fileNamesPostfix;
		File newFile = new File(newFileName);
		return newFile;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.logging.engine.cache.ILogQueueFileHandler#fileProcessed(java.io.File, java.lang.String, java.lang.String)
	 */
	@Override
	public void fileProcessed(File filePointer, String minTime, String maxTime) {
		String finalFileName=minTime+"_"+maxTime+fileNamesPostfix;
		logger.log(AcsLogLevel.INFO,filePointer.getPath()+" processed. Renaming to "+finalFileName);
		File finalFile = new File(finalFileName);
		filePointer.renameTo(finalFile);
		logger.log(AcsLogLevel.INFO,"File successfully renamed");
	}

	/* (non-Javadoc)
	 * @see com.cosylab.logging.engine.cache.ILogQueueFileHandler#getMaxFileSize()
	 */
	@Override
	public long getMaxFileSize() {
		return maxFileSize;
	}
	
	public static void main(String[] args) {
		try {
			final CacheFileNotification cacheFileNotification= new CacheFileNotification();
			cacheFileNotification.start();
			cacheFileNotification.latch.await();
			Runtime.getRuntime().addShutdownHook(new Thread() {
			    public void run() {
			    	cacheFileNotification.stop();
			    }
			});
		} catch (Throwable t) {
			t.printStackTrace();
		}
	}

	/**
	 * We need to listen to logs to empty the cache so that files are released 
	 */
	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		processedLgs++;
		if (System.currentTimeMillis()-lastMsgTime>60000) {
			logger.log(AcsLogLevel.INFO,"Logs processed "+processedLgs);
			processedLgs=0;
			lastMsgTime=System.currentTimeMillis();
		}
	}
}
