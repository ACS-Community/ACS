/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.util.stringqueue;

import java.io.File;
import java.io.IOException;
import java.util.Random;

/**
 * A default implementation of {@link IStringQueueFileHandler} to create 
 * and delete cache files locally.
 * <P>
 * <code>DefaultQueueFileHandlerImpl</code> should be used when the queue 
 * contains plain strings or, more in general, when the files of the queue
 * do not contain a header and a footer as it happens for example if the files
 * contain XML strings. In this last case, prefer {@link DefaultXmlQueueFileHandler}
 * 
 * @author acaproni
 * @since ACS 9.0
 */
public class DefaultQueueFileHandlerImpl extends TimestampedStringQueueFileHandler {
	
	/**
	 * The random number generator
	 */
	private final Random randomNumGenerator = new Random(System.currentTimeMillis());
	
	/**
	 * Build the handler with the default size  and prefix.
	 * 
	 * @see StringQueueFileHandler
	 */
	public DefaultQueueFileHandlerImpl() {
		super();
	}
	
	/**
	 * Build the handler with the passed size for the files.
	 * 
	 * @param maxFileSize The max size of the files of the cache.
	 * @see StringQueueFileHandler
	 */
	public DefaultQueueFileHandlerImpl(long maxFileSize) {
		super(maxFileSize);
	}
	
	/**
	 * Build the handler with the default size for the files
	 * and the passed prefix
	 * 
	 * @param prefix The prefix of the name of the cache files
	 * @see StringQueueFileHandler
	 */
	public DefaultQueueFileHandlerImpl(String prefix) {
		super(prefix);
	}
	
	/**
	 * Build the handler with the passed size and prefix.
	 * 
	 * @param maxFileSize The max size of the files of the cache.
	 * @param prefix The prefix of the name of the cache files
	 * @see StringQueueFileHandler
	 */
	public DefaultQueueFileHandlerImpl(long maxFileSize, String prefix) {
		super(maxFileSize,prefix);
	}

	/** 
	 * @see alma.acs.util.stringqueue.IStringQueueFileHandler#fileProcessed(java.io.File, java.lang.String, java.lang.String)
	 */
	@Override
	public void fileProcessed(File filePointer, String minTime, String maxTime) {
		if (filePointer==null) {
			throw new IllegalArgumentException("The file can't be null");
		}
		if (!filePointer.delete()) {
			System.err.println("Error deleting "+filePointer.getAbsolutePath());
		}
	}

	/**
	 * Attempts to create the file for the strings in several places
	 * before giving up.
	 * 
	 * @see alma.acs.util.stringqueue.IStringQueueFileHandler#getNewFile()
	 */
	@Override
	public File getNewFile() throws IOException {
		String name=null;
		File f=null;
		try {
			// Try to create the file in $ACS_TMP
			String acstmp = System.getProperty("ACS.tmp");
			if (!acstmp.endsWith(File.separator)) {
				acstmp=acstmp+File.separator;
			}
			File dir = new File(acstmp);
			f = File.createTempFile(prefix,".tmp",dir);
			name=f.getAbsolutePath();
		} catch (IOException ioe) {
			// An error :-O
			String homeDir = System.getProperty("user.dir");
			// Check if the home dir is writable
			File homeDirFile = new File(homeDir);
			if (homeDirFile.isDirectory() && homeDirFile.canWrite()) {
				do {
					// Try to create the file in the home directory
					int random = randomNumGenerator.nextInt();
					name = homeDir + File.separator+prefix+random+".jlog";
					f = new File(name);
				} while (f.exists());
			} else {
				// The home folder is not writable: try to get a system temp file
				f=File.createTempFile(prefix,".tmp");
				name=f.getAbsolutePath();
			}
		}
		if (f!=null) {
			f.deleteOnExit();
		}
		return f;
	}
}
