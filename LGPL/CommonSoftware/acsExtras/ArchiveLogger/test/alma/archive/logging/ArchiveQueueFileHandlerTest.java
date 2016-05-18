package alma.archive.logging;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Arrays;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * 
 * @author almadev
 */
public class ArchiveQueueFileHandlerTest extends TestCase {
	private final static Logger LOG = Logger.getLogger(ArchiveQueueFileHandlerTest.class.getSimpleName());
	private final static File LOG_DIR = new File("logs");
	
	/**
	 * 
	 */
	public ArchiveQueueFileHandlerTest() {
		LOG.setUseParentHandlers(false);
		LOG.setLevel(Level.FINER);
		LOG.addHandler(new Handler() {

			@Override
			public void publish(LogRecord record) {
				String string = record.getLevel() + " [Thread-" + record.getThreadID() + "]";
				string += " " + record.getSourceClassName().substring(record.getSourceClassName().lastIndexOf('.') + 1) + "." + record.getSourceMethodName();
				string += ": " + record.getMessage();
				System.out.println(string);
			}

			@Override
			public void flush() {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void close() throws SecurityException {
				// TODO Auto-generated method stub
				
			}
			
		});
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetNewFile() throws Exception {
		final String filenamePattern = "log\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}_YYYY-MM-DDTHH:MM:SS\\.mmm\\.xml";
		ArchiveQueueFileHandler handler = new ArchiveQueueFileHandler(LOG, LOG_DIR.getAbsolutePath(), 1, 1025L);
		File f = handler.getNewFile();
		Assert.assertNotNull(f);
		Assert.assertTrue(f.getName(), f.getName().matches(filenamePattern));
		LOG.info(f.getAbsolutePath());
	}
	
	/**
	 * 
	 * @throws Exception
	 */
	public void testCloseFile() throws Exception {
		File oldLogFile = new File(LOG_DIR, "log1970-01-21T00:00:00.000_YYYY-MM-DDTHH:MM:SS.mmm.xml");
		// touch the log file
		try (FileOutputStream ofs = new FileOutputStream(oldLogFile)) {;;}
		ArchiveQueueFileHandler handler = new ArchiveQueueFileHandler(LOG, LOG_DIR.getAbsolutePath(), 1, 1025L);
		String earliestLogTimestamp = "2014-12-06T13:00:00.000";
		String lastLogTimestamp = "2014-12-06T15:12:34.567";
		handler.fileProcessed(oldLogFile, earliestLogTimestamp, lastLogTimestamp);
		Assert.assertFalse(oldLogFile.exists());
		File newLogFile = new File(LOG_DIR, String.format("log%s_%s.xml", earliestLogTimestamp, lastLogTimestamp));
		Assert.assertTrue(Arrays.toString(LOG_DIR.list()), newLogFile.exists());
	}
	
	/**
	 * 
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		LOG_DIR.mkdir();
	}

	/**
	 * 
	 */
	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
		for (File nextFile: LOG_DIR.listFiles()) {
			nextFile.delete();
		}
		LOG_DIR.delete();
	}
	
}
