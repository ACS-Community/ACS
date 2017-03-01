package alma.acs.xmlfilestore.test.logging;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.util.IsoDateFormat;
import alma.acs.xmlfilestore.common.QueueFileHandler;

/**
 * 
 * @author acaproni
 * @since 2016.6
 */
public class QueueFileHandlerTest extends ComponentClientTestCase {
	private final static Logger LOG = Logger.getLogger(QueueFileHandlerTest.class.getSimpleName());

	/**
	 * The folder to store log files
	 */
	private File logDir;
	
	/**
	 * Constructor 
	 */
	public QueueFileHandlerTest() throws Exception {
		super("ArchiveQueueFileHandlerTest");
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
	 * This test checks the correctness of the format of the name of the file against a regular expresison.
	 * 
	 * @throws Exception
	 */
	public void testGetNewFile() throws Exception {
		final String filenamePattern = "log\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}_YYYY-MM-DDTHH:MM:SS\\.mmm\\.xml";
		QueueFileHandler handler = new QueueFileHandler(this.getContainerServices(), logDir.getAbsolutePath(), 1, 1025L,"log","Logging");
		File f = handler.getNewFile();
		assertNotNull(f);
		assertTrue(f.getName()+" has a wrong format!", f.getName().matches(filenamePattern));
		LOG.info(f.getAbsolutePath());
	}
	
	/**
	 * This test checks if the file has been renamed after calling {@link QueueFileHandler#fileProcessed(File, String, String)}.
	 * <P>
	 * The name of the file, after renaming, must match with the creation and closing time of the file
	 * and not depend on the content of the timestamped strings it contains.
	 * <BR>
	 * This test creates a file th invokes {@link QueueFileHandler#fileProcessed(File, String, String)} passing
	 * two timestamps and checks that they are ignored in favour of the actual timestamp 
	 * 
	 * @throws Exception
	 */
	public void testCloseFile() throws Exception {
		// The format of the file after renaming
		final String filenamePattern = "log\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}_\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}\\.xml";
		// Create a file whose name contains the actual time
		String now = IsoDateFormat.formatCurrentDate();
		File oldLogFile = new File(logDir, "log"+now+"_YYYY-MM-DDTHH:MM:SS.mmm.xml");
		// touch the log file
		try (FileOutputStream ofs = new FileOutputStream(oldLogFile)) {;;}
		QueueFileHandler handler = new QueueFileHandler(this.getContainerServices(), logDir.getAbsolutePath(), 1, 1025L,"log","Logging");
		// The earliest timestamp must be before the actual timestamp to ensure it is ignored
		String earliestLogTimestamp = "2014-12-06T13:00:00.000";
		// The oldest timestamp must be after the actual timestamp to ensure it is ignored
		int nextYear = Calendar.getInstance().get(Calendar.YEAR)+1;
		String oldestLogTimestamp = ""+nextYear+"-12-06T15:12:34.567";
		// Wait 10 seconds before closing the file tobe sure the creatin and closing timestamps in the
		// name of the file differ
		try {
			Thread.sleep(10000);
		} catch (InterruptedException ie) {}
		handler.fileProcessed(oldLogFile, earliestLogTimestamp, oldestLogTimestamp);
		try {
			Thread.sleep(10);
		} catch (InterruptedException ie) {}
		String timeAfterProcessing = IsoDateFormat.formatCurrentDate();
		assertFalse(oldLogFile.exists());
		File newLogFile = new File(logDir, String.format("log%s_%s.xml", earliestLogTimestamp, oldestLogTimestamp));
		assertEquals("Too many log files in "+logDir.getAbsolutePath(),1,logDir.list().length);
		String fileName = logDir.list()[0];
		// Check the format
		assertTrue(fileName+" renamed with wrong format!", fileName.matches(filenamePattern));
		// Check if the name contains the oldest or the newest timepstamp
		assertEquals("The file name should not contain the oldest timestamp", -1,fileName.indexOf(oldestLogTimestamp));
		assertEquals("The file name should not contain the newest timestamp", -1,fileName.indexOf(earliestLogTimestamp));
		// The creationtimestamp (now) should be in the name of the file (i.e. not changed)
		assertTrue("The start timestamp has been changed!",fileName.startsWith("log"+now+"_") );
		// Check that the oldest timestamp in the name of the file is before
		// the fileProcessed had been executed
		int posUnderscore = fileName.lastIndexOf('_');
		int posFileNameExtension = fileName.lastIndexOf(".xml");
		String lastIsoTimestamp=fileName.substring(posUnderscore+1,posFileNameExtension);
		Date afterClosingDate = IsoDateFormat.parseIsoTimestamp(timeAfterProcessing);
		Date closingTimestamp=IsoDateFormat.parseIsoTimestamp(lastIsoTimestamp);
		assertTrue("The closing timestamp in the file is after fileProcessed had been called",closingTimestamp.before(afterClosingDate));
	}
	
	/**
	 * 
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		logDir = new File(System.getProperty("ACS.tmp",".")+"/logOfTest");
		logDir.mkdir();
	}

	/**
	 * 
	 */
	@Override
	protected void tearDown() throws Exception {
		for (File nextFile: logDir.listFiles()) {
			nextFile.delete();
		}
		logDir.delete();
		super.tearDown();
	}
	
}
