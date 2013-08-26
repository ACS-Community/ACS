package alma.acs.util;

import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import alma.acs.testsupport.TestLogger;

public class AcsFileFinderForDirectoriesTest
{
	private Logger logger;
	
	private File rootDir1;
	private File rootDir2;
	private File rootDir3;
	private File acsdataDir;

	
	@Before
	public void setUp() throws Exception {
		logger = TestLogger.getLogger("TestLogger");
		File sandboxDir = new File("AcsFileFinderForDirs_Sandbox").getAbsoluteFile();
		assertThat("Test must be started in the module's test directory.", sandboxDir.exists(), is(true));
		
		rootDir1 = new File(sandboxDir, "rootDir1");
		rootDir2 = new File(sandboxDir, "rootDir2");
		rootDir3 = new File(sandboxDir, "rootDir3");
		String testSearchPath = rootDir1.getAbsolutePath() + File.pathSeparator +
								rootDir2.getAbsolutePath() + File.pathSeparator +
								rootDir3.getAbsolutePath();
		System.setProperty(AcsFileFinderForDirectories.SEARCHPATH_PROPERTYNAME, testSearchPath);
		
		acsdataDir = new File(sandboxDir, "fakeAcsdata");
		String testAcsdataPath = acsdataDir.getAbsolutePath();
		System.setProperty(AcsFileFinderForDirectories.ACSDATA_PATH_PROPERTYNAME, testAcsdataPath);
	}

	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Tests the basic functionality of correctly finding a single file 
	 * in a set of root directories.
	 */
	@Test
	public void testSingleFileDirectoryOverlay() {
		
		// test without ACSDATA 
		
		AcsFileFinderForDirectories fileFinder = new AcsFileFinderForDirectories(logger, false);
		List<File> rootDirs = fileFinder.getRootDirs();
		List<File> expectedRootDirs = Arrays.asList(rootDir1, rootDir2, rootDir3);
		assertThat(rootDirs, equalTo(expectedRootDirs));
		
		String testFileName = "testFile1.txt"; // exists multiple times under the root and config dirs
		File testFile = fileFinder.findFile("", testFileName);
		assertThat("testFile1.txt should come from rootDir1", testFile, equalTo(new File(rootDir1, testFileName)));
		testFile = fileFinder.findFile("config", testFileName);
		assertThat("config/testFile1.txt should come from rootDir1", testFile, equalTo(new File(rootDir1, "config" + File.separator + testFileName)));
		
		testFileName = "testFile4.txt"; // exists only under rootDir2/config
		testFile = fileFinder.findFile("config", testFileName);
		assertThat("config/testFile4.txt should come from rootDir2", testFile, equalTo(new File(rootDir2, "config" + File.separator + testFileName)));
		
		testFileName = "testFile5.txt"; // exists only under fakeAcsdata, which should be invisible
		testFile = fileFinder.findFile("", testFileName);
		assertThat("testFile5.txt from fakeAcsdata should be invisible", testFile, nullValue());

		// repeat test with ACSDATA 
		
		fileFinder = new AcsFileFinderForDirectories(logger, true);
		testFileName = "testFile1.txt";
		testFile = fileFinder.findFile("", testFileName);
		assertThat("testFile1.txt should come from rootDir1", testFile, equalTo(new File(rootDir1, testFileName)));
		testFileName = "testFile5.txt"; // exists only under fakeAcsdata, which should now be visible
		testFile = fileFinder.findFile("", testFileName);
		assertThat("testFile5.txt from fakeAcsdata should be visible", testFile, equalTo(new File(acsdataDir, testFileName)));
	}

	/**
	 * Tests the basic functionality of correctly finding multiple files 
	 * in a set of root directories.
	 */
	@Test
	public void testMultipleFilesDirectoryOverlay() {
		
		AcsFileFinderForDirectories fileFinder = new AcsFileFinderForDirectories(logger, true);
		FilenameFilter filter = new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return ( name.endsWith(".txt"));
			}
		};
		
		List<File> fileList = fileFinder.findFiles("config", filter);
		assertThat(fileList, containsInAnyOrder(
					new File(rootDir1, "config" + File.separator + "testFile1.txt"),
					new File(rootDir1, "config" + File.separator + "testFile2.txt"),
					new File(rootDir1, "config" + File.separator + "testFile3.txt"),
					new File(rootDir2, "config" + File.separator + "testFile4.txt")
				));
	}

	@Test
	public void testInputParamHandling() {
		
		AcsFileFinderForDirectories fileFinder = new AcsFileFinderForDirectories(logger, true);
		
		assertThat("null relative path should be treated like empty relative path", 
				fileFinder.findFile(null, "testFile1.txt"), equalTo(fileFinder.findFile("", "testFile1.txt")));
		
		try {
			fileFinder.findFile("", null);
			fail("findFile with null file name should throw NPE");
		} catch (NullPointerException ex) {
			// expected
		}
		
		// TODO: Also for FilenameFilter
	}

}
