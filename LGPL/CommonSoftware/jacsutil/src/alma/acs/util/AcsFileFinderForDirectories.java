package alma.acs.util;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import alma.acs.makesupport.AcsFileFinder;

/**
 * Finds a specific file in the directory structure 
 * made up of logically overlaid directories 
 * with roots at $INTROOT, $INTLIST, $ACSROOT,
 * as specified in the system property <code>acs.system.path</code>.
 * Optionally also looks under $ACSDATA.
 * <p>
 * The first occurrence of a file is taken, so that "less permanent" root directories
 * such as $INTROOT are preferred over more permanent dirs such as $ACSROOT.
 * <p>
 * Implementation note: We do not use {@link AcsFileFinder}
 * because it would cache all files under the root directories,
 * which is too much for our purpose.
 * 
 * @author hsommer
 * @since ACS 10.2, see http://jira.alma.cl/browse/COMP-5690
 */
public class AcsFileFinderForDirectories
{
	public static final String SEARCHPATH_PROPERTYNAME = "acs.system.path";
	public static final String ACSDATA_PATH_PROPERTYNAME = "ACS.data";

	private final Logger logger;
	
	private final List<File> rootDirs;
	
	/**
	 * Creates an AcsFileFinderForDirectories that can be used for multiple file searches.
	 * 
	 * @param logger 
	 * @param includeAcsdataDir If true then $ACSDATA is included in the list of root dirs,
	 *        as the last chance for finding a file.
	 */
	public AcsFileFinderForDirectories(Logger logger, boolean includeAcsdataDir) {
		this.logger = logger;
		String searchpath = System.getProperty(SEARCHPATH_PROPERTYNAME);
		List<String> rootDirNames = new ArrayList<String>(Arrays.asList(searchpath.split(File.pathSeparator + "+")));
		if (includeAcsdataDir) {
			String acsdataDirName = System.getProperty(ACSDATA_PATH_PROPERTYNAME);
			rootDirNames.add(acsdataDirName);
		}
		
		// check and store directories
		rootDirs = new ArrayList<File>();
		for (String rootDirName : rootDirNames) {
			if (!rootDirName.trim().isEmpty()) {
				File rootDir = new File(rootDirName);
				if (rootDir.exists() && rootDir.isDirectory() && rootDir.canRead()) {
					rootDirs.add(rootDir);
				}
				else {
					logger.fine("Dropping bad root dir '" + rootDirName + "'.");
				}
			}
		}
	}
	
	/**
	 * This method is meant only for unit testing. 
	 */
	List<File> getRootDirs() {
		return new ArrayList<File>(rootDirs);
	}
	
	
	
	/**
	 * Searches for a single file with a known name.
	 * 
	 * @param relativePath
	 *            The path underneath $INTROOT, $ACSROOT, for example "config" or "lib/oracle/hiddenJars" or "".
	 * @param fileName
	 *            The file name (without path), e.g. "myConfigFile.txt".
	 * @return The file found, or <code>null</code> if it wasn't found.
	 * @throws NullPointerException
	 *             If <code>fileName</code> is <code>null</code>
	 */
	public File findFile(String relativePath, String fileName) {
		if (relativePath == null) {
			relativePath = "";
		}
		for (File rootDir : rootDirs) {
			File dir = new File(rootDir, relativePath);
			File file = new File(dir, fileName);
			if (file.exists()) {
				return file;
			}
		}
		return null;
	}
	
	/**
	 * More flexible file search, that allows the user to check the filename 
	 * in the supplied <code>filter</code> callback, for example to return
	 * all "*.txt" files under a given relative directory "config/myBelovedTxtFiles".
	 * <p>
	 * For performance reasons (and to get simpler code) the method {@link #findFile(String, String)}
	 * should be used instead whenever the single file name is known.
	 * 
	 * @param relativePath The path underneath $INTROOT, $ACSROOT, for example "config" or "lib/oracle/hiddenJars".
	 * @param filter Must decide which files match. 
	 *               The <code>dir</code> callback argument will be consistent with the <code>relativePath</code>
	 *               and should not be considered in the FilenameFilter implementation that should focus
	 *               only on the <code>name</code> parameter.
	 * @return The list of all files that matched the directory and filter, possibly empty.
	 *         If a certain file appeared under more than one root directory 
	 *         (e.g. under both "$INTROOT/config/bla" and "$ACSROOT/config/bla")
	 *         then the first occurrence (from the less permanent location) is taken.
	 */
	public List<File> findFiles(String relativePath, FilenameFilter filter) {
		if (relativePath == null) {
			relativePath = "";
		}
		Map<String, File> foundFiles = new LinkedHashMap<String, File>();
		for (File rootDir : rootDirs) {
			File dir = new File(rootDir, relativePath);
			if (dir.exists()) {
				File[] matchedFiles = dir.listFiles(filter);
				for (File matchedFile : matchedFiles) {
					// Do not 'overwrite' a file with same relative path and name that was found under a previous rooDir
					String relativePathName = relativePath + File.separator + matchedFile.getName();
					if (!foundFiles.containsKey(relativePathName)) {
						foundFiles.put(relativePathName, matchedFile);
					}
				}
			}
		}
		return new ArrayList<File>(foundFiles.values());
	}
}
