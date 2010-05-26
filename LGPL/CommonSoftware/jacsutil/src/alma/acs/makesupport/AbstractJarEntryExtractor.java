package alma.acs.makesupport;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

public abstract class AbstractJarEntryExtractor {

	/**
	 * prefix paths inside JAR files which are not considered part of Java package paths
	 */
	public static final String[] PREFIX_PATHS = new String[] {"src", "test"};
	
	public final String FILETYPE;

	public AbstractJarEntryExtractor(String filetype) {
		super();
		FILETYPE = filetype;
	}

	/**
	 * Gets the Java class name from a JarEntry.
	 * 
	 * Uses {@link #PREFIX_PATHS} to remove a leading prefix path
	 * that is not part of the Java package. 
	 *
	 * @param javaEntry
	 * @return the class name
	 */
	protected String getClassName(JarEntry javaEntry) {
		String className = javaEntry.getName();
		for (int j = 0; j < PREFIX_PATHS.length; j++)
		{
			if (className.startsWith(PREFIX_PATHS[j]))
			{
				className = className.substring(PREFIX_PATHS[j].length());
				break;
			}
		}
		if (className.startsWith("/")) {
			className = className.substring(1);
		}
		return className;
	}

	/**
	 * Lists all Java files files of type FILETYPE that are contained inside a given JAR file.
	 * 
	 * The current implementation only looks for a FILETYPE file ending, 
	 * ignoring the contents of such files.
	 * 
	 * @param jarfile the JAR file to be searched
	 * @return  entries that are Java files of type FILETYPE (array != null, possibly empty)
	 */
	public JarEntry[] getJavaEntries(JarFile jarfile) {
		List<JarEntry> javaEntries = new ArrayList<JarEntry>();
		
		Enumeration<JarEntry> jarEntries = jarfile.entries();
		
		while (jarEntries.hasMoreElements())
		{
			JarEntry entry = (JarEntry) jarEntries.nextElement();
			if (entry.getName().endsWith(FILETYPE))
			{
				javaEntries.add(entry);
			}
		}		
		
		return (JarEntry[]) javaEntries.toArray(new JarEntry[javaEntries.size()]);
	}

}