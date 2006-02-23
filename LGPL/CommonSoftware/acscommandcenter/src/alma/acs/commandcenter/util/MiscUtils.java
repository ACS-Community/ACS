/*
 * Created on Oct 26, 2005 by mschilli
 */
package alma.acs.commandcenter.util;

import java.util.logging.Logger;




public class MiscUtils {

	
	/**
	 * Returns a logger for the specified class, based on
	 * the requester's package name, e.g. "alma.acs.commandcenter.engine",
	 * thus the logger will be shared by all classes in that package.
	 *  
	 * @param requester object which needs a package-wide logger
	 * @return a logger
	 */
	public static Logger getPackageLogger(Object requester) {
		return getPackageLogger(requester.getClass());
	}

	/**
	 * Returns a logger for the specified class, based on
	 * the requester's package name, e.g. "alma.acs.commandcenter.engine",
	 * thus the logger will be shared by all classes in that package.
	 *  
	 * @param requester class which needs a package-wide logger
	 * @return a logger
	 */
	public static Logger getPackageLogger(Class requester) {
		String pkg = requester.getPackage().getName();
		Logger ret = Logger.getLogger(pkg);
		return ret;
	}
	
	/**
	 * Returns the specified string parsed as as int, or 0 if parsing fails.
	 * 
	 * @return the specified string parsed as as int, or 0 if parsing fails.
	 */
	public static int parseInt (String text) {
		try {
			return Integer.parseInt(text);
		} catch (NumberFormatException exc) {
			return 0;
		}
	}


}


