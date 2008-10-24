/*
 * Created on Oct 26, 2005 by mschilli
 */
package alma.acs.commandcenter.util;

import java.util.StringTokenizer;
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
	public static Logger getPackageLogger(Class<?> requester) {
		String pkg = requester.getPackage().getName();
		Logger ret = Logger.getLogger(pkg);
		return ret;
	}
	
	/**
	 * Returns the specified string parsed as an int, or 0 if parsing fails.
	 * 
	 * @return the specified string parsed as an int, or 0 if parsing fails.
	 */
	public static int parseInt (String text) {
		try {
			return Integer.parseInt(text);
		} catch (NumberFormatException exc) {
			return 0;
		}
	}

	/**
	 * Returns the specified strings concatenated as one comma-separated string.
	 * 
	 * @return the specified strings concatenated as one comma-separated string.
	 */
	public static String join (String[] text) {
		if (text == null || text.length == 0)
			return "";
		StringBuilder ret = new StringBuilder(text[0]);
		for (int i=1; i<text.length; i++) {
			ret.append(",").append(text[i]);
		}
		return ret.toString();
	}

	/**
	 * Returns the specified string parsed into pieces.
	 * Commas and semi-colons are accepted as separators, whitespace is tolerated.
	 * Thus, the formats recognized are roughly:<ul>
	 * <li> "abc,def,ghi"
	 * <li> "abc, def, ghi"
	 * <li> "abc;def;ghi"
	 * <li> "abc; def; ghi"
	 * </ul>
	 * @return the specified string parsed into pieces.
	 */
	public static String[] split (String text) {
		StringTokenizer t = new StringTokenizer(text, ",;");
		String[] ret = new String[t.countTokens()];
		for (int i=0; i<ret.length; i++) {
			ret[i] = t.nextToken().trim();
		}
		return ret;
	}

}


