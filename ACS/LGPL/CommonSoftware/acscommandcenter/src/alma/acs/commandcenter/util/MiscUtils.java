/*
 * Created on Oct 26, 2005 by mschilli
 */
package alma.acs.commandcenter.util;

import java.util.StringTokenizer;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.ACSPorts;
import alma.acs.util.AcsLocations;




public class MiscUtils {

	
	/**
	 * Returns a logger for the specified class, based on
	 * the requester's package name, e.g. "alma.acs.commandcenter.engine",
	 * thus the logger will be shared by all classes in that package.
	 *  
	 * @param requester object which needs a package-wide logger
	 * @return a logger
	 */
	public static AcsLogger getPackageLogger(Object requester) {
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
	public static AcsLogger getPackageLogger(Class<?> requester) {
		String pkg = requester.getPackage().getName();
		AcsLogger ret = ClientLogManager.getAcsLogManager().getLoggerForApplication(pkg, false);
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
	 * This parses the quick notation ("host:instance") of a manager location,
	 * and converts it to a corbaloc. Returns a corbaloc, or null if not parsable.
	 * 
	 * @return a corbaloc from a quick notation, or null.
	 */
   public static String convertShortNotationToCorbaloc (String manager) {
		try {
			String[] ss = manager.split(":");
			if (ss.length != 2) return null;      // only one colon in string
			if (ss[1].length() != 1) return null; // only one digit after colon
			String mgrPort = ACSPorts.globalInstance(Integer.parseInt(ss[1])).giveManagerPort();
			return AcsLocations.convertToManagerLocation(ss[0], mgrPort);
		} catch (Exception exc) {
			return null;
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


