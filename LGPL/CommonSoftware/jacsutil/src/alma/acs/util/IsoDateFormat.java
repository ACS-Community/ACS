package alma.acs.util;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * ISO 8601 date format.
 * <p>
 * http://jira.alma.cl/browse/COMP-1929 made it clear that we use the same format string all over ACS,
 * and that there should be this class to encapsulate it.
 * @author hsommer
 */
public class IsoDateFormat extends SimpleDateFormat
{
	private static final IsoDateFormat instance = new IsoDateFormat();
	
	public static final String pattern = "yyyy-MM-dd'T'HH:mm:ss.SSS";

	public IsoDateFormat() {
		super(pattern);
	}

	/**
	 * Convenience method that works with a shared instance of this class.
	 * @see DateFormat#format(Date)
	 */
	public static String formatDate(Date date) {
		synchronized (instance) {  // see sync comment for java.text.DataFormat
			return instance.format(date);			
		}
	}
	
	/**
	 * Convenience method that works with a shared instance of this class.
	 */
	public static String formatCurrentDate() {
		return formatDate(new Date());
	}
}
