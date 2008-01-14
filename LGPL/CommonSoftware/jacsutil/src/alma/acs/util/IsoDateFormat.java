package alma.acs.util;

import java.text.SimpleDateFormat;

/**
 * ISO 8601 date format.
 * <p>
 * http://jira.alma.cl/browse/COMP-1929 made it clear that we use the same format string all over ACS,
 * and that there should be this class to encapsulate it.
 * @author hsommer
 */
public class IsoDateFormat extends SimpleDateFormat
{
	public static final String pattern = "yyyy-MM-dd'T'HH:mm:ss.SSS";

	public IsoDateFormat() {
		super(pattern);
	}

}
