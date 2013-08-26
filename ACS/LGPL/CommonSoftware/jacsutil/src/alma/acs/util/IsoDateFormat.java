/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.util;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * ISO 8601 date format.
 * <p>
 * http://jira.alma.cl/browse/COMP-1929 made it clear that we use the same format string all over ACS,
 * and that there should be this class to encapsulate it.
 * <p>
 * No trailing 'Z' is assumed or used to define a reference to UTC time, which means that all timestamps 
 * are interpreted as referencing local time. 
 * In Alma for example all computers are set to UTC so that local time is always UTC.
 * 
 * @author hsommer
 */
public class IsoDateFormat extends SimpleDateFormat
{
	private static final IsoDateFormat instance = new IsoDateFormat();
	
	public static final String pattern = "yyyy-MM-dd'T'HH:mm:ss.SSS";

	public IsoDateFormat() {
		super(pattern);
		// Uncomment this when testing on a machine that is not set to UTC.
		// setTimeZone(TimeZone.getTimeZone("UTC"));
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
	
	public static Date parseIsoTimestamp(String isoTimestamp) throws ParseException {
		synchronized (instance) {  // see sync comment for java.text.DataFormat
			return instance.parse(isoTimestamp);
		}
	}
}
