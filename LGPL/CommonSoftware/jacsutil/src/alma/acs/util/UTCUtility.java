/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.util;

import java.util.Date;
import java.util.TimeZone;


/**
 * Converts UTC times of different epochs.
 * <p>
 * Note that Alma uses not only the Unix/Java time and OMG time 
 * that this class currently supports, but also "array time" 
 * which is a variant of VMS time: Nanoseconds since Nov 17, 1858.
 * If we ever need to convert this to Java time, the formula is 
 * (jTime + 40587 * 86400000) * 1000000L
 * with 40587 = number of days between 1858-11-17 and 1970-01-01
 * and 86400000 = milliseconds per day.
 * See also ICD/HLA/ASDM :: ArrayTime.
 * 
 * @author hsommer Jul 8, 2003 2:50:08 PM
 */
public class UTCUtility
{
	/**
	 * Converts a time duration (not time stamp!) given in milliseconds
	 * to 100ns by multiplying it with 10000.
	 * @param javaMillisDuration
	 * @return The duration in Corba-style 100-ns units.
	 */
	public static long durationJavaMillisToOmg(long javaMillisDuration) {
		return javaMillisDuration * 10000L;
	}
	
    /**
     * Converts from OMG time ("long-epoch UTC", 100-ns since Oct 15, 1582) 
     * to Java time ("short-epoch UTC", ms since Jan 01, 1970).
     * <p>
     * @return  time in milliseconds since Jan 01, 1970 00:00:00 
     */
    public static long utcOmgToJava(long omgTime) 
	{
	    return ( omgTime - 122192928000000000L ) / 10000L;
	}
    
    /**
     * Converts from Java time ("short-epoch UTC", ms since Jan 01, 1970) 
     * to OMG time ("long-epoch UTC", 100-ns since Oct 15, 1582).
     * <p>
     * @return  time in 100-nanoseconds since Oct 15, 1582 00:00:00 
     */
    public static final long utcJavaToOmg(long jTime)
	{
	    return ( jTime * 10000L + 122192928000000000L );
	}

    /**
     * Converts the given java time ("short-epoch UTC", ms since Jan 01, 1970) 
     * to a stringified format: "yyyy-MM-ddTHH:mm:ss.SSS"
     * <p>
     * @return given date/time in UTC format
     * @see IsoDateFormat#pattern
     */
    public static String getUTCDate(long jTime)
	{
	    TimeZone timezone = TimeZone.getDefault();
	    jTime = jTime - timezone.getOffset(jTime);
	    return IsoDateFormat.formatDate(new Date(jTime));
	}
}
