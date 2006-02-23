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
package alma.acs.time;

import alma.acstime.Epoch;

import alma.acs.util.UTCUtility;

/**
 * Converts native Java time to ALMA time units.
 * 
 * @author dfugate Sep 30, 2004 
 */
public class TimeHelper extends UTCUtility
{
    /**
     * Converts from an ACS Epoch ("long-epoch UTC", 100-ns since Oct 15, 1582) 
     * to Java time ("short-epoch UTC", ms since Jan 01, 1970).
     * <p>
     * @return  time in milliseconds since Jan 01, 1970 00:00:00 
     */
    public static long epochToJava(Epoch acsTime) 
	{
	    return utcOmgToJava(acsTime.value);
	}
    
    /**
     * Converts from Java time ("short-epoch UTC", ms since Jan 01, 1970) 
     * to an ACS epoch ("long-epoch UTC", 100-ns since Oct 15, 1582).
     * <p>
     * @return  ACS Epoch (in 100-nanoseconds since Oct 15, 1582 00:00:00)
     */
    public static final Epoch javaToEpoch(long jTime)
	{
	    return new Epoch(utcJavaToOmg(jTime));
	}
    
    /**
     *  Returns the current timestamp in ACS Epoch ("long-epoch UTC", 100-ns 
     *  since Oct 15, 1582) format. To convert this value
     *  to a long, just use getTimeStamp().value
     * <p>
     * @return current ACS Epoch (in 100-nanoseconds since Oct 15, 1582 00:00:00)
     */
    public static Epoch getTimeStamp()
	{
	    return javaToEpoch(System.currentTimeMillis());
	}

    /**
     *  Returns the current UTC time in stringified format: "yyyy-MM-ddTHH:mm:ss.SSS"
     * <p>
     * @return present date/time in UTC format
     */
    public static String getUTCDate()
	{
	    return getUTCDate(getTimeStamp());
	}

    /**
     *  Converts the given ACS Epoch ("long-epoch UTC", 100-ns 
     *  since Oct 15, 1582) to a stringified format: "yyyy-MM-ddTHH:mm:ss.SSS"
     * <p>
     * @return present date/time in UTC format
     */
    public static String getUTCDate(Epoch acsTime)
	{
	    return getUTCDate(epochToJava(acsTime));
	}
}
