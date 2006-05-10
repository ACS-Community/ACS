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
package alma.acs.logging;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;
import java.util.logging.Level;

/**
 * Defines ACS specific logging levels.
 * Also provides non-ACS level, which includes
 * set of standard logging and possibly other vendor levels,
 * to ACS level mapping. Maps Java-specific levels to ACS specific levels. <p>
 * Although an OFF level is not mentioned in the Acs documentation, it is included 
 * for the purpose of dealing with bad levels as well as of blocking logging. 
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class AcsLogLevel extends Level implements ACSCoreLevel, Comparable
{
	/**
	 * The resource bundle name to be used in localizing ACS level name.
	 */
	private final static String ACS_BUNDLE_NAME = AcsLogLevel.class.getPackage().getName();

	/**
	 * List of all ACS levels, sorted automatically with lowest log level first.
	 */
	private static TreeSet<AcsLogLevel> known = new TreeSet<AcsLogLevel>();

	/**
	 * Fast lookup table mapping.
	 */
	private static Map<Level, AcsLogLevel> lookup = new HashMap<Level, AcsLogLevel>();

	/******************** Java API ACS Levels ********************/
	
	/**
	 * Messages indicating function-calling sequence.
	 */
	public static final AcsLogLevel ALL = new AcsLogLevel("ALL", Level.ALL.intValue(), ACS_LEVEL_ALL);

	/**
	 * Messages indicating function-calling sequence.
	 */
	public static final AcsLogLevel TRACE = new AcsLogLevel("TRACE", Level.FINEST.intValue(), ACS_LEVEL_TRACE);

	/**
	 * Messages that contain information normally of use only when
	 * debugging a program.
	 */
	public static final AcsLogLevel DEBUG = new AcsLogLevel("DEBUG", Level.CONFIG.intValue(), ACS_LEVEL_DEBUG);

	/**
	 * Informational messages.
	 */
	public static final AcsLogLevel INFO = new AcsLogLevel("INFO", Level.INFO.intValue(), ACS_LEVEL_INFO);

	/**
	 * Conditions that are not error conditions, but that may require
	 * special handling.
	 * TODO: use something like 850 instead of 801 to allow other levels between INFO and NOTICE 
	 */
	public static final AcsLogLevel NOTICE = new AcsLogLevel("NOTICE", 801, ACS_LEVEL_NOTICE);

	/**
	 * Warning messages.
	 */
	public static final AcsLogLevel WARNING = new AcsLogLevel("WARNING", Level.WARNING.intValue(), ACS_LEVEL_WARNING);

	/**
	 * Error messages.
	 * TODO: use something like 930 instead of 901 to allow other levels between ERROR and WARNING
	 */
	public static final AcsLogLevel ERROR = new AcsLogLevel("ERROR", 901, ACS_LEVEL_ERROR);

	/**
	 * Critical conditions, such as hard device errors.
	 * TODO: use something like 960 instead of 902 to allow other levels between CRITICAL and ERROR
	 */
	public static final AcsLogLevel CRITICAL = new AcsLogLevel("CRITICAL", 902, ACS_LEVEL_CRITICAL);

	/**
	 * A condition that should be corrected immediately, such as a
	 * corrupted system database.
	 * TODO: use something like 980 instead of 903 to allow other levels between ERROR and ALERT
	 */
	public static final AcsLogLevel ALERT = new AcsLogLevel("ALERT", 903, ACS_LEVEL_ALERT);

	/**
	 * A panic condition. This is normally broadcast to all users.
	 */
	public static final AcsLogLevel EMERGENCY = new AcsLogLevel("EMERGENCY", Level.SEVERE.intValue(), ACS_LEVEL_EMERGENCY);

	
	
	/**
	 * The ACS error system defined level (small integer) which this JDK-style level maps to
	 */
	private final int acsCoreLevel;

	/**
	 * XML Entry name.
	 */
	private String entryName = null;
	
	/**
	 * Create a named Level with a given integer value.
	 * 
	 * @param name  the name of the Level, for example "INFO".
	 * @param value an integer value for the level.
	 */
	public AcsLogLevel(String name, int value, int acsCoreLevel)
	{
		super(name, value, ACS_BUNDLE_NAME);

		// create entry name, so that is computent only once
		entryName = name.substring(0, 1).toUpperCase() + name.substring(1).toLowerCase();

		this.acsCoreLevel = acsCoreLevel;		

		// add to tree of known lists
		synchronized (known)
		{
			known.add(this);
		}

		// save for fast lookups
		synchronized (lookup)
		{
			lookup.put(this, this);
		}
	}

	/**
	 * Converts an ACS core log level (as defined in ACS, Unix or similar) 
	 * to the best fitting AcsLogLevel. 
	 * If no AcsLogLevel directly corresponds to the given core level, then the AcsLogLevel
	 * whose associated acsCoreLevel is >= the given core level is chosen.
	 * Only if <code>acsCoreLevel</code> is larger than EMERGENCY's core level,
	 * we "round down" to EMERGENCY.
	 * @param acsCoreLevel
	 * @return
	 */
	public static AcsLogLevel fromAcsCoreLevel(int acsCoreLevel) {
		AcsLogLevel ret = null;
		if (acsCoreLevel >= EMERGENCY.getAcsLevel()) {
			ret = EMERGENCY;
		}
		else {
			for (Iterator<AcsLogLevel> iter = known.iterator(); iter.hasNext();) {
				ret = iter.next();
				if (ret.getAcsLevel() >= acsCoreLevel) {
					break;
				}
			}
		}		
		return ret;
	}
	
	
	/**
	 * Returns the corresponding ACS core level (small positive integer defined by ACS).
	 * This is different from the JDK-style level, which can be any integer. 
	 * @return		ACS core level
	 */
	public int getAcsLevel()
	{
		return acsCoreLevel;
	}

	/**
	 * Returns the ACS XML level entryName.
	 * @return		ACS XML level entryName
	 */
	public String getEntryName()
	{
		return entryName;
	}

	/**
	 * Compares thoe level with the specified level for order.
	 * 
	 * @see java.lang.Comparable#compareTo(Object)
	 */
	public int compareTo(Object obj)
	{
		Level l = (Level) obj;
		if (intValue() < l.intValue())
			return -1;
		else if (intValue() == l.intValue())
			return 0;
		else
			return 1;
	}

	/**
	 * Maps any (JDK or ACS) level to an ACS native level.
	 * @param level	any level
	 * @return 		native level, can be <code>null</code> if no native levels found or Level.OFF.intValue()
	 */
	public static AcsLogLevel getNativeLevel(Level level)
	{
		// save for fast lookups
		Object luLevel = null;
		synchronized (lookup)
		{
			luLevel = lookup.get(level);
		}
		if (luLevel != null)
			return (AcsLogLevel) luLevel;

		// check if there is any native level
		// of OFF
		if (known.size() == 0 || level.intValue() == Level.OFF.intValue())
			return null;
		if (level.intValue() == Level.ALL.intValue())
			return AcsLogLevel.ALL;

		// search through iterator and find the most appropriate. Relies on "known" being a sorted set with low levels first.
		synchronized (known)
		{
			Iterator<AcsLogLevel> iter = known.iterator();
			AcsLogLevel nativeLevel = iter.next();
			while (level.intValue() > nativeLevel.intValue() && iter.hasNext())
				nativeLevel = iter.next();

			// save for lookup
			synchronized (lookup)
			{
				lookup.put(level, nativeLevel);
			}

			return nativeLevel;
		}
	}
	
	
	/**
	 * This method should only be used to generate documentation about the various level mappings in use.
	 * @param ps The PrintStream to print to, e.g. System.out
	 */
	static void printMappings(PrintStream ps) {
		final String delim = "\t";
		for (Iterator<AcsLogLevel> iter = known.iterator(); iter.hasNext();) {
			AcsLogLevel level = iter.next();
			String acsLevelName = level.getEntryName();
			int levelValue = level.intValue();
			int coreLevelValue = level.getAcsLevel();
			ps.println(acsLevelName + delim + levelValue + delim + coreLevelValue);
		}
	}
}