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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;
import java.util.logging.Level;

import alma.acs.logging.level.AcsLogLevelDefinition;

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
public class AcsLogLevel extends Level implements Comparable<AcsLogLevel>
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

// Currently we try to do without a level definition for ALL, see module loggingidl	
//	/**
//	 * Messages indicating function-calling sequence.
//	 */
//	public static final AcsLogLevel ALL = new AcsLogLevel("ALL", Level.ALL.intValue(), ACS_LEVEL_ALL);

	/**
	 * Messages indicating function-calling sequence.
	 */
	public static final AcsLogLevel TRACE = new AcsLogLevel("TRACE", Level.FINEST.intValue(), AcsLogLevelDefinition.TRACE);

	/**
	 * A lower level than DEBUG, just to allow finer tuning. Generally same meaning. See COMP-3749.
	 */
	public static final AcsLogLevel DELOUSE = new AcsLogLevel("DELOUSE", Level.FINER.intValue(), AcsLogLevelDefinition.DELOUSE);

	/**
	 * Messages that contain information normally of use only when
	 * debugging a program.
	 * Java levels FINE and CONFIG map to this DEBUG level.
	 */
	public static final AcsLogLevel DEBUG = new AcsLogLevel("DEBUG", Level.CONFIG.intValue(), AcsLogLevelDefinition.DEBUG);

	/**
	 * Informational messages.
	 */
	public static final AcsLogLevel INFO = new AcsLogLevel("INFO", Level.INFO.intValue(), AcsLogLevelDefinition.INFO);

	/**
	 * Conditions that are not error conditions, but that may require
	 * special handling.
	 * TODO: use something like 850 instead of 801 to allow other levels between INFO and NOTICE 
	 */
	public static final AcsLogLevel NOTICE = new AcsLogLevel("NOTICE", 801, AcsLogLevelDefinition.NOTICE);

	/**
	 * Warning messages.
	 */
	public static final AcsLogLevel WARNING = new AcsLogLevel("WARNING", Level.WARNING.intValue(), AcsLogLevelDefinition.WARNING);

	/**
	 * Error messages.
	 * TODO: use something like 930 instead of 901 to allow other levels between ERROR and WARNING
	 */
	public static final AcsLogLevel ERROR = new AcsLogLevel("ERROR", 901, AcsLogLevelDefinition.ERROR);

	/**
	 * Critical conditions, such as hard device errors.
	 * TODO: use something like 960 instead of 902 to allow other levels between CRITICAL and ERROR
	 */
	public static final AcsLogLevel CRITICAL = new AcsLogLevel("CRITICAL", 902, AcsLogLevelDefinition.CRITICAL);

	/**
	 * A condition that should be corrected immediately, such as a
	 * corrupted system database.
	 * TODO: use something like 980 instead of 903 to allow other levels between ERROR and ALERT
	 */
	public static final AcsLogLevel ALERT = new AcsLogLevel("ALERT", 903, AcsLogLevelDefinition.ALERT);

	/**
	 * A panic condition. This is normally broadcast to all users.
	 */
	public static final AcsLogLevel EMERGENCY = new AcsLogLevel("EMERGENCY", Level.SEVERE.intValue(), AcsLogLevelDefinition.EMERGENCY);

	/**
	 * Level not to be used for actual logging, but to set log levels for filtering.
	 * Overwrites/hides {@link Level.OFF}.
	 */
	public static final AcsLogLevel OFF = new AcsLogLevel(Level.OFF.getName(), Level.OFF.intValue(), AcsLogLevelDefinition.OFF);
	
	
	/**
	 * The ACS error system defined level (small integer) which this JDK-style level maps to
	 */
	private final AcsLogLevelDefinition acsCoreLevel;

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
	public AcsLogLevel(String name, int value, AcsLogLevelDefinition acsCoreLevel)
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
		synchronized (lookup) {
			lookup.put(this, this);
		}
	}

	
	/**
	 * Converts an ACS core log level (small integer as defined in ACS IDL) 
	 * to the largest matching AcsLogLevel.
	 */
	public static AcsLogLevel fromAcsCoreLevel(AcsLogLevelDefinition acsCoreLevel) {
		AcsLogLevel ret = null;
		for (AcsLogLevel acsLogLevel : known) {
			ret = acsLogLevel;
			if (!(ret.getAcsLevel().compareTo(acsCoreLevel) < 0)) {
				break;
			}
		}
		return ret;
	}

	/**
	 * Converts an ACS core log level (small integer as defined in ACS IDL) 
	 * to the lowest matching AcsLogLevel or JDK Level.
	 * <p>
	 * Note the difference to {@link #fromAcsCoreLevel(AcsLogLevelDefinition)}: 
	 * <ul>
	 *   <li>If more than one JDK-style log level gets mapped to the given core level,
	 *       then the <em>lowest</em> of these levels gets returned.
	 *       <em>This method is therefore suitable to compute JDK log levels for filtering</em>
	 *       when logs with all matching JDK levels should be allowed.</li>
	 *   <li>Currently this method is implemented with hard-wired levels, which means that 
	 *       user-defined subclasses of AcsLogLevel are not considered. 
	 *       (As of ACS 8.x, there is no known usage of custom log levels.
	 *       Supporting them for this method would require to keep a list similar to {@link #known}
	 *       that includes all JDK Level and AcsLogLevel objects.)
	 * </ul>
	 */
	public static Level getLowestMatchingJdkLevel(AcsLogLevelDefinition acsCoreLevel) {
		switch (acsCoreLevel) {
		case TRACE:
			// numerically the same as Level.FINEST
			return AcsLogLevel.TRACE;
		case DELOUSE:
			// numerically the same as Level.FINER
			return AcsLogLevel.DELOUSE;
		case DEBUG:
			// Both JDK levels FINE and CONFIG map to AcsLogLevel.DEBUG.
			// AcsLogLevel.DEBUG is numerically the same as CONFIG, which is higher than FINE (reasons related to logic in getNativeLevel()).
			return Level.FINE;
		case INFO:
			// numerically the same as Level.INFO
			return AcsLogLevel.INFO;
		case NOTICE:
			return AcsLogLevel.NOTICE;
		case WARNING:
			// numerically the same as Level.WARNING
			return AcsLogLevel.WARNING;
		case ERROR:
			return AcsLogLevel.ERROR;
		case CRITICAL:
			return AcsLogLevel.CRITICAL;
		case ALERT:
			return AcsLogLevel.ALERT;
		case EMERGENCY:
			// numerically the same as Level.SEVERE
			return AcsLogLevel.EMERGENCY;
		case OFF:
			// numerically the same as Level.OFF
			return AcsLogLevel.OFF;
		default:
			throw new IllegalArgumentException("Unexpected enum literal AcsLogLevelDefinition." + acsCoreLevel.name);
		}
	}
	
	
	/**
	 * Returns the corresponding ACS core level, which is a small positive integer 
	 * defined as "priority" in the ACS logging and archiving architecture document
	 * and coded in IDL (see {@link alma.AcsLogging.alma.LogLevels.WARNING_NAME} and similar constants).
	 * <p>
	 * This level is different from the JDK-style level, which can be any integer. 
	 * @return		ACS core level
	 */
	public AcsLogLevelDefinition getAcsLevel()
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
	 * Compares the level with the specified level for order.
	 * 
	 * @see java.lang.Comparable#compareTo(Object)
	 */
	public int compareTo(AcsLogLevel l)
	{
		if (intValue() < l.intValue())
			return -1;
		else if (intValue() == l.intValue())
			return 0;
		else
			return 1;
	}

	/**
	 * Maps any (JDK or ACS) level to an ACS native level.
	 * <p>
	 * Note that for some strange historical reason, the ACS log level is called "native" here, 
	 * to confuse those who think that JDK levels would be native...
	 * 
	 * @param level	any level
	 * @return 		native level, can be <code>null</code> if no native level is found or if level==Level.OFF
	 */
	public static AcsLogLevel getNativeLevel(Level level)
	{
		// try fast lookup
		AcsLogLevel luLevel = null;
		synchronized (lookup) {
			luLevel = lookup.get(level);
		}
		if (luLevel != null) {
			return luLevel;
		}

		// check if there is any native level of OFF
		if (known.size() == 0 || level.intValue() == Level.OFF.intValue())
			return null;
//		if (level.intValue() == Level.ALL.intValue())
//			return AcsLogLevel.ALL;

		// search through iterator and find the most appropriate. Relies on "known" being a sorted set with low levels first.
		synchronized (known) {
			Iterator<AcsLogLevel> iter = known.iterator();
			AcsLogLevel acsLevel = iter.next();
			while (level.intValue() > acsLevel.intValue() && iter.hasNext()) {
				acsLevel = iter.next();
			}
			// save for lookup
			synchronized (lookup) {
				lookup.put(level, acsLevel);
			}

			return acsLevel;
		}
	}
	
	
	/**
	 * This method should only be used to generate documentation about the various level mappings in use.
	 * @param ps The PrintStream to print to, e.g. System.out
	 */
	static void printMappings(PrintStream ps) {
		final String delim = "\t";
		synchronized (known) {
			for (AcsLogLevel level : known) {
				String acsLevelName = level.getEntryName();
				int levelValue = level.intValue();
				int coreLevelValue = level.getAcsLevel().value;
				ps.println(acsLevelName + delim + levelValue + delim + coreLevelValue);
			}
		}
	}
}