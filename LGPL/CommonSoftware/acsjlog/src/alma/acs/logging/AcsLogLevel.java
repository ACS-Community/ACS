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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
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
	 * List of all ACS levels.
	 */
	private static Set<AcsLogLevel> known = new TreeSet<AcsLogLevel>();

	/**
	 * Fast lookup table mapping.
	 */
	private static Map<Level, AcsLogLevel> lookup = new HashMap<Level, AcsLogLevel>();

	/******************** Java API ACS Levels ********************/

	/**
	 * Messages indicating function-calling sequence.
	 */
	public static final AcsLogLevel TRACE = new AcsLogLevel("TRACE", 300, ACS_LEVEL_TRACE);

	/**
	 * Messages that contain information normally of use only when
	 * debugging a program.
	 */
	public static final AcsLogLevel DEBUG = new AcsLogLevel("DEBUG", 700, ACS_LEVEL_DEBUG);

	/**
	 * Informational messages.
	 */
	public static final AcsLogLevel INFO = new AcsLogLevel("INFO", 800, ACS_LEVEL_INFO);

	/**
	 * Conditions that are not error conditions, but that may require
	 * special handling.
	 */
	public static final AcsLogLevel NOTICE = new AcsLogLevel("NOTICE", 801, ACS_LEVEL_NOTICE);

	/**
	 * Warning messages.
	 */
	public static final AcsLogLevel WARNING = new AcsLogLevel("WARNING", 900, ACS_LEVEL_WARNING);

	/**
	 * Error messages.
	 */
	public static final AcsLogLevel ERROR = new AcsLogLevel("ERROR", 901, ACS_LEVEL_ERROR);

	/**
	 * Critical conditions, such as hard device errors.
	 */
	public static final AcsLogLevel CRITICAL = new AcsLogLevel("CRITICAL", 902, ACS_LEVEL_CRITICAL);

	/**
	 * A condition that should be corrected immediately, such as a
	 * corrupted system database.
	 */
	public static final AcsLogLevel ALERT = new AcsLogLevel("ALERT", 903, ACS_LEVEL_ALERT);

	/**
	 * A panic condition. This is normally broadcast to all users.
	 */
	public static final AcsLogLevel EMERGENCY = new AcsLogLevel("EMERGENCY", 1000, ACS_LEVEL_EMERGENCY);

	/**
	 * The resource bundle name to be used in localizing ACS level name.
	 */
	private int acsLevel = ACS_LEVEL_UNKNOWN;

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
	public AcsLogLevel(String name, int value, int acsLevel)
	{
		super(name, value, ACS_BUNDLE_NAME);

		// create entry name, so that is computent only once
		entryName = name.substring(0, 1).toUpperCase() + name.substring(1).toLowerCase();

		this.acsLevel = acsLevel;		

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

	public static AcsLogLevel fromAcsCoreLevel(int acsCoreLevel) {
		AcsLogLevel ret = null;
		for (Iterator<AcsLogLevel> iter = known.iterator(); iter.hasNext();) {
			AcsLogLevel acsLevel = iter.next();
			if (acsLevel.getAcsLevel() == acsCoreLevel) {
				ret = acsLevel;
				break;
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
		return acsLevel;
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

		// this is not needed anymore since we have goot fast lookups ;)
		// if this already native level
		//if (ACS_BUNDLE_NAME.equals(level.getResourceBundleName()))
		//	return (AcsLogLevel)level;

		// check if there is any native level
		// of OFF
		if (known.size() == 0 || level.intValue() == Level.OFF.intValue())
			return null;
		if (level.intValue() == Level.ALL.intValue())
			return AcsLogLevel.TRACE;

		// search through iterator and find the most appropriate
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
}