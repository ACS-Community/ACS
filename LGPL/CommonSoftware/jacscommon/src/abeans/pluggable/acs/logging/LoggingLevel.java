/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs.logging;

import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeSet;
import java.util.logging.Level;

/**
 * Defines ACS specific logging levels.
 * Also provides non-ACS level, which includes
 * set of standard logging and possibly other vendor levels,
 * to ACS level mapping. 
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class LoggingLevel extends Level implements ACSCoreLevel, Comparable
{
    /**
     * The resource bundle name to be used in localizing ACS level name.
     */
    private final static String ACS_BUNDLE_NAME = LoggingLevel.class.getPackage().getName();

    /**
     * List of all ACS levels.
     */
    private static TreeSet known = new TreeSet();

    /**
     * Fast lookup table mapping.
     */
    private static HashMap lookup = new HashMap();

	/******************** Java API ACS Levels ********************/
	
	/**
	 * Messages indicating function-calling sequence.
	 */
	public static final LoggingLevel TRACE = new LoggingLevel("TRACE", 400, ACS_LEVEL_TRACE);
	
	/**
	 * Messages that contain information normally of use only when
	 * debugging a program.
	 */
	public static final LoggingLevel DEBUG = new LoggingLevel("DEBUG", 700, ACS_LEVEL_DEBUG);
	
	/**
	 * Informational messages.
	 */
	public static final LoggingLevel INFO = new LoggingLevel("INFO", 800, ACS_LEVEL_INFO);
	
	/**
	 * Conditions that are not error conditions, but that may require
	 * special handling.
	 */
	public static final LoggingLevel NOTICE = new LoggingLevel("NOTICE", 801, ACS_LEVEL_NOTICE);
	
	/**
	 * Warning messages.
	 */
	public static final LoggingLevel WARNING = new LoggingLevel("WARNING", 900, ACS_LEVEL_WARNING);
	
	/**
	 * Error messages.
	 */
	public static final LoggingLevel ERROR = new LoggingLevel("ERROR", 901, ACS_LEVEL_ERROR);
	
	/**
	 * Critical conditions, such as hard device errors.
	 */
	public static final LoggingLevel CRITICAL = new LoggingLevel("CRITICAL", 902, ACS_LEVEL_CRITICAL);
	
	/**
	 * A condition that should be corrected immediately, such as a
	 * corrupted system database.
	 */
	public static final LoggingLevel ALERT = new LoggingLevel("ALERT", 903, ACS_LEVEL_ALERT);
	
	/**
	 * A panic condition. This is normally broadcast to all users.
	 */
	public static final LoggingLevel EMERGENCY = new LoggingLevel("EMERGENCY", 1000, ACS_LEVEL_EMERGENCY);




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
	public LoggingLevel(String name, int value, int acsLevel)
	{
		super(name, value, ACS_BUNDLE_NAME);

		assert (name.length() > 1);
		
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

	/**
	 * Returns the ACS core level.
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
		Level l = (Level)obj;
		if (intValue() < l.intValue())
			return -1;
		else if (intValue() == l.intValue())
			return 0;
		else
			return 1;	
	}
	
	/**
	 * Maps level to native level.
	 * @param level	any level
	 * @return 		native level, can be <code>null</code> if no native levels found or Level.OFF.intValue()
	 */
	public static LoggingLevel getNativeLevel(Level level)
	{
		// save for fast lookups
		Object luLevel = null;
		synchronized (lookup)
		{
			luLevel = lookup.get(level);
		}
		if (luLevel!=null)
			return (LoggingLevel)luLevel;
			
		// this is not needed anymore since we have goot fast lookups ;)
		// if this already native level
		//if (ACS_BUNDLE_NAME.equals(level.getResourceBundleName()))
		//	return (LoggingLevel)level;
		
		// check if there is any native level
		// of OFF
		if (known.size()==0 || level.intValue()==Level.OFF.intValue())
			return null;
			
		// search through iterator and find the most appropriate
		synchronized (known)
		{
			Iterator iter = known.iterator();
			LoggingLevel nativeLevel = (LoggingLevel)iter.next();
			while (level.intValue() > nativeLevel.intValue() && iter.hasNext())
				nativeLevel = (LoggingLevel)iter.next();
			
			// save for lookup
			synchronized (lookup)
			{
				lookup.put(level, nativeLevel);
			}

			return nativeLevel;
		}

	}

}
