/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs;

import java.util.logging.Level;

/**
 * Private level class implementation.
 * 
 * This level is used by <code>abeans.pluggable.acs.LoggingExceptionHandlerService</code> for
 * all the exception messages (reports) ment to go to console.
 * Needed since <code>java.util.logging.Level</code> constructor is <code>protected</code>.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ExceptionReportLevel extends Level
{

	/**
	 * Level for exception messages (reports) ment to go to console.
	 */
	public static final Level EXCEPTION = new ExceptionReportLevel("EXCEPTION", Level.WARNING.intValue());
	
    /**
     * Create a named Level with a given integer value.
     * 
     * @param name  the name of the Level, for example "INFO".
     * @param value an integer value for the level.
     */
	protected ExceptionReportLevel(String name, int value)
	{
		super(name, value, ExceptionReportLevel.class.getClass().getName());
	}

}
