/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs.logging;

/**
 * Contains constants for ACS Core Levels.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface ACSCoreLevel
{

    /**
     * Unknown (ACS level).
     */
	public final int ACS_LEVEL_UNKNOWN = 0;
	
	/**
	 * Messages indicating function-calling sequence (ACS level).
	 */
	public final int ACS_LEVEL_TRACE = 2;
	
	/**
	 * Messages that contain information normally of use only when
	 * debugging a program (ACS level).
	 */
	public final int ACS_LEVEL_DEBUG = 3;
	
	/**
	 * Informational messages (ACS level).
	 */
	public final int ACS_LEVEL_INFO = 4;
	
	/**
	 * Conditions that are not error conditions, but that may require
	 * special handling (ACS level).
	 */
	public final int ACS_LEVEL_NOTICE = 5;
	
	/**
	 * Warning messages (ACS level).
	 */
	public final int ACS_LEVEL_WARNING = 6;
	
	/**
	 * Error messages (ACS level).
	 */
	public final int ACS_LEVEL_ERROR = 8;
	
	/**
	 * Critical conditions, such as hard device errors (ACS level).
	 */
	public final int ACS_LEVEL_CRITICAL = 9;
	
	/**
	 * A condition that should be corrected immediately, such as a
	 * corrupted system database (ACS level).
	 */
	public final int ACS_LEVEL_ALERT = 10;
	
	/**
	 * A panic condition. This is normally broadcast to all users (ACS level).
	 */
	public final int ACS_LEVEL_EMERGENCY = 11;


}
