/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.logging;

import java.util.logging.Level;

import abeans.core.AssertionFailed;
import abeans.core.defaults.LoggingHandlerPolicy;

/**
 * This policy can be installed into loggers supporting subclasses of 
 * <code>LoggingLevelPolicy</code>. 
 * 
 * The handler installed as a result of this policy will log all message
 * log entries with a level equal to or greater than the level carried
 * as a value of this policy. If, during installation of the policy,
 * the console handler is already installed, the <code>QoSProvider</code>
 * installing the policy should recheck the logging levels.
 * 
 * @author		Gasper Tkacik (gasper.tkacik@cosylab.com)
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @see		MessageLogEntry
 * @see		MessageLog
 * @see		java.util.logging.Level
 */
public class LoggingLevelPolicy extends LoggingHandlerPolicy
{

	/**
	 * The name of this policy. By policy design contract this is the name
	 * of the class for this policy without the leading package prefix.
	 */
	public static final String NAME = "LoggingLevelPolicy";
	
	/**
	 * Constructs a new policy, given the log level for the console 
	 * handler, using the default point range and best-effort semantics.
	 * 
	 * @param value	console handler will log entries with levels higher or equal
	 * 					to the specified level
	 */
	public LoggingLevelPolicy(Level value)
	{
		super(value);
	}

	/**
	 * Full constructor form for this policy, takes a level argument
	 * for the console handler as well as the conformance and range parameters.
	 * 
	 * @param	value			the minimum level of message logs to be processed by
	 * 							the console handler
	 * @param conformance		can be either best-effort or mandatory
	 * @param range			can be either point or hierarchical
	 * @throws AssertionFailed	not thrown
	 */
	public LoggingLevelPolicy(Level value, short conformance, short range)
		throws AssertionFailed
	{
		super(value, conformance, range);
	}

	/**
	 * Constructs a new policy from the string. The allowed values of the string
	 * are all values from which a <code>Level</code> instance can be produced using
	 * <code>Level.parse()</code> method. They are equal to the names of the static
	 * final public fields of the <code>Level</code> class without the <code>Level</code>
	 * prefix, e.g. "INFO", "ALL", etc.
	 * 
	 * @param value	the value from which the <code>Level</code> value of this policy	
	 * 					will be constructed
	 */
	public LoggingLevelPolicy(String value)
	{
		super(value);
	}

	/**
	 * Returns a short description of this policy.
	 * 
	 * @return description
	 * @see abeans.core.Policy#getDescription()
	 */
	public String getDescription()
	{
		return "If set in the logger supporting this policy, it will cause log messages with level equal or greater to the level specified by this policy to be sent to the ACS logging handler.";
	}

}
