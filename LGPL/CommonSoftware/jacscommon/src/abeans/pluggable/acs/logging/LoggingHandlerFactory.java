/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs.logging;

import java.util.logging.Handler;

import abeans.core.defaults.HandlerFactory;
import abeans.core.defaults.LoggingHandlerPolicy;

/**
 * Implementation of <code>HandlerFactory</code>.
 * 
 * <code>HandlerFactory</code> is used by <code>AbeansLogger</code> 
 * implementators to install from user provided handlers, which are 
 * appropriate for used <code>LoggingHandlerPolicy</code>.
 * With this factory a policy is decoupled from particular handler 
 * implementation. Logger can construct a handler with lazy-initialization
 * pattern (handler is created when it is needed), what is useful when handler 
 * needs to perform costly initialization (eg. remote handler).
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class LoggingHandlerFactory implements HandlerFactory
{

	/**
	 * The only logging handler to be managed.
	 */
	LoggingHandler handler;

	/**
	 * Constructor for LoggingHandlerFactory.
	 */
	public LoggingHandlerFactory()
	{
		super();
	}

	/**
	 * @see abeans.core.defaults.HandlerFactory#getHandlerForPolicy(LoggingHandlerPolicy)
	 */
	public Handler getHandlerForPolicy(LoggingHandlerPolicy policy)
	{
		// create handler if not created yet
		if (handler == null)
		{
			handler = new LoggingHandler();
			handler.setFormatter(new LoggingFormatter());
		}
		
		// apply policy
		handler.setLevel(policy.getValue());		
		return handler;
	}

}
