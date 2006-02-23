/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs.logging;

import java.util.logging.Handler;
import java.util.logging.LogRecord;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.Root;
import abeans.core.UnableToInstallComponentException;
import abeans.core.defaults.ExceptionIgnorer;
import abeans.pluggable.acs.ExceptionReportLevel;


/**
 * ACS XML Logging handler.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class LoggingHandler extends Handler implements Identifiable
{

	/**
	 * Identifier.
	 */
	Identifier id;

	/**
	 * Remote Logging Service.
	 */
	RemoteLoggingService remote = null;

	/**
	 * Constructor for LoggingFormatter.
	 */
	public LoggingHandler() 
	{
		super();
		
		//
		// RemoteLoggingService
		//

		//UnableToInstallComponentException exc = null;

		// try to install RemoteLoggingService, if none installed yet
		remote = (RemoteLoggingService)Root.getComponentManager().getComponent(RemoteLoggingService.class);
		if (remote == null) 
		{
			try
			{
				Root.getComponentManager().installComponent(RemoteLoggingService.class);
				remote = (RemoteLoggingService)Root.getComponentManager().getComponent(RemoteLoggingService.class);
			}
			catch (UnableToInstallComponentException uice)
			{
				new ExceptionIgnorer(uice);
				//exc = uice;
			}
		}
		
		// no logging here 
		//if (remote == null) 
		//	new MessageLogEntry(this, "Remote logging disabled due to ACS Remote Logging Service initialization failure.", exc, LoggingLevel.ERROR).dispatch();

	}

	/**
	 * @see java.util.logging.Handler#publish(LogRecord)
	 */
	public void publish(LogRecord log)
	{
		// filter out logs below logging priority
		// and deny all informational exception logs
		// they are logged separately
		if (remote == null ||
			!isLoggable(log) ||
			log.getLevel() == ExceptionReportLevel.EXCEPTION)
			return;
			
		String xml = getFormatter().format(log);
		if (xml != null)
		{
			//System.out.println(xml);
			remote.log(LoggingLevel.getNativeLevel(log.getLevel()).getAcsLevel(), xml);
		}
	}

	/**
	 * @see java.util.logging.Handler#flush()
	 */
	public void flush()
	{
	}

	/**
	 * @see java.util.logging.Handler#close()
	 */
	public void close() throws SecurityException
	{
	}

	/**
	 * @see java.util.logging.Handler#getFormatter()
	 */
	/*
	public Formatter getFormatter()
	{
		Formatter f = super.getFormatter();
		
		if (f==null)
		{
			f = new LoggingFormatter();
			setFormatter(f);
		}
			
		return f;
	}
	*/

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		return null;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return false;
	}

}
