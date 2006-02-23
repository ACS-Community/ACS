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

import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;

import alma.acs.logging.formatters.AcsXMLLogFormatter;

/**
 * @author rgeorgie
 *
 *Tests the AcsLogLevel
 */
public class AcsLoggingHandlerTest extends junit.framework.TestCase
{
//	AcsLoggingHandler m_acsLoggingHandler = new AcsLoggingHandler();
//	AcsXMLLogFormatter m_acsXMLFormatter = null;
//
//	static String localcopy;
//	/**
//	 * Manager that controls the default java loggers and services.
//	 */
//	private LogManager m_logManager;
//
//	/**
//	 * String corresponding to class that is used in the properties file
//	 * to define level, cacheSize, formatter, local copy.
//	 */
//	private String m_handlerClassName;
//
//	private boolean m_verbose = false;
//
//	public AcsLoggingHandlerTest(String name)
//	{
//		super(name);
//		m_acsXMLFormatter = new AcsXMLLogFormatter();
//		m_logManager = LogManager.getLogManager();
//
//		m_handlerClassName = AcsLoggingHandler.class.getName();
//
//		localcopy = m_logManager.getProperty(m_handlerClassName + ".localcopy");
//	}
//
//	/**
//	 * Method testFormatRecordLog. Checks whether a log record is formatted 
//	 * according to what is expected using the Java Formatter.
//	 */
//	public void testFormatSimpleLogRecord()
//	{
//		//formatSimpleLogRecord
//		LogRecord logRecord = new LogRecord(AcsLogLevel.INFO, "INFO testFormatSimpleLogRecord message");
//		assertTrue(m_acsLoggingHandler.formatSimpleLogRecord(logRecord) != null);
//	}
//	
//	/** 
//	 * According to the specifications, the level set in the properties file should not be less than INFO
//	 * at the different stages of the project.
//	 */
//	public void testPublish()
//	{
//		Level curLevel = m_acsLoggingHandler.getLevelProp();
//		int currentLevel = AcsLogLevel.getNativeLevel(curLevel).getAcsLevel();
//		LogRecord logRecord = new LogRecord(AcsLogLevel.INFO, "INFO testPublish message");
//		int testLevel = AcsLogLevel.getNativeLevel(logRecord.getLevel()).getAcsLevel();
//		assertTrue(currentLevel <= testLevel);
//	}
}