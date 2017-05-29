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
 *
 *    Created on Jan 18, 2008
 *
 */

package alma.acs.xmlfilestore.test.logging;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import junit.framework.TestCase;

/**
 * @author hmeuss
 * 
 */
public class ArchiveLoggerImplTest extends TestCase {
	private Logger m_logger = Logger.getLogger("ArchiveLoggerImplTest");
	private final String logMessage = 
			"<Emergency TimeStamp=\"2008-11-05T10:47:33.865\""
			+ " File=\"alma.acs.container.ComponentAdapter\""
			+ " Line=\"233\" Routine=\"_getInterfaces\""
			+ " Host=\"te22\" Process=\"frodoContainer\""
			+ " SourceObject=\"frodoContainer\" Thread=\"RequestProcessor-5\""
			+ " LogId=\"91\"><![CDATA[interfaces of component 'ARCHIVE_LOGGER':"
			+ " IDL:alma/xmlstore/Logger:1.0IDL:alma/ACS/ACSComponent:1.0]]></Emergency>";

	/**
	 * 
	 * @param testName
	 * @throws Exception
	 */
	public ArchiveLoggerImplTest(String testName) throws Exception {
		super(testName);
		m_logger.setUseParentHandlers(false);
		m_logger.setLevel(Level.FINER);
		m_logger.addHandler(new Handler() {

			@Override
			public void publish(LogRecord record) {
				String string = record.getLevel() + " [Thread-" + record.getThreadID() + "]";
				string += " " + record.getSourceClassName().substring(record.getSourceClassName().lastIndexOf('.') + 1) + "." + record.getSourceMethodName();
				string += ": " + record.getMessage();
				System.out.println(string);
			}

			@Override
			public void flush() {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void close() throws SecurityException {
				// TODO Auto-generated method stub
				
			}
			
		});
	}	
}
