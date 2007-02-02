/*
*ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2007 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

/** 
 * @author  nbarriga
 * @version $Id: simpleLog.java,v 1.1 2007/02/02 08:52:46 nbarriga Exp $
 * @since    
 */

package alma.acs.loggingts;

import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.lang.Throwable;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.AcsLogger;

/**
 * Insert a Class/Interface comment.
 * 
 */
public class simpleLog{
	private Logger m_logger;
	public simpleLog(Logger logger){
		this.m_logger=logger;
		((AcsLogger)m_logger).addLoggerClass(this.getClass());
	}
	public void log(){
		Level level = AcsLogLevel.WARNING;
/*		LogRecord lr = new LogRecord(level, "Simple Log");
		lr.setMillis(System.currentTimeMillis());
		StackTraceElement[] stTrElems = (new Throwable()).getStackTrace();
		StackTraceElement stTrEl = stTrElems[1];
		lr.setSourceClassName(stTrEl.getClassName());
		lr.setSourceMethodName(stTrEl.getMethodName());
		lr.setLoggerName("java " + System.getProperty("java.version"));
		Map<String, Object> logProperties = new HashMap<String, Object>();
		lr.setParameters(new Object[] {logProperties} );
		logProperties.put("Line", new Long(stTrEl.getLineNumber()));
		logProperties.put("ThreadName", Thread.currentThread().getName());
		try{
			logProperties.put("HostName", InetAddress.getLocalHost().getHostName());
		}
		catch(UnknownHostException e)
		{
			logProperties.put("HostName", "Unknown");
		}

		//logProperties.put("StackId", stackID);
		//logProperties.put("StackLevel", new Long(stackLevel));
		logProperties.put("testProperty", "testProperty value");



		//Logger m_logger = Logger.getLogger("AcsRemoteLogger");
		//Logger m_logger =ClientLogManager.getAcsLogManager().getLoggerForComponent("dddd");
		//m_logger.log(lr);
*/		

		Map<String, Object> nameValue = new HashMap<String, Object>();
		nameValue.put("logName","simpleLog");
		Map<String, Object> logProperties = new HashMap<String, Object>();
		logProperties.put("LogProperties", nameValue);
		m_logger.log(level,"Simple Log",new Object[] {logProperties});
	}
}
