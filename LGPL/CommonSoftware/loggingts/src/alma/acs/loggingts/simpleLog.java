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
 * @version $Id: simpleLog.java,v 1.2 2007/02/08 09:46:15 nbarriga Exp $
 * @since    
 */

package alma.acs.loggingts;

//import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.Level;
//import java.net.InetAddress;
//import java.net.UnknownHostException;
import java.util.HashMap;
//import java.util.Iterator;
import java.util.Map;
//import java.util.Properties;
//import java.lang.Throwable;

import alma.acs.logging.AcsLogLevel;
//import alma.acs.logging.ClientLogManager;
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

		Map<String, Object> nameValue = new HashMap<String, Object>();
		nameValue.put("logName","simpleLog");
		m_logger.log(level,"Simple Log", nameValue);
		//System.out.println("blabla2");



		
	}
}
