/*
 ALMA - Atacama Large Millimiter Array
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
 * @version $Id: AcsLogRecord.java,v 1.3 2007/05/28 06:40:44 cparedes Exp $
 * @since    
 */

package alma.acs.logging;

import java.util.logging.Level;
import java.util.logging.LogRecord;

/**
 * As of ACS 6.0.2, this special LogRecord is only produced by the generated type-safe logs
 * (module loggingts) which automatically set the target audience.
 * <p>
 * Note that in the above scenario of type-safe logs, non-JDK logging data is attached to special fields in a custom log record,
 * which is then logged to a normal JDK logger. 
 * In the standard logging scenario it is the other way around: we use a custom logger, which creates standard LogRecords, and
 * special non-JDK data is attached in the form of special property-style log parameters.
 * @TODO- It may be better to unify the two approaches of storing non-JDK log data.   
 */
public class AcsLogRecord extends LogRecord
{
	private String audience;

	private Object params[];

	public AcsLogRecord(Level level, String msg, Object parameters, String loggerName) {
		super(level, msg);
		this.setLoggerName(loggerName);
		params = new Object[1];
		params[0] = parameters;
		this.setParameters(params);
	}

	public void setAudience(String audience) {
		this.audience = audience;
	}

	public String getAudience() {
		return this.audience;
	}
}
