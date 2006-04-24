/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.acs.logging.formatters;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.LogRecord;

/**
 * Class that encapsulates access to <code>logRecord.getParameters()</code> and handling of a special Property-Map parameter.
 * <p>
 * Parts of this code were taken out from AcsXMLLogFormatter.
 * 
 * @author hsommer
 * created Dec 14, 2004 5:41:33 PM
 */
public class LogParameterExtractor {

    public static final String PARAM_THREAD_NAME = "ThreadName";
    public static final String PARAM_LINE = "Line";
    
    private LogRecord currentLogRecord;
    
    /** 
     * A <code>Map</code>that is used as an optional special log parameter. 
     * It transports information from the application to the log formatter
     * which could otherwise not be transported using the JDK's <code>LogRecord</code>. 
     */ 
    private Map properties;
    
    private List<Object> otherParameters;


    public LogParameterExtractor() {
    }

    public LogParameterExtractor(LogRecord currentLogRecord) {
        setCurrentLogRecord(currentLogRecord);
    }
    
    
    /**
     * Sets the log record from which the other methods can extract information.
     * @param logRecord
     */
    public void setCurrentLogRecord(LogRecord logRecord) {
        
        if (currentLogRecord == logRecord) {
            return;
        }
        
        currentLogRecord = logRecord;
        
		properties = null;
		otherParameters = new ArrayList<Object>();
		
        Object[] parameters = logRecord.getParameters();
		if (parameters != null) {
			for (int i = 0; i < parameters.length; i++) {				
				// // arbitrary rule to pick first Map as the special properties Map; in practice there should not be any other Map.
				if (parameters[i] instanceof Map && properties == null) { 
					properties = (Map) parameters[i];
				}
				else {
				    otherParameters.add(parameters[i]);
				}				    
			}
		}
    }

    
    /**
    * Returns parameters of the current log record which a different from a special Properties <code>Map</code>.
    * @return array of parameters, possibly empty, but never null. 
    */
    public Object[] getNonPropertiesMapParameters() {
        return otherParameters.toArray();
    }
    
    
	/**
	 * Extract property with specified name of type long from the <code>Map</code>
	 * contained in the object of type <code>java.lang.Long</code>.
	 * @param name			name of the property
	 * @param defaultValue value returned if failed to obtain property value
	 * @return				value, <code>defaultValue</code> on failure
	 */
	public long extractLongProperty(String name, long defaultValue)
	{

		long retVal = defaultValue;
		if (properties != null)
		{
			Object lv = properties.get(name);
			if (lv != null && lv instanceof Long)
				retVal = ((Long) lv).longValue();
		}

		return retVal;
	}

	/**
	 * Extract property with specified name of type String from the <code>Map</code>
	 * contained in the object of type <code>java.lang.String</code>.
	 * @param name			name of the property
	 * @param defaultValue value returned if failed to obtain property value
	 * @return				value, <code>defaultValue</code> on failure
	 */
    public String extractStringProperty(String name, String defaultValue)
	{

		String retVal = defaultValue;
		if (properties != null)
		{
			Object sv = properties.get(name);
			if (sv != null)
				retVal = sv.toString();
		}

		return retVal;
	}

	/**
	 * Extract property with specified name of type Map from the <code>Map</code>
	 * contained in the object of type <code>java.util.Map</code>.
	 * @param name			name of the property
	 * @param defaultValue value returned if failed to obtain property value
	 * @return				value, <code>defaultValue</code> on failure 
	 */
    public Map extractMapProperty(String name, Map defaultValue)
	{
		Map retVal = defaultValue;
		if (properties != null)
		{
			Object mv = properties.get(name);
			if (mv != null && mv instanceof Map)
				retVal = (Map) mv;
		}

		return retVal;
	}
	
    
}
