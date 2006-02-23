package alma.acs.logging.formatters;

import java.util.logging.LogRecord;

import alma.acs.logging.ClientLogManager;

public class SourceObjectExtractor {

	/**
	 * Extracts the component or container name from the logger name. 
	 * This is the so-called "SourceObject". 
	 * If extraction fails, the logger namespace is returned.
	 * If the namespace is null, null is returned.
	 * @param record
	 * @return
	 */
	static String getSourceObject(LogRecord record) {
		String sourceObject = record.getLoggerName();
		if (sourceObject != null) {
            // try to strip off fixed prefix from logger namespace
            try {
                if (sourceObject.startsWith(ClientLogManager.NS_COMPONENT)) {
                    sourceObject = sourceObject.substring(ClientLogManager.NS_COMPONENT.length()+1);
                }
                else if (sourceObject.startsWith(ClientLogManager.NS_CONTAINER)) {
                    sourceObject = sourceObject.substring(ClientLogManager.NS_CONTAINER.length()+1);
                }
            } catch (Exception e) {
                // fallback: use logger namespace
            }            
        }
		return sourceObject;
	}
}
