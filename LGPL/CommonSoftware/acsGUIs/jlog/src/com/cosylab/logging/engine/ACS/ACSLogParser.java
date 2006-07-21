package com.cosylab.logging.engine.ACS;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * Interface to allow different types of parsers to be plugged in. Currently, two types
 * exist, perhaps more could be written in the future. However, in all likelihood we will
 * only use one (e.g. the fastest one) in the 'real' world.
 * 
 * @author sharring
 * @see ACSLogParserDOM
 */
public interface ACSLogParser {

	/**
	 * Parses the xmlLog. This method must be synchronized to ensure that the
	 * parser parses only one log at a time.
	 * @param xmlString the xml string to parse.
	 * @return object implementing the ILogEntry interface, containing the parsed data for 
	 *         the log.
	 * @throws LogParseException if parsing problems are encountered.
	 */
	public abstract ILogEntry parse(String xmlString) throws LogParseException;

}