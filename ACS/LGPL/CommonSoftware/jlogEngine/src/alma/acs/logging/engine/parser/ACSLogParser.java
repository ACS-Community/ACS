/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
package alma.acs.logging.engine.parser;


import com.cosylab.logging.engine.ACS.LogParseException;
import com.cosylab.logging.engine.log.ILogEntry;

/**
 * Interface to allow different types of parsers to be plugged in. Currently, two types
 * exist, perhaps more could be written in the future. However, in all likelihood we will
 * only use one (e.g. the fastest one) in the 'real' world.
 * 
 * @author sharring
 * 
 */
public interface ACSLogParser {

	/**
	 * Parses the xmlLog. This method must be synchronized to ensure that the
	 * parser parses only one log at a time.
	 * 
	 * @param xmlString the xml string to parse.
	 * @return object implementing the ILogEntry interface, containing the parsed data for 
	 *         the log.
	 * @throws LogParseException if parsing problems are encountered.
	 */
	public abstract ILogEntry parse(String xmlString) throws LogParseException;

}