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

/**
 * A factory to build parsers.
 * <P>
 * The purpose of this object is to hide the type of parser really used at run time.
 * <P>
 * The object could instantiate a new parser or use only one instance, a singleton.
 * <P>
 * At the present there is one parser available, <code>ACSLogParserDOM</code> but we are
 * trying to adopt VTD XML parser.
 * <BR>
 * Having this factory allows to transparently use a different implementation at run-time
 * depending on the real availability of the parser.
 *   
 * @author acaproni
 *
 */
public class ACSLogParserFactory {
	
	/**
	 * The parser is a singleton built at the first invocation
	 * of <code>getParser()</code>.
	 */
	private static ACSLogParser parser=null;
	
	/**
	 * Get a parser.
	 * <P>
	 * The <code>ACSLogParser</code> returned by this method can be a new instance
	 * or not, depending on the implementation.
	 * 
	 * @return The parser.
	 * @throws <code>Exception</code> in case of error building the parser
	 */
	public static ACSLogParser getParser() throws Exception {
		if (parser==null) {
			parser =  new ACSLogParserDOM();
		}
		return parser;
	}
}
