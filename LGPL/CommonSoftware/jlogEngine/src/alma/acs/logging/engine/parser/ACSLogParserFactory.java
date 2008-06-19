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
 * There are 2 parsers available: DOM and VTD.
 * VTD is ACS/LGPL/Tools and installed by ACS. It is licensed under GPL and available
 * at http://vtd-xml.sourceforge.net/
 * VTD claims to be very fast (and effectively it performs better then DOM) so the factory
 * tries to instantiate a VTD parser if it is present.
 * <BR>
 * Having this factory allows to transparently use a different implementation at run-time
 * depending on the real availability of the parsers.
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
	 * This property is used to check if VTD is installed to avoid trying to instantiate
	 * if the library is missing
	 * <P> 
	 * It is initially set to <code>true</code> to try to instantiate VTD the first time
	 * <code>getParser()</code> is called
	 */
	private static boolean usingVTD=true;
	
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
		if (parser!=null) {
			return parser;
		}
		if (usingVTD) {
			try {
				// Initially try to in instantiate VTD-XML parser
				parser = new ACSLogParserVTD();
				return parser;
			} catch (Throwable t) {
				usingVTD=false;
			}
		} 
		parser =  new ACSLogParserDOM();
		return parser;
	}
}
