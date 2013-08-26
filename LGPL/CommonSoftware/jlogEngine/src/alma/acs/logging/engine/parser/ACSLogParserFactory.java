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
	 * The supported parsers.
	 * <P>
	 * This has been introduced to allow the usage of a specific parser
	 * especially useful for testing where we need to test parsing against
	 * each possible parser.
	 * 
	 * @author acaproni
	 *
	 */
	public enum ParserTypes {
		DOM,
		VTD
	}
	
	/**
	 * The parser is a singleton built at the first invocation
	 * of <code>getParser()</code>.
	 * <P>
	 * Calls to <code>getParser(ParserType...)</code> do not change the value of this
	 * property.
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
				parser = getParser(ParserTypes.VTD);
				return parser;
			} catch (Throwable t) {
				usingVTD=false;
			}
		} 
		parser =  getParser(ParserTypes.DOM);
		return parser;
	}
	
	/**
	 * Get a parser of the given type.
	 * <P>
	 * This method allows to get a parser of a specific type and is thought for 
	 * testing purposes. 
	 * <P>
	 * If the type of the requested parser is not the type of the parser in use then 
	 * a new parser is instantiated and returned but the parser in use remains untouched.
	 * 
	 * @param parserType The type of the parser to instantiate. It can't be <code>null</code>.
	 * @return The parser of the given type
	 * @throws <{@link Exception} in case of error instantiating the parser.
	 */
	public static ACSLogParser getParser(ParserTypes parserType) throws Exception {
		if (parserType==null) {
			throw new IllegalArgumentException("Tye type can't be null");
		}
		if (parserType==ParserTypes.VTD) {
			parser = new ACSLogParserVTD();
		} else { 
			parser =  new ACSLogParserDOM();
		}
		return parser;
	}
	
	/**
	 * Return the type of the passed parser.
	 * 
	 * @param parser The parser whose type has to be checked. It can't be <code>null</code>.
	 * @return The type of the parser.
	 * @throw <code>Exception</code> if the type of the parser is not recognized/supported.
	 */
	public static ParserTypes getParserType(ACSLogParser parserToCheck) throws Exception {
		if (parserToCheck==null) {
			throw new IllegalArgumentException("The parser can't be null");
		}
		if (parserToCheck instanceof ACSLogParserDOM) {
			return ParserTypes.DOM;
		}
		if (parserToCheck instanceof ACSLogParserVTD) {
			return ParserTypes.VTD;
		}
		throw new Exception("Unknown parser type: "+parserToCheck.getClass().getName());
	}
	
	/**
	 * Return the type of the parser in use.
	 * 
	 * @return The type of the parser in use or <code>null</code> if no parser is still in use 
	 *         i.e. <code>getParser()</code> has not been executed yet.
	 *  @throw <code>Exception</code> if the type of the parser is not recognized/supported.
	 */
	public static ParserTypes getParserType() throws Exception {
		if (parser==null) {
			return null;
		}
		return getParserType(parser);
	}
}
