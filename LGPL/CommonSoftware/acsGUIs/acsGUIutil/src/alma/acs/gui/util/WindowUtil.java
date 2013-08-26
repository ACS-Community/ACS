/*
 Created on Aug 17, 2011 by mschilli

 ALMA - Atacama Large Millimiter Array
 (c) European Southern Observatory, 2011

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */
package alma.acs.gui.util;

import java.awt.Frame;
import java.awt.Rectangle;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Helper methods for dealing with Java Windows.
 *
 * @since August 2011
 * @author M.Schilling, ESO
 */
public class WindowUtil {
	/* 
	 * Code reviewed on 17 Aug 2011 by Rodrigo and Marcus.
	 */

	/**
	 * Assigns position and size to a desktop frame from a textual
	 * geometry definition. For allowed formats of geometry definition,
	 * see {@link #parseGeometry(String)}.
	 * 
	 * If {@code geometry} is {@code null} or cannot be understood, this call
	 * will do nothing and return {@code false}.
	 *
	 * @param frame - a desktop frame to position and size
	 * @param geometry - a geometry definition string 
	 * @return whether the geometry definition was valid and thus applied
	 */
	public static boolean applyGeometry (Frame frame, String geometry) {
		Rectangle rect = null;
		try {
			rect = parseGeometry(geometry);
		} catch (IllegalArgumentException exc) {
			return false;
		}

		if (rect.width <= 0 || rect.height <= 0) {
			frame.pack();
			frame.setLocation (rect.x, rect.y);
			return true;
		}

		frame.setBounds(rect);
		return true; 
	}

	/**
	 * Parses a geometry string made up of positional parameters
	 * <i>"width height xpos ypos"</i> or <i>"xpos ypos"</i> (ie. X server format),
	 * or named parameters <i>"x=...,y=...,w=...,h=..."</i>.
	 * <p>
	 * <u>Examples:</u><ul>
	 * <li>400x500+200+300
	 * <li>+200+300
	 * <li>x=200, y=300, w=400, h=500
	 * <li>x=200y=300w=400h=500
	 * <li>w=400;h=500;x=200;y=300;
	 * </ul>
	 *
	 * Note that negative x/y coordinates (ie. positioning leftwards from the
	 * screen's right border) are not supported.
	 * <p>
	 * This method throws {@code IllegalArgumentException} if the geometry string is invalid.
	 * Using this method and catching that exception provides a simple way of testing a geometry
	 * string before use, e.g. when you are parsing command-line arguments.
	 * <p>
	 * The returned rectangle will contain {@code 0} for any coordinate that was
	 * not defined in the geometry string. This can happen for strings specified in
	 * "X server short notation" (where only x and y are defined), and for strings
	 * specified in "named parameters" notation (if parameters were omitted).
	 *
	 * @param geometry - the geometry as text
	 * @return the geometry as a rectangle
	 * @throws IllegalArgumentException if the argument is null or cannot be parsed
	 */
	public static Rectangle parseGeometry (String geometry) throws IllegalArgumentException {

		/* msc 2011-08: for allowing negative x/y (ie. counting leftwards from the right border)
		 * it is necessary to change "PositiveNumber" below to become "SignedNumber", and
		 * to find out the real screen size in a multi-screen environment (GraphicsEnvironment).
		 * the latter seems to be unpredictable on some machines/platforms, so i'm skipping it.
		 */

		if (geometry == null)
			throw new IllegalArgumentException("Bad geometry: null");


		Rectangle r = new Rectangle();
		Matcher m;

		m = p1.matcher (geometry);
		if (m.matches()) {
			r.width = asInt(m.group(1));
			r.height = asInt(m.group(2));
			r.x = asInt(m.group(3));
			r.y = asInt(m.group(4));
			return r;
		}

		m = p2.matcher (geometry);
		if (m.matches()) {
			r.x = asInt(m.group(1));
			r.y = asInt(m.group(2));
			return r;
		}

		m = p3.matcher(geometry);
		if (m.matches()) {
			for (int i = 1; i <= m.groupCount(); i++) {
				if (m.group(i).equals("w"))
					r.width = asInt(m.group(++i));
				if (m.group(i).equals("h"))
					r.height = asInt(m.group(++i));
				if (m.group(i).equals("x"))
					r.x = asInt(m.group(++i));
				if (m.group(i).equals("y"))
					r.y = asInt(m.group(++i));
			}
			return r;
		}

		throw new IllegalArgumentException("Bad geometry "+geometry+". Must be "+hr1+" or "+hr2+" or "+hr3);
	}

	private final static String Number = "(\\d+)";
	private final static String AnyNaN = "\\D*";
	private final static String NamVal = "([whxy])=" + Number;
	private final static String Multiply  = "[xX]";
	private final static String PositiveNumber = "([\\+]\\d+)";

	private final static String hr1 = "WIDTHxHEIGHT+XPOS+YPOS";
	private final static Pattern p1 = Pattern.compile (Number + Multiply + Number + PositiveNumber + PositiveNumber);

	private final static String hr2 = "+XPOS+YPOS";
	private final static Pattern p2 = Pattern.compile (PositiveNumber + PositiveNumber);

	private final static String hr3 = "x=XPOS,y=YPOS,w=WIDTH,h=HEIGHT";
	private final static Pattern p3 = Pattern.compile(NamVal + AnyNaN + NamVal + AnyNaN + NamVal + AnyNaN + NamVal + AnyNaN);

	private static int asInt (String number) {
		if (number.charAt(0) == '+') // Integer class cannot parse "+" prefix
			number = number.substring(1);
		return Integer.parseInt (number);
	}

	
	// ============================================================================
}

