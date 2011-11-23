/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/*
 * Created on Jul 29, 2008 by mschilli
 */
package alma.acs.gui.standards;

import java.util.Collections;
import java.util.Enumeration;
import java.util.Properties;
import java.util.TreeSet;

import javax.swing.UIManager;



/**
 * This class activates the Alma Standard Settings.
 *
 * Also this class helps in saving and loading gui standard settings, see {@link Example}.
 */
public class GuiStandards {

	/**
	 * A call to this method is recommended for applications wanting to use the Alma GUI
	 * Standards. It applies the currently defined properties by means of passing them to
	 * the Swing UI Manager. Swing widgets created hereafter will automatically have the
	 * proper UI settings without the need for programmatic intervention like explicitly
	 * setting their background color.
	 */
	public static void enforce() {

		// 1) iterate over contents of UIManager
		// -----------------------------------------------------------------------
		// the keys held by UIManager typically follow a
		// certain naming convention which we exploit here
		
		Enumeration<Object> en = UIManager.getDefaults().keys();
		while (en.hasMoreElements()) {
			String key = String.valueOf(en.nextElement());

			if (key.endsWith(".background")) {
				if (key.startsWith("Text")
				 || key.startsWith("Password")  
				 || key.startsWith("Table") 
				 || key.startsWith("Tree"))
					UIManager.put(key, StandardColors.EDITOR_BG.color);
				else 
				if (key.startsWith("Button") || key.startsWith("ToggleButton") )
					; // msc: changing the buttons makes it harder to recognize them
				else
					UIManager.put(key, StandardColors.MAIN_BG.color);
			}

			else
			if (key.endsWith(".selectionBackground")) {
				UIManager.put(key, StandardColors.SELECTION_BG.color);
			}

			else
			if (key.endsWith(".foreground")) {
				if (key.startsWith("Spinner")
				 || key.startsWith("ScrollBar")
				 || key.startsWith("Separator")
				 || key.startsWith("ProgressBar")
				 || key.startsWith("Slider"))
					; // msc: recoloring their foreground looks strange
				else
					UIManager.put(key, StandardColors.TEXT_FG.color);
			}
		}

		
		// 2) assign colors for explicit keys
		// -----------------------------------------------------------------------
		// the keys down here don't match the usual naming convention.
		//
		// Or they don't exist at JVM startup yet, and thus are not known
		// to UIManager if this logic is run at JVM startup. this happens
		// for keys installed by graphical libraries (JIDE, for instance).


		// for the arrows at the end of scrollbars
		UIManager.put("control", StandardColors.MAIN_BG.color);

		// for the currently selected tab-handle
		UIManager.put("TabbedPane.selected", StandardColors.SELECTION_BG.color);

		// for user attention tab-handles (self-invented)
		UIManager.put("TabbedPane.attention", StandardColors.STATUS_WARNING_BG.color);
		
		// for JIDE library
		UIManager.put("JideTabbedPane.tabAreaBackground", StandardColors.MAIN_BG.color);

	}

		

	/**
	 * Returns a comment with some explanation, useful when saving settings to a file.
	 */
	public static String comment () {
		String comment = "Custom gui standards for " + System.getProperty("user.name")
				+ "\n   Colors may be specified by name (white, lightGray, gray, darkGray,"
				+ "\n   black, red, pink, orange, yellow, green, magenta, cyan, blue),"
				+ "\n   or as hexadecimal RGB (e.g. 0000FF for the color blue)."
				+ "\n   http://www.easycalculation.com/color-coder.php may be useful."
				+ "\n";
		return comment;
	}

	/**
	 * Returns the current settings, ready for storage to an external location.
	 */
	public static Properties current () {
		Properties p = new SortedProperties();

		for (StandardColors c : StandardColors.values()) {
			p.put(c.toString() + "_COL", StandardColors.encode(c.color));
		}

		/*
		 * Could likewise store the Icon definitions here, but doesn't seem reasonable to
		 * allow custom icons.
		 */

		return p;
	}

	/**
	 * Redefines the current settings according to the specified properties object. The
	 * object may contain a subset of all possible properties, leaving the rest untouched.
	 * This method effects a call to {@link #enforce()}, thus the redefined settings
	 * will affect widgets created hereafter.
	 */
	public static void redefine (Properties p) {
		String val;

		for (StandardColors c : StandardColors.values()) {
			val = p.getProperty (c.toString() + "_COL");
			if (val != null) /* leave untouched if undefined */
				c.color = StandardColors.decode(val);
		}

		/*
		 * Could likewise load the Icon definitions here, but doesn't seem reasonable to
		 * allow custom icons.
		 */

		GuiStandards.enforce();
	}



	// Nested class
	// ====================================================

	/**
	 * This is a properties hashtable that enumerates its keys in alphabetic order.
	 * Alphabetic order is desirable when writing properties to a file.
	 */
	static public class SortedProperties extends Properties {

		/** {@inheritDoc} */
		public SortedProperties() {}

		/** {@inheritDoc} */
		public SortedProperties(Properties defaults) {
			super(defaults);
		}

		/**
		 * Returns an <b>alphabetic</b> enumeration of the keys in this hashtable. This
		 * gets used by the various <code>store...</code> methods.
		 * 
		 * @return an <b>alphabetic</b> enumeration of the keys in this hashtable.
		 * @see #store(java.io.OutputStream, String)
		 * @see #store(java.io.Writer, String)
		 */
		@Override
		public synchronized Enumeration<Object> keys () {
			return Collections.enumeration(new TreeSet<Object>(super.keySet()));
		}

	}

}
