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
 * Created on Jul 28, 2008 by mschilli
 */
package alma.acs.gui.standards;

import java.awt.GridLayout;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;




public class Example {

	public static void main (String[] args) {
		try {
			new Example().example();
			// new Example().test();

		} catch (Exception exc) {
			exc.printStackTrace();
		}
	}

	void example() throws IOException {

		GuiStandards.enforce();

		JPanel window = createWindow();

		JComponent widget1 = createWidget("widget 1 with default settings"); 
		window.add(widget1);
		window.validate();

		// save as file, then edit it, and load again
		File file = new File("colorsetting.props");
		storeCurrentSettings(file);
		editSettingsOnDisk(file);
		loadCustomSettings(file);

		JComponent widget2 = createWidget("widget 2 with customized settings");
		window.add(widget2);
		window.validate();
	}
	
	
	
	// How to store and load settings
	// ===================================================


	void storeCurrentSettings (File file) throws IOException {
		Properties p = GuiStandards.current();
		String comment = GuiStandards.comment();

		FileOutputStream fos = new FileOutputStream(file);
		p.store(fos, comment);
		fos.close();

		System.out.println("stored gui settings to "+file);
	}

	
	void loadCustomSettings (File file) throws IOException {
		Properties p = new Properties();
		FileInputStream fis = new FileInputStream(file);
		p.load(fis);
		fis.close();

		GuiStandards.redefine(p);

		System.out.println("loaded gui settings from "+file);
	}

	
	
	// How to create widgets
	// ===================================================


	JComponent createWidget (String message) {
		JLabel comp = new JLabel (message);
		comp.setOpaque(true);
		comp.setIcon(StandardIcons.ACTION_STOP.icon);

		return comp;
	}
	
	
	
	// Proprietary helpers for this Demo
	// ===================================================

	JPanel createWindow() {
		JPanel panel = new JPanel(new GridLayout(0,1,10,10));
		JDialog d = new JDialog((JDialog)null, "Example", false);
		d.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		d.add(panel);
		d.setSize(300,200);
		d.setVisible(true);
		return panel;
	}

	void editSettingsOnDisk (File file) throws IOException {
		Properties p = new GuiStandards.SortedProperties();

		FileInputStream fis = new FileInputStream(file);
		p.load(fis);
		fis.close();

		p.put("TEXT_FG_COL", "yellow");
		p.put("MAIN_BG_COL", "777777");
		
		FileOutputStream fos = new FileOutputStream(file);
		p.store(fos, "");
		fos.close();
	}
	
	void test() throws IOException {
		// store current
		Properties p = GuiStandards.current();
		String comment = GuiStandards.comment();

		p.store(System.out, comment);
		System.out.println("---");

		// change and apply
		p.put("MAIN_BG_COL", "223344");
		GuiStandards.redefine(p);
		for (StandardColors c : StandardColors.values())
			System.out.println(c + "=" + c.color);
	}


	
}








