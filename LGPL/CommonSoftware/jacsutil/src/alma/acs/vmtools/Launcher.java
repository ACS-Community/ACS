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
 * Created on Oct 27, 2005 by mschilli
 */
package alma.acs.vmtools;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.lang.reflect.Method;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;



/**
 * The VmTools Launcher.
 * 
 * <p><b>What it does:</b>
 * VmTools Launcher will first put up a little quick-starter Gui, then it
 * will run the actual application. The quick-starter Gui contains buttons
 * to run the actual tools, the actual application will be started through
 * its <code>main(String[])</code> method.
 * 
 * <p><b>Using VmTools Launcher:</b>
 * The first command line argument to the Launcher must be the fully qualified
 * name of the application's main class (containing the main method of the application).
 * This first element will be stripped off, and all following arguments will be
 * passed to the application.</p>
 * 
 * <p><b>Adding more VmTools:</b>
 * A VmTool <ul>
 * <li>must be a descendant of JComponent
 * <li>must possess a public zero-arg constructor
 * </ul>
 * The VmTool's class is to be added to the class array implemented in this class.
 * This may change to something more dynamic in a future version.</p>
 * 
 *
 * @author mschilli
 */
public class Launcher {

	
	Class[] toolClasses = new Class[]{LogManagerGui.class, SysPropEditor.class};
	

	// ================================================
	// Launching VmTools & Application
	// ================================================

	/**
	 * Runs the Launcher Gui, then runs the application (specified as first argument).
	 */
	public static void main (String[] args) {

		if (args.length == 0) {
			errorStop("No application specified: Give application class as first argument");
		}

		// --- run the launcher gui

		try {
			Launcher inst = new Launcher();
		} catch (Throwable exc) {
			error("Failed to start VmTools Launcher: "+exc);
		}

		// --- run the application

		launchApplication(args);
	}

	static void launchApplication(String[] args) {
		
		// strip off first arg
		String applicationClass = args[0];

		// all other args will be passed to the application
		String[] applicationArgs = new String[args.length - 1];
		System.arraycopy(args, 1, applicationArgs, 0, applicationArgs.length);

		// try to load and run application
		try {
			ClassLoader cl = Thread.currentThread().getContextClassLoader();
			Class c = Class.forName(applicationClass, true, cl);
			Method main = c.getDeclaredMethod("main", new Class[]{String[].class});
			Object applicationInst = c.newInstance();
			main.invoke(applicationInst, new Object[]{applicationArgs});

		} catch (Exception exc) {
			exc.printStackTrace();// ///////////////
			errorStop("Failed to run class '" + applicationClass + "': " + exc);
		}
	}
	
	
	private static void errorStop (String message) {
		String me = Launcher.class.getName();
		System.err.println(me + ": Stop. " + message);
		System.exit(1);
	}

	private static void error (String message) {
		String me = Launcher.class.getName();
		System.err.println(me + ": " + message);
	}

	
	// ================================================
	// VmTools Launcher's  Gui
	// ================================================

	
	protected JFrame launcherFrame;

	public Launcher() {
		launcherFrame = createFrame("Acs VmTools");

		JPanel buttons = new JPanel();

		for (int i = 0; i < toolClasses.length; i++) {
		
			Class toolClass = toolClasses[i];
			String toolClassName = toolClass.getName();  
			
			try {
				String toolName = toolClassName.substring(toolClassName.lastIndexOf('.')+1);
			
				if (JComponent.class.isAssignableFrom(toolClass)) {
					JButton btn = createButton(toolName, toolClass);
					buttons.add(btn);
				} else {
					error("Won't add '"+toolClassName+"' to VmTools Launcher: Only descendants of JComponent can be added");
				}
			
			} catch (Exception e) {
				error("Failed to add '"+toolClassName+"' to VmTools Launcher");
			}
		}
		
		launcherFrame.getContentPane().add(buttons);
		launcherFrame.pack();
		launcherFrame.setVisible(true);
	}

	
	
	protected JFrame createFrame (String title) {
		final JFrame ret = new JFrame(title);
		ret.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		ret.addWindowListener(new WindowAdapter() {

			@Override
			public void windowClosing (WindowEvent evt) {
				int answer = JOptionPane.showConfirmDialog(ret, "Really close Acs VmTools?", "Close Window", JOptionPane.YES_NO_OPTION);
				if (answer == JOptionPane.YES_OPTION) {
					ret.setVisible(false);
					ret.dispose();
				}
			}
		});
		return ret;
	}

	protected JButton createButton (String toolName, Class toolClass) {
		JButton ret = new JButton(toolName);
		ret.setToolTipText("Start " + toolName);

		class ToolStarter implements ActionListener {

			protected String toolName;
			protected Class toolClass;

			protected JComponent toolInstance;
			protected JDialog toolWindow;

			protected ToolStarter(String toolName, Class toolClass) {
				this.toolName = toolName;
				this.toolClass = toolClass;
			}

			public void actionPerformed (ActionEvent evt) {
				if (toolInstance == null) {
					try {
						toolInstance = (JComponent) toolClass.newInstance();
					} catch (Exception exc) {
						error("Failed to launch tool '" + toolName + "': " + exc);
						return;
					}
					toolWindow = new JDialog(launcherFrame, toolName, false);
					toolWindow.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
					toolWindow.getContentPane().add(toolInstance);
				}
				toolWindow.setVisible(true);
			}
		}
		
		ToolStarter ts = new ToolStarter(toolName, toolClass);
		ret.addActionListener(ts);
		return ret;
	}

	

}


