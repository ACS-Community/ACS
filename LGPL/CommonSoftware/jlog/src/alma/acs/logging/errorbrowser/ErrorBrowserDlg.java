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
package alma.acs.logging.errorbrowser;

import java.awt.Dimension;
import java.awt.Point;
import java.lang.reflect.InvocationTargetException;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

import com.cosylab.logging.LoggingClient;

import alma.acs.logging.table.LogEntryTableModelBase;

/**
 * The dialog showing the stack trace of errors.
 * <P>
 * The dialog has one tab for each stack trace whose title is the stack ID.
 * 
 * @author acaproni
 *
 */
public class ErrorBrowserDlg extends JDialog {
	
	/**
	 * The pane showing the error tabs
	 */
	private JTabbedPane tabbedPane = new JTabbedPane();
	
	/**
	 * The logging client
	 */
	private final LoggingClient loggingClient;

	/**
	 * Constructor
	 * 
	 * @param client A not <code>null</code> reference to the <code>LoggingClient</code>
	 */
	public ErrorBrowserDlg(LoggingClient client) {
		super((JFrame)null, "Error browser");
		if (client==null) {
			throw new IllegalArgumentException("The LoggingClient can't be null");
		}
		loggingClient=client;
		setDefaultCloseOperation(JDialog.HIDE_ON_CLOSE);
		initialize();
	}
	
	/**
	 * Init the GUI.
	 * <P>
	 * The dialog is composed of a set of tabs, one for each stack trace.
	 */
	private void initialize() {
		add(tabbedPane);
		setMinimumSize(new Dimension(400,200));
		setPreferredSize(new Dimension(500,300));
		pack();
		setVisible(true);
	}
	
	/**
	 * Add a new error tab.
	 * <P>
	 * The tab is added by a separate thread.
	 * 
	 * @param sourceModel The model used by the <code>Engine</code> to look for logs
	 *                    belonging to the error trace with the give <code>STACKID</code>
	 * @param stackId The <code>STACKID</code> of logs of the stack trace
	 */
	public synchronized void addErrorTab(final LogEntryTableModelBase sourceModel, final String stackId) {
		if (stackId==null || stackId.isEmpty()) {
			throw new IllegalArgumentException("The stackID can't be null nor empty");
		}
		if (sourceModel==null) {
			throw new IllegalArgumentException("The model can't be null");
		}
		removeDuplacatedTab(stackId);
		
		// Add the new tab
		try {
			SwingUtilities.invokeAndWait(new Runnable() {
				public void run() {
					ErrorTab newTab;
					try {
						newTab = new ErrorTab(sourceModel,stackId);
					} catch (Exception e) {
						System.err.println("Error creating an error tab: "+e.getMessage());
						e.printStackTrace();
						return;
					}
					tabbedPane.insertTab(stackId, null, newTab, "Stack trace for ID "+stackId, 0);
					tabbedPane.setTabComponentAt(0, new TabComponent(tabbedPane));
					setVisible(true);
				}
			});
		} catch (Exception e) {
			System.err.println("Error adding tab: "+e.getMessage());
			e.printStackTrace(System.err);
		}
	}
	
	/**
	 * Close the dialog
	 */
	public void close() {
		for (int t=0; t<tabbedPane.getTabCount(); t++) {
			ErrorTab tab = (ErrorTab)tabbedPane.getComponentAt(t);
			if (tab!=null) {
				tab.close();
			} 
		}
		setVisible(false);
		dispose();
	}
	
	/**
	 * Remove all the tabs with the given title, if any.
	 * <P>
	 * The removal runs synchronously inside the vent dispatcher EDT.
	 * 
	 * @param title The title of the tab
	 */
	private void removeDuplacatedTab(final String tabTitle) {
		// Check if a tab with the given name already exists
		while (true) {
			try {
				SwingUtilities.invokeAndWait(new Runnable() {
					public void run() {
						for (int t=0; t<tabbedPane.getTabCount(); t++) {
							if (tabbedPane.getTitleAt(t).equals(tabTitle)) {
								tabbedPane.removeTabAt(t);
								break;
							} 
						}
					}
				});
				return;
			} catch (InterruptedException e) {
				// If the thread has been interrupted then retry
				e.printStackTrace();
				continue;
			} catch (InvocationTargetException e) {
				// If an error happens while executing run() then prints out a message
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * Override <code>setVisible()</code> to move the dialog
	 * over the logging client and in front of other windows
	 */
	@Override
	public void setVisible(boolean visible) {
		boolean wasVisibel=isVisible();
		super.setVisible(visible);
		// Move the win on top of jlog
		if (visible && isShowing()) {
			Point loggingPos = loggingClient.getLocationOnScreen();
			if (!wasVisibel) {
				setLocation(loggingPos);
			}
			toFront();
		}
	}
	
}
