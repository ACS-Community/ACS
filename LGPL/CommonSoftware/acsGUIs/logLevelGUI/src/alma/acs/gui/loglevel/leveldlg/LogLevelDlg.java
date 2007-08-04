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
package alma.acs.gui.loglevel.leveldlg;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.TitledBorder;

import si.ijs.maci.LoggingConfigurable;

/**
 * The dialog to see and changes the log level
 * 
 * @author acaproni
 *
 */
public class LogLevelDlg extends JDialog implements ActionListener {
	
	// The button to close the dialog
	private JButton doneBtn = new JButton("Done");
	
	// The Button to apply the changes
	private JButton applyBtn = new JButton("Apply");
	
	// The LoggingConfigurable
	private LoggingConfigurable logConf=null;
	
	// The table of log levels
	private LogLevelTable table;
	private LogLevelModel model;
	
	/**
	 * Constructor 
	 * 
	 * @param configurable The LoggingConfigurable whose log level
	 *                     the user wants to read or set
	 * @param title The name of the configurable to add to the tile
	 */
	public LogLevelDlg(LoggingConfigurable configurable, String name) {
		super();
		if (configurable==null) {
			throw new IllegalArgumentException("Invalid null LoggingConfigurable in constructor");
		}
		logConf=configurable;
		setTitle("Log level configurator: "+name);
		initialize(name);
	}
	
	/**
	 * Init the GUI
	 */
	private void initialize(String name) {
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		setLocation(50, 50);
		setTitle("Log level: "+name);
		
		// Set tooltip to buttons
		doneBtn.setToolTipText("Close the dialog");
		applyBtn.setToolTipText("Apply the changes");
		
		// Add the botton at the bottom
		JPanel btnPnl = new JPanel();
		btnPnl.add(doneBtn,BorderLayout.EAST);
		btnPnl.add(applyBtn,BorderLayout.WEST);
		doneBtn.addActionListener(this);
		applyBtn.addActionListener(this);
		
		add(btnPnl,BorderLayout.SOUTH);
		
		// Add the widgets with the log levels at the center
		add(initLogLevelsPanel(),BorderLayout.CENTER);
		pack();
	}
	
	/**
	 * Initialize the log level panel (i.e. the table)
	 * 
	 * @return The panel with the table of log levels
	 */
	private JComponent initLogLevelsPanel() {
		LogLevelHelper[] levels=null;
		try {
			levels = loggersLbl();
		} catch (Exception e) {
			JLabel lbl = new JLabel("No log levels available");
			lbl.setToolTipText("Function not implemented");
			return lbl;
		}
		
		model = new LogLevelModel(levels);
		table = new LogLevelTable(model);
		JScrollPane scrollPane = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollPane.setViewportView(table);
		return scrollPane;
	}
	
	/**
	 * set the lable for the logger names 
	 * 
	 * @return
	 */
	private LogLevelHelper[] loggersLbl() throws Exception {
		String[] logNames = logConf.get_logger_names();
		if (logNames==null) {
			return new LogLevelHelper[0];
		}
		LogLevelHelper[] ret = new LogLevelHelper[logNames.length];
		for (int t=0; t<logNames.length; t++) {
			ret[t]= new LogLevelHelper(logNames[t],logConf.get_logLevels(logNames[t]));
		}
		return ret;
	}
	
	/**
	 * 
	 * @param e
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==doneBtn) {
			// Close the dialog after checking if the user changed log levels
			if (userChangedLogLevels()) {
				if (JOptionPane.showConfirmDialog(
						null, 
						"Do you really want to discard changes?", 
						"Confirm", 
						JOptionPane.YES_NO_OPTION)==JOptionPane.NO_OPTION) {
					return;
				}
			}
			setVisible(false);
			logConf=null;
			dispose();
		} else if (e.getSource()==applyBtn) { 
			applyChanges();
		} else {
			throw new IllegalStateException("Unknown source of events: "+e.getSource());
		}
	}
	
	/**
	 * 
	 * Apply the changes to the log levels, if any
	 *
	 */
	private void applyChanges() {
		LogLevelHelper[] newLevels = model.getLevels();
		for (LogLevelHelper logLvl: newLevels) {
			if (logLvl.modified()) {
				System.out.println("Applying new log levels to "+logLvl.getName());
				try {
					logConf.set_logLevels(logLvl.getName(), logLvl.getLogLevels());
				} catch (Throwable t) {
					System.err.println("Exception caught while setting log level "+logLvl.getName()+": "+t.getMessage());
					t.printStackTrace(System.err);
				}
			}
		}
		model.changesApplied();
	}
	
	/**
	 * Check if the user changed one of the logger level
	 * 
	 * @return true if the user changed at least one level
	 */
	public boolean userChangedLogLevels() {
		if (model==null) {
			// The model is null if the table is not shown
			// because the client does not support 
			// logLevels operations
			return false;
		}
		LogLevelHelper[] newLevels = model.getLevels();
		for (LogLevelHelper logLvl: newLevels) {
			if (logLvl.modified()) {
				return true;
			}
		}
		return false;
	}
}
