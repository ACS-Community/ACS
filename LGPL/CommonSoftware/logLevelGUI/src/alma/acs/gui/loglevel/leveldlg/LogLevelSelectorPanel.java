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

import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.TitledBorder;

import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.LogTypeRenderer;

import alma.acs.gui.loglevel.LogLvlSelNotSupportedException;

import si.ijs.maci.LoggingConfigurable;
import si.ijs.maci.LoggingConfigurableOperations;

/**
 * The panel to select the log level of the named loggers
 * 
 * @author acaproni
 *
 */
public class LogLevelSelectorPanel extends JPanel implements ActionListener {
	
	// The Button to apply the changes
	private JButton applyBtn = new JButton("Apply");
	
	// The LoggingConfigurable
	private LoggingConfigurableOperations logConf=null;
	
	// The table of log levels
	private LogLevelTable table;
	private LogLevelModel model;
	
	
	private JComboBox allLocalCB;
	private JComboBox allGlobalCB;
	public LogTypeRenderer editorLocal;
	public LogTypeRenderer editorGlobal;
	
	private JCheckBox defaultCB = new JCheckBox("",false);
	
	/**
	 * Constructor 
	 * 
	 * @param owner The windo that owns this dialog (it can be null)
	 * @param configurable The LoggingConfigurable whose log level
	 *                     the user wants to read or set
	 * @param title The name of the configurable to add to the tile
	 * @throws LogLvlSelNotSupportedException If the configurable does not support selection
	 */
	public LogLevelSelectorPanel(LoggingConfigurableOperations configurable, String name) throws LogLvlSelNotSupportedException {
		if (configurable==null) {
			throw new IllegalArgumentException("Invalid null LoggingConfigurable in constructor");
		}
		
		// The editor for local and global log levels
		String[] descs = new String[LogTypeHelper.values().length];
		
		for (int t=0; t<descs.length; t++) {
			descs[t]=LogTypeHelper.values()[t].logEntryType;
		}
		allLocalCB=new JComboBox(descs);
		allGlobalCB=new JComboBox(descs);
		editorLocal= new LogTypeRenderer();
		editorGlobal= new LogTypeRenderer();
		
		logConf=configurable;
		setName(name);
		initialize(name);
	}
	
	/**
	 * Init the GUI
	 * 
	 * @throws LogLvlSelNotSupportedException If the configurable does not support selection
	 */
	private void initialize(String name) throws LogLvlSelNotSupportedException {
		BoxLayout layout = new BoxLayout(this,BoxLayout.Y_AXIS);
		setLayout(layout);
		
		// Add the widgets with the log levels at the center
		add(initLogLevelsPanel());
		
		// Add the panel to set all the levels
		add(initAllLoggersPanel());
		
		// Set tooltip to buttons
		applyBtn.setToolTipText("Apply the changes");
		
		// Add the botton at the bottom
		JPanel btnPnl = new JPanel(new FlowLayout(FlowLayout.CENTER));
		btnPnl.add(applyBtn);
		applyBtn.addActionListener(this);
		
		add(btnPnl);
	}
	
	/**
	 * Initialize the log level panel (i.e. the table)
	 * 
	 * @return The panel with the table of log levels
	 * 
	 * @throws LogLvlSelNotSupportedException If the configurable does not support selection
	 */
	private JComponent initLogLevelsPanel() throws LogLvlSelNotSupportedException {
		LogLevelHelper[] levels=null;
		try {
			levels = loggersLbl();
		} catch (Exception e) {
			throw new LogLvlSelNotSupportedException("Function not yet implemented by "+getName(),e);
		}
		
		model = new LogLevelModel(levels);
		table = new LogLevelTable(model);
		
		JScrollPane scrollPane = new JScrollPane(table);
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
	 * 
	 * @see java.awt.event.ActionListener
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==applyBtn) { 
			applyChanges();
		} else if (e.getSource()==defaultCB) {
			LogLevelHelper[] levels = ((LogLevelModel)table.getModel()).getLevels();
			for (LogLevelHelper lvl: levels) {
				lvl.setUseDefault(defaultCB.isSelected());
			}
			((LogLevelModel)table.getModel()).fireTableDataChanged();
		} else if (e.getSource()==allLocalCB) {
			int newLvl = allLocalCB.getSelectedIndex();
			LogLevelHelper[] levels = ((LogLevelModel)table.getModel()).getLevels();
			for (LogLevelHelper lvl: levels) {
				lvl.setLocalLevel(LogTypeHelper.values()[newLvl]);
			}
			((LogLevelModel)table.getModel()).fireTableDataChanged();
		} else if (e.getSource()==allGlobalCB) {
			int newLvl = allGlobalCB.getSelectedIndex();
			LogLevelHelper[] levels = ((LogLevelModel)table.getModel()).getLevels();
			for (LogLevelHelper lvl: levels) {
				lvl.setGlobalLevel(LogTypeHelper.values()[newLvl]);
			}
			((LogLevelModel)table.getModel()).fireTableDataChanged();
		} else {
			throw new IllegalStateException("Unknown source of events: "+e.getSource());
		}
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
	
	/**
	 * Setup the panel with the option for all the named loggers
	 * 
	 * @return
	 */
	private JPanel initAllLoggersPanel() {
		TitledBorder border = BorderFactory.createTitledBorder("All named loggers actions");
		GridLayout layout = new GridLayout(3,2);
		layout.setVgap(5);
		
		JPanel mainPnl = new JPanel(new FlowLayout(FlowLayout.CENTER));
		JPanel panel = new JPanel(layout);
		panel.setBorder(border);
		
		// Default
		JLabel defaultLbl=new JLabel("Set/unset all the use default");
		panel.add(defaultLbl);
		panel.add(defaultCB);
		
		// local
		JPanel localPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JLabel localLbl=new JLabel("Set all local levels");
		panel.add(localLbl);
		allLocalCB.setRenderer(editorLocal);
		localPnl.add (allLocalCB);
		panel.add(localPnl);
		
		// Global
		JPanel globalPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JLabel globalLbl = new JLabel("Set all global levels");
		panel.add(globalLbl);
		allGlobalCB.setRenderer(editorGlobal);
		globalPnl.add(allGlobalCB);
		panel.add(globalPnl);
		
		mainPnl.add(panel);
		
		// Set the listeners
		defaultCB.addActionListener(this);
		allLocalCB.addActionListener(this);
		allGlobalCB.addActionListener(this);
		
		return mainPnl;
	}
}
