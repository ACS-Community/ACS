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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.TitledBorder;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import org.omg.CORBA.SystemException;

import com.cosylab.logging.client.EntryTypeIcon;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.LogTypeRenderer;

import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.Logging.LoggerDoesNotExistEx;
import alma.Logging.LoggingConfigurableOperations;
import alma.Logging.LoggingConfigurablePackage.LogLevels;
import alma.acs.gui.loglevel.LogLvlSelNotSupportedException;
import alma.acs.logging.level.AcsLogLevelDefinition;

/**
 * The panel to select the log level of the named loggers
 * 
 * @author acaproni
 */
public class LogLevelSelectorPanel extends JPanel implements ActionListener {

	private static final long serialVersionUID = 5291375242590375664L;

	// The Button to apply the changes
	private JButton applyBtn = new JButton("Apply");
	
	// The Button to refresh the list
	private JButton refreshBtn = new JButton("Refresh");
	
	// The LoggingConfigurable
	private final LoggingConfigurableOperations logConf;
	
	// The table of log levels
	private LogLevelTable table;
	private LogLevelModel model;
	
	
	private JComboBox allLocalCB;
	private JComboBox allGlobalCB;
	private JLabel    minLocal;
	private JLabel    minGlobal;
	public LogTypeRenderer editorLocal;
	public LogTypeRenderer editorGlobal;
	
	private JButton   defaultBtn = new JButton("Reset all loggers to use default levels");

	private final Logger logger;
	
	
	/**
	 * Constructor 
	 * 
	 * @param owner The window that owns this dialog (it can be null)
	 * @param configurable The LoggingConfigurable whose log level
	 *                     the user wants to read or set
	 * @param logger 
	 * @param title The name of the configurable to add to the tile
	 * @throws LogLvlSelNotSupportedException If the configurable does not support selection
	 */
	//public LogLevelSelectorPanel(LoggingConfigurableOperations configurable, String name) throws LogLvlSelNotSupportedException {
	public LogLevelSelectorPanel(LoggingConfigurableOperations configurable, String name, Logger logger) throws Exception {
		if (configurable==null) {
			throw new IllegalArgumentException("Invalid null LoggingConfigurable in constructor");
		}
		
		this.logger = logger;
		
		// The editor for local and global log levels
		String[] descs = new String[LogTypeHelper.values().length];
		
		for (int t=0; t<descs.length; t++) {
			descs[t]=LogTypeHelper.values()[t].logEntryType;
		}
		allLocalCB=new JComboBox(descs);
		allGlobalCB=new JComboBox(descs);
		editorLocal= new LogTypeRenderer();
		editorGlobal= new LogTypeRenderer();

		ActionListener al = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				applyBtn.setEnabled(true);
			}
		};
		allLocalCB.addActionListener(al);
		allGlobalCB.addActionListener(al);
		
		logConf = configurable;
		setName(name);
		initialize();
	}
	
	/**
	 * Init the GUI
	 * 
	 * @throws LogLvlSelNotSupportedException If the configurable does not support selection
	 */
	//private void initialize(String name) throws LogLvlSelNotSupportedException {
	private void initialize() throws Exception {
		BoxLayout layout = new BoxLayout(this,BoxLayout.Y_AXIS);
		setLayout(layout);
		
		JComponent logLevelsPanel = initLogLevelsPanel(); 

		// Add the panel to set all the levels
		add(initAllLoggersPanel());
		
		// Add the widgets with the log levels at the center
		add(logLevelsPanel);
		
		// Add the panel to show the minimum log levels
		add(initMimimumLevelsPanel());
		
		// Set tooltip to buttons
		applyBtn.setToolTipText("Apply the changes");
		refreshBtn.setToolTipText("Refresh the list");
		
		// Add the botton at the bottom
		JPanel btnPnl = new JPanel(new FlowLayout(FlowLayout.CENTER));
		btnPnl.add(applyBtn);
		btnPnl.add(refreshBtn);
		applyBtn.addActionListener(this);
		refreshBtn.addActionListener(this);
		
		applyBtn.setEnabled(false);

		add(btnPnl);
	}
	
	/**
	 * Initialize the log level panel (i.e. the table)
	 * 
	 * @return The panel with the table of log levels
	 * 
	 * @throws LogLvlSelNotSupportedException If the configurable does not support selection
	 */
	private JComponent initLogLevelsPanel() throws AcsJCORBAProblemEx {

		LogLevelHelper[] levels = loggersLbl();
		
		model = new LogLevelModel(levels);
		model.addTableModelListener(new TableModelListener(){
			public void tableChanged(TableModelEvent e) {
				applyBtn.setEnabled(userChangedLogLevels());
			}
		});
		
		table = new LogLevelTable(model);
		JScrollPane scrollPane = new JScrollPane(table);
		return scrollPane;
	}
	
	/**
	 * Gets the loggers and their levels.
	 * <p>
	 * This method makes remote calls and should not be called in the event thread! 
	 * 
	 * @return
	 * @throws AcsJCORBAProblemEx In case of ORB / network failure or if remote process is unresponsive or unreachable.
	 */
	private LogLevelHelper[] loggersLbl() throws AcsJCORBAProblemEx {

		List<LogLevelHelper> ret = new ArrayList<LogLevelHelper>();
		
		try {
			// get the logger names
			final String[] logNames = logConf.get_logger_names();
			
			// get the log levels for each logger
			for (String logName : logNames) {
				try {
					LogLevels logLevels = logConf.get_logLevels(logName);
					ret.add(new LogLevelHelper(logName, logLevels));
				} catch (LoggerDoesNotExistEx ex) {
					logger.warning("Failed to retrieve log levels info for logger '" + logName + "'. Will skip this logger.");
				}
			}
			return ret.toArray(new LogLevelHelper[0]);
			
		} catch (SystemException ex) {
			AcsJCORBAProblemEx ex2 = new AcsJCORBAProblemEx(ex);
			ex2.setInfo("Failed to retrieve logger names or levels.");
			throw ex2;
		}
	}
	
	/**
	 * Apply the changes to the log levels, if any.
	 * <p>
	 * TODO: This method makes remote calls and should not be called in the event thread.
	 *       However this would require more refactoring, because it currently also accesses swing components.
	 * 
	 * @throws AcsJCORBAProblemEx In case of ORB / network failure or if remote process is unresponsive or unreachable.
	 */
	private void applyChanges() {
		
		try {
			for (LogLevelHelper logLvl : model.getLevels()) {
				if (logLvl.modified()) { // see reset of modification flag in the changesApplied call below
					System.out.println("Applying new log levels to " + logLvl.getName() + ": <"
							+ logLvl.isUsingDefault() + ", " + logLvl.getGlobalLevel() + ", " + logLvl.getLocalLevel()
							+ ">");
					logConf.set_logLevels(logLvl.getName(), logLvl.getLogLevels());
				}
			}
			int localIndex = allLocalCB.getSelectedIndex();
			LogTypeHelper local = LogTypeHelper.values()[localIndex];
			final int localAcs = local.getAcsCoreLevel().value;
			
			int globalIndex = allGlobalCB.getSelectedIndex();
			LogTypeHelper global = LogTypeHelper.values()[globalIndex];
			final int globalAcs = global.getAcsCoreLevel().value;
			
			boolean useDefault = logConf.get_default_logLevels().useDefault;
			LogLevels toset = new LogLevels(useDefault, (short)globalAcs, (short)localAcs);
			logConf.set_default_logLevels(toset);
			
			model.changesApplied();
			updateMinLevels();
			
			applyBtn.setEnabled(false);
			
		} catch (SystemException ex) {
//			AcsJCORBAProblemEx ex2 = new AcsJCORBAProblemEx(ex);
			String msg = "Failed to set log levels for '" + getName() + "' because of Corba errors. Giving up, also for other loggers of the same process.";
			logger.log(Level.WARNING, msg, ex);
			JOptionPane.showMessageDialog(null, msg + " \nCheck the logs for details.", "Error", JOptionPane.ERROR_MESSAGE);
		} catch (Exception ex) {
//			AcsJUnexpectedExceptionEx ex2 = new AcsJUnexpectedExceptionEx(ex);
			String msg = "Failed to set log levels for '" + getName() + "'. Giving up, also for other loggers of the same process.";
			logger.log(Level.WARNING, msg, ex);
			JOptionPane.showMessageDialog(null, msg + " \nCheck the logs for details.", "Error", JOptionPane.ERROR_MESSAGE);
		}
	}
	
	/**
	 * Refresh the list, to see changes made by other operators
	 */
	public void refresh() {
		
		if (userChangedLogLevels()) {
			if (JOptionPane.showConfirmDialog(null, "Do you really want to discard changes?", "Confirm",
					JOptionPane.YES_NO_OPTION) == JOptionPane.NO_OPTION) {
				return;
			}
		}
		
		try {
			refreshAllLoggersPanel();
			applyChanges();
	
			LogLevelHelper[] levels = null;
			levels = loggersLbl();
	
			model.setLevels(levels);
			model.fireTableDataChanged();
	
			applyBtn.setEnabled(false);
		} catch (Exception ex) {
			String msg = "Failed to read loggers or levels levels for '" + getName() + "'.";
			logger.log(Level.WARNING, msg, ex);
			JOptionPane.showMessageDialog(null, msg + " \nCheck the logs for details.", "Error", JOptionPane.ERROR_MESSAGE);
		}
	}
	
	/**
	 * 
	 * @see java.awt.event.ActionListener
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==applyBtn) { 
			applyChanges();
		} else if (e.getSource()==refreshBtn) { 
			refresh();
		} else if (e.getSource()==defaultBtn) {
			LogLevelModel llm = (LogLevelModel)table.getModel();
			llm.setAllToCommonLevels();
		} else if (e.getSource()==allLocalCB) {
			int index  = allLocalCB.getSelectedIndex();
			LogTypeHelper newLvl = LogTypeHelper.values()[index];
			LogLevelModel llm = (LogLevelModel)table.getModel();
			llm.setCommonLocalLevel(newLvl);
		} else if (e.getSource()==allGlobalCB) {
			int index = allGlobalCB.getSelectedIndex();
			LogTypeHelper newLvl = LogTypeHelper.values()[index];
			LogLevelModel llm = (LogLevelModel)table.getModel();
			llm.setCommonGlobalLevel(newLvl);
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
	 * @return the mimimum level
	 */
	private LogTypeHelper getMinLocalLevel() {
		return getMinLogLevel(true);
	}
	
	/**
	 * @return the minimum level
	 */
	private LogTypeHelper getMinGlobalLevel() {
		return getMinLogLevel(false);
	}
	
	/**
	 * Get the minimum log level among the loggers
	 * 
	 * @param isLocal true for local log level, false for global log level 
	 * @return the minimum level
	 */
	private LogTypeHelper getMinLogLevel(boolean isLocal) {
		LogTypeHelper errret = LogTypeHelper.TRACE; 
		LogLevelHelper[] levels; 
		try {
			levels = loggersLbl();
		} catch (Exception e) {
			System.err.println("Function not yet implemented by "+getName());
			return errret;
		}
		// @todo should this be OFF (OFF is kind of special)
		int minval = LogTypeHelper.EMERGENCY.getAcsCoreLevel().value;
		for (LogLevelHelper l : levels) {
			int val = isLocal ? l.getLocalLevel() : l.getGlobalLevel();
			if (minval > val)
				minval = val;
		}
		LogTypeHelper logType;
		try {
			AcsLogLevelDefinition levelDef = AcsLogLevelDefinition.fromInteger(minval);
			logType=LogTypeHelper.fromAcsCoreLevel(levelDef);
		} catch (Exception e) {
			System.err.println("Error parsing a log type: "+minval);
			e.printStackTrace(System.err);
			return errret;
		}
		return logType;
	}

	
	/**
	 * Setup the panel with the option for all the named loggers
	 * 
	 * @return
	 * @throws AcsJCORBAProblemEx 
	 */
	private JPanel initAllLoggersPanel() throws AcsJCORBAProblemEx {
		TitledBorder border = BorderFactory.createTitledBorder("Process wide default log levels");

		JPanel        mainPnl = new JPanel();
		GridBagLayout      gl = new GridBagLayout();
		GridBagConstraints gc = new GridBagConstraints();
		mainPnl.setLayout(gl);
		mainPnl.setBorder(border);
		
		JLabel localLbl  = new JLabel("Default local log level");
		allLocalCB.setRenderer(editorLocal);
		JLabel globalLbl = new JLabel("Default remote log level");
		allGlobalCB.setRenderer(editorGlobal);
		
		gc.insets = new Insets(5, 5, 5, 5);
		
		gc.gridx = 0; gc.gridy = 0;
		
		gl.setConstraints(localLbl, gc);
		mainPnl.add(localLbl);
		gc.gridx++;
		gl.setConstraints(allLocalCB, gc);
		mainPnl.add(allLocalCB);
		gc.gridx++;
		gl.setConstraints(globalLbl, gc);
		mainPnl.add(globalLbl);
		gc.gridx++;
		gl.setConstraints(allGlobalCB, gc);
		mainPnl.add(allGlobalCB);
		
		gc.gridx = 0; gc.gridy++;
		gc.gridwidth = GridBagConstraints.REMAINDER;
		gl.setConstraints(defaultBtn, gc);
		mainPnl.add(defaultBtn);
		
		// Set the listeners
		defaultBtn.addActionListener(this);
		allLocalCB.addActionListener(this);
		allGlobalCB.addActionListener(this);
		
		// set initial choices
		refreshAllLoggersPanel();
		
		return mainPnl;
	}
	
	/**
	 * This method makes remote calls and should not be called in the event thread!
	 * @throws AcsJCORBAProblemEx In case of ORB / network failure or if remote process is unresponsive or unreachable.
	 */
	private void refreshAllLoggersPanel() throws AcsJCORBAProblemEx {

		try {
			LogLevels defaultLevels = logConf.get_default_logLevels();
			
			int acsLevel = defaultLevels.minLogLevelLocal;
			
			try {
				LogTypeHelper logTypeLocal = LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.fromInteger(acsLevel));
				allLocalCB.setSelectedIndex(logTypeLocal.ordinal());
				model.setCommonLocalLevel(logTypeLocal);
			} catch (AcsJIllegalArgumentEx e) {
				logger.warning("Unexpected log level " + acsLevel + " obtained as default minLogLevelLocal.");
			}
			
			acsLevel = defaultLevels.minLogLevel;
			try {
				LogTypeHelper logTypeGlobal = LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.fromInteger(acsLevel));
				allGlobalCB.setSelectedIndex(logTypeGlobal.ordinal());
				model.setCommonGlobalLevel(logTypeGlobal);
			} catch (AcsJIllegalArgumentEx e) {
				logger.warning("Unexpected log level " + acsLevel + " obtained as default minLogLevel.");
			}
		} catch (SystemException ex) {
			AcsJCORBAProblemEx ex2 = new AcsJCORBAProblemEx(ex);
			ex2.setInfo("Failed to retrieve logger names or levels.");
			throw ex2;
		}
	}

	/**
	 * Setup the panel to show minimum levels
	 * 
	 * @return
	 */
	private JPanel initMimimumLevelsPanel() {
		TitledBorder border = BorderFactory.createTitledBorder("Minimum Log Levels");

		JPanel        mainPnl = new JPanel();
		GridBagLayout      gl = new GridBagLayout();
		GridBagConstraints gc = new GridBagConstraints();
		mainPnl.setLayout(gl);
		mainPnl.setBorder(border);
		
		JLabel localLbl  = new JLabel("Current minimum local level");
		minLocal = new JLabel("");
		JLabel globalLbl = new JLabel("Current minimum remote level");
		minGlobal = new JLabel("");

		gc.insets = new Insets(5, 10, 5, 10);
		
		gc.gridx = 0; gc.gridy = 0;
		
		gl.setConstraints(localLbl, gc);
		mainPnl.add(localLbl);
		gc.gridx++;
		gc.insets.right *= 2;
		gl.setConstraints(minLocal, gc);
		mainPnl.add(minLocal);
		gc.gridx++;
		gc.insets.right /= 2;
		gl.setConstraints(globalLbl, gc);
		mainPnl.add(globalLbl);
		gc.gridx++;
		gl.setConstraints(minGlobal, gc);
		mainPnl.add(minGlobal);

		updateMinLevels();
		
		return mainPnl;
	}

	/**
	 *  Updates the minimum log level panel
	 */
	private void updateMinLevels() {
		LogTypeHelper minLocalLevel = getMinLocalLevel();
		minLocal.setIcon(EntryTypeIcon.getIcon(minLocalLevel));
		minLocal.setText(minLocalLevel.toString());
		minLocal.repaint();

		LogTypeHelper minGlobalLevel = getMinGlobalLevel();
		minGlobal.setIcon(EntryTypeIcon.getIcon(minGlobalLevel));
		minGlobal.setText(minGlobalLevel.toString());
		minLocal.repaint();
	}

}
