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
import java.util.concurrent.ExecutionException;
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
import javax.swing.SwingWorker;
import javax.swing.border.TitledBorder;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import si.ijs.maci.LoggingConfigurableOperations;
import si.ijs.maci.LoggingConfigurablePackage.LogLevels;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.acs.gui.loglevel.LogLvlSelNotSupportedException;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.maciErrType.LoggerDoesNotExistEx;

import com.cosylab.logging.client.EntryTypeIcon;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.LogTypeRenderer;

/**
 * The panel to select the log level of the named loggers
 * 
 * @author acaproni
 * 
 * update history:
 * 2011/07/27 -- bhola.panta @ naoj  
 *  JIRA-COMP-4183 related
 *	1. need to catch system exception, when for example. a container is unresponsive
 *  2. a logger is used to log exceptions  
						
 *
 */
public class LogLevelSelectorPanel extends JPanel implements ActionListener {

	private static final long serialVersionUID = 5291375242590375664L;

	// The Button to apply the changes
	private JButton applyBtn = new JButton("Apply");
	
	// The Button to refresh the list
	private JButton refreshBtn = new JButton("Refresh");
	
	// The LoggingConfigurable
	private LoggingConfigurableOperations logConf=null;
	
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
	
	
	/**
	 * Constructor 
	 * 
	 * @param owner The windo that owns this dialog (it can be null)
	 * @param configurable The LoggingConfigurable whose log level
	 *                     the user wants to read or set
	 * @param title The name of the configurable to add to the tile
	 * @throws LogLvlSelNotSupportedException If the configurable does not support selection
	 */
	//public LogLevelSelectorPanel(LoggingConfigurableOperations configurable, String name) throws LogLvlSelNotSupportedException {
	public LogLevelSelectorPanel(LoggingConfigurableOperations configurable, String name) throws Exception {
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

		ActionListener al = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				applyBtn.setEnabled(true);
			}
		};
		allLocalCB.addActionListener(al);
		allGlobalCB.addActionListener(al);
		
		logConf=configurable;
		setName(name);
		initialize(name);
	}
	/**
	 *  A logger to record exceptions
	 */
	//private static Logger logger = Logger.getLogger(LogLevelSelectorPanel.class.getName());
	private static Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(LogLevelSelectorPanel.class.getName(), true);
	
	/**
	 * Init the GUI
	 * 
	 * @throws LogLvlSelNotSupportedException If the configurable does not support selection
	 */
	//private void initialize(String name) throws LogLvlSelNotSupportedException {
	private void initialize(String name) throws Exception {
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
	//private JComponent initLogLevelsPanel() throws LogLvlSelNotSupportedException {
	private JComponent initLogLevelsPanel() throws Exception {
		LogLevelHelper[] levels=null;
		try {
			levels = loggersLbl();
		//} catch (Exception e) {
		//	throw new LogLvlSelNotSupportedException("Function not yet implemented by "+getName(),e);
		//}
		//added to deal with unresponsive CORBA call
		//2010-02-17 panta@naoj
		} catch (org.omg.CORBA.SystemException cse) {
			throw new Exception(cse.toString());
		
		} catch (LoggerDoesNotExistEx lde) {
			System.err.println("LogLevelSelectorPanel:initLogLevelsPanel LoggerDoesNotExistEx "+lde.toString());
			throw lde;
		} catch (Exception e) {
			System.err.println("LogLevelSelectorPanel:initLogLevelsPanel exception "+e.toString());
			throw e;
		}
		
		model = new LogLevelModel(levels);
		table = new LogLevelTable(model);
		model.addTableModelListener(new TableModelListener(){
			public void tableChanged(TableModelEvent e) {
				applyBtn.setEnabled(userChangedLogLevels());
			}
		});
		
		JScrollPane scrollPane = new JScrollPane(table);
		return scrollPane;
	}
	
	/**
	 * set the labels of the logger names 
	 * 
	 * @return
	 */
	private LogLevelHelper[] loggersLbl() throws Exception {

		// get the logger names
		SwingWorker<String[], Void> worker = new SwingWorker<String[], Void>() {
			protected String[] doInBackground() throws Exception {
				return logConf.get_logger_names();
			}
		};
		worker.execute();

		final String[] logNames = worker.get();
		if (logNames==null) {
			return new LogLevelHelper[0];
		}

		// get the log levels for each logger
		SwingWorker<LogLevels[], Void> worker2 = new SwingWorker<LogLevels[], Void>() {
			protected LogLevels[] doInBackground() throws Exception {
				LogLevels[] levels = new LogLevels[logNames.length];
				for (int i=0; i<logNames.length; i++) {
					levels[i] = logConf.get_logLevels(logNames[i]);
				}
				return levels;
			}
		};
		worker2.execute();
		
		LogLevels[] levels = worker2.get();
		LogLevelHelper[] ret = new LogLevelHelper[logNames.length];
		for (int t=0; t<logNames.length; t++)
			ret[t]= new LogLevelHelper(logNames[t],levels[t]);

		return ret;
	}
	
	/**
	 * 
	 * Apply the changes to the log levels, if any
	 *
	 */
	// JIRA-COMP-4183 related changes made on 2011/07/27
	private void applyChanges() {
		final LogLevelHelper[] newLevels = model.getLevels();
		
		SwingWorker<Void,Void> worker = new SwingWorker<Void, Void>() {
			@Override
			protected Void doInBackground() throws Exception {

				for (LogLevelHelper logLvl: newLevels) {
					if (logLvl.modified()) {
						try {
							System.out.println("Applying new log levels to "+logLvl.getName()+": <"+logLvl.isUsingDefault()+", "+logLvl.getGlobalLevel()+", "+logLvl.getLocalLevel()+">");
							logConf.set_logLevels(logLvl.getName(), logLvl.getLogLevels());
						//} catch (Throwable t) {
						} catch (org.omg.CORBA.SystemException cse) {
							System.err.println("Exception caught while setting log level "+logLvl.getName()+": "+cse.getMessage());
							//t.printStackTrace(System.err);
							logger.info(cse.toString());
							return null; //if system exception, get out of the loop
						}
						catch (Exception e) {
							logger.info(e.toString());
							//e.printStackTrace();
							JOptionPane.showMessageDialog(null,
									" Container failed to receive the log level change or refresh request:\n"+e.toString(), 
									"Error", JOptionPane.ERROR_MESSAGE);
						}
					}
				}
				return null;
			}
		};
		worker.execute();

		try {
			worker.get();
		} catch (/*org.omg.CORBA.SystemException*/ExecutionException cse) {
			logger.info(cse.toString());
			//t.printStackTrace(System.err);
			JOptionPane.showMessageDialog(null,
					" Container failed to receive the log level change or refresh request:\n"+cse.toString(), 
					"Error", JOptionPane.ERROR_MESSAGE);
			return;
			
		} catch(Exception e) {
			logger.info(e.toString());
			//e.printStackTrace(System.err);
			JOptionPane.showMessageDialog(null,
					" Container failed to receive the log level change or refresh request:\n"+e.toString(), 
					"Error", JOptionPane.ERROR_MESSAGE);
			return;
		}

		try {
			int localIndex = allLocalCB.getSelectedIndex();
			LogTypeHelper local = LogTypeHelper.values()[localIndex];
			final int localAcs = local.getAcsCoreLevel().value;

			int globalIndex = allGlobalCB.getSelectedIndex();
			LogTypeHelper global = LogTypeHelper.values()[globalIndex];
			final int globalAcs = global.getAcsCoreLevel().value;

			SwingWorker<Void,Void> worker2 = new SwingWorker<Void, Void>() {
				protected Void doInBackground() throws Exception {
					boolean useDefault = logConf.get_default_logLevels().useDefault;
					LogLevels toset = new LogLevels(useDefault, (short)globalAcs, (short)localAcs);
					logConf.set_default_logLevels(toset);
					return null;
				}
			};
			worker2.execute();
			worker2.get();
			
		} catch (/*org.omg.CORBA.SystemException*/ExecutionException cse) {
			logger.info(cse.toString());
			//t.printStackTrace(System.err);
			JOptionPane.showMessageDialog(null,
					" Container failed to receive the log level change or refresh request:\n"+cse.getClass(), 
					"Error", JOptionPane.ERROR_MESSAGE);
			return;
			
		} catch (Exception e) {
			logger.info(e.toString());
			JOptionPane.showMessageDialog(null,
					" Container failed to receive the log level change or refresh request:\n"+e.toString(), 
					"Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		model.changesApplied();
		updateMinLevels();

		applyBtn.setEnabled(false);
	}
	
	/**
	 * Refresh the list, to see changes made by other operators
	 */
	public void refresh() {
    	if (userChangedLogLevels()) {
			if (JOptionPane.showConfirmDialog(
					null, 
					"Do you really want to discard changes?", 
					"Confirm", 
					JOptionPane.YES_NO_OPTION)==JOptionPane.NO_OPTION) {
				return;
			}
		}
		
		refreshAllLoggersPanel();
		applyChanges();

		LogLevelHelper[] levels=null;
		try {
			levels = loggersLbl();
		} 
		 catch (ExecutionException e) { 
			//e.printStackTrace();
			 logger.info(e.toString());
			return;
		} catch (Exception e) {
			System.err.println("Function not yet implemented by "+getName());
			logger.info(e.toString());
			//e.printStackTrace(System.err);
			return;
		}

		model.setLevels(levels);
		model.fireTableDataChanged();
		
		applyBtn.setEnabled(false);
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
	 */
	private JPanel initAllLoggersPanel() {
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
	
	private void refreshAllLoggersPanel() {

		LogLevels defaultLevels = null;

		SwingWorker<LogLevels,Void> worker = new SwingWorker<LogLevels, Void>() {
			protected LogLevels doInBackground() throws Exception {
				return logConf.get_default_logLevels();
			}
		};
		worker.execute();

		try {
			defaultLevels = worker.get();
		} catch (/*org.omg.CORBA.SystemException cse*/ExecutionException ee) {
			System.out.println("refreshAllLoggersPanel.ExecutionException");
			//e1.printStackTrace();
			return;
		}
		catch (Exception e1) {
			System.out.println("refreshAllLoggersPanel.Exception");
			//e1.printStackTrace();
		}
		
		int acsLevel = defaultLevels.minLogLevelLocal;
		try {
			LogTypeHelper logTypeLocal  = LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.fromInteger(acsLevel));
			allLocalCB.setSelectedIndex(logTypeLocal.ordinal());
			model.setCommonLocalLevel(logTypeLocal);
		} catch (AcsJIllegalArgumentEx e) {
			System.out.println("Unexpected log level : " + acsLevel);
			e.printStackTrace(System.err);
		}
		
		acsLevel = defaultLevels.minLogLevel;
		try {
			LogTypeHelper logTypeGlobal = LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.fromInteger(acsLevel));
			allGlobalCB.setSelectedIndex(logTypeGlobal.ordinal());
			model.setCommonGlobalLevel(logTypeGlobal);
		} catch (AcsJIllegalArgumentEx e) {
			System.out.println("Unexpected log level : " + acsLevel);
			e.printStackTrace(System.err);
		}
	}

	/**
	 * Setup the panel to show mimimum levels
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
