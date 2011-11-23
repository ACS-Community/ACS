/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
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
package com.cosylab.logging.stats;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Point;
import java.util.Calendar;
import java.util.Timer;
import java.util.TimerTask;

import com.cosylab.logging.LoggingClient;

/**
 * Shows statistics from loaded logs
 * 
 * @author acaproni
 *
 */
public class StatsDlg extends JDialog implements ActionListener {
	
	/**
	 * The string shown when it is not possible to get a value
	 * from the system
	 */
	private static final String NotAvailable= "N/A";
	
	/**
	 * The interval (msec) between iterations while monitoring
	 */
	private final int MONITORING_INTERVAL = 1000;
	
	private JLabel totNumOfLogsLbl = new JLabel(NotAvailable);
	private JLabel visibleLogsLbl  = new JLabel(NotAvailable);
	private JLabel hiddenLogsLbl = new JLabel(NotAvailable);
	private JLabel availMemLbl = new JLabel(NotAvailable);
	private JLabel usedMemLbl = new JLabel(NotAvailable);
	private JLabel timeFrameLbl = new JLabel(NotAvailable);
	private JLabel inRateLbl = new JLabel(NotAvailable);
	private JLabel outRateLbl = new JLabel(NotAvailable);
	
	/**
	 * The number of the cache files (it refers to the cache of jlog
	 * and not to the cache of the engine)
	 */
	private JLabel numOfCacheFiles  = new JLabel(NotAvailable);
	
	/**
	 * The disk space used by all the files of the cache 
	 * (it refers to the cache of jlog and not to the cache of the engine)
	 */
	private JLabel diskSpace = new JLabel(NotAvailable);
	
	private JButton closeBtn = new JButton("Close");
	private JButton refreshBtn = new JButton("Refresh");
	
	private JButton monitorBtn = new JButton("Start monitoring");
	
	// A reference to the LoggingClient
	private LoggingClient logging;
	
	/**
	 * The timer for monitoring
	 */
	private Timer timer=null;
	
	/** 
	 * Builds and show the dialog
	 * 
	 * @param logCache
	 */
	public StatsDlg(LoggingClient mainWin) {
		super();
		if (mainWin==null) {
			throw new IllegalArgumentException("The LoggingClient can't be null");
		}
		logging =mainWin;
		setDefaultCloseOperation(HIDE_ON_CLOSE);
		
		initialize();
        pack();
	}
	
	/**
	 * Setup the GUI
	 */
	private void initialize() {
		this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setTitle("Statistics");
		this.setBounds(50, 35, 100, 100);
		JPanel mainPnl = new JPanel(new BorderLayout());
		
		JPanel valuesPnl = new JPanel(new GridLayout(10,1));
		
		// Add the num of logs
		JPanel numOfLogsPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		numOfLogsPnl.add(new JLabel("Total logs: "));
		numOfLogsPnl.add(totNumOfLogsLbl);
		valuesPnl.add(numOfLogsPnl);
		
		// Visible logs
		JPanel visibleLogsPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		visibleLogsPnl.add(new JLabel("Visible logs: "));
		visibleLogsPnl.add(visibleLogsLbl);
		valuesPnl.add(visibleLogsPnl);
		
		// Hidden logs
		JPanel hiddenLogsPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		hiddenLogsPnl.add(new JLabel("Hidden logs: "));
		hiddenLogsPnl.add(hiddenLogsLbl);
		valuesPnl.add(hiddenLogsPnl);
		
		// Add the available memory
		JPanel availMemPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		availMemPnl.add(new JLabel("Memory available: "));
		availMemPnl.add(availMemLbl);
		valuesPnl.add(availMemPnl);
		
		// Add the used memory
		JPanel usedMemPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		usedMemPnl.add(new JLabel("Used memory: "));
		usedMemPnl.add(usedMemLbl);
		valuesPnl.add(usedMemPnl);
		
		// Add the number of the files of cache
		JPanel usedCacheFiles = new JPanel(new FlowLayout(FlowLayout.LEFT));
		usedCacheFiles.add(new JLabel("Number of files of cache: "));
		usedCacheFiles.add(numOfCacheFiles);
		valuesPnl.add(usedCacheFiles);
		
		// Add the disk space used by the files of the cache
		JPanel usedDiskSpace = new JPanel(new FlowLayout(FlowLayout.LEFT));
		usedDiskSpace.add(new JLabel("Disk space used by the cache: "));
		usedDiskSpace.add(diskSpace);
		valuesPnl.add(usedDiskSpace);
		
		// Add the time frame
		JPanel timeFramePnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		timeFramePnl.add(new JLabel("Time frame: "));
		timeFramePnl.add(timeFrameLbl);
		valuesPnl.add(timeFramePnl);
		
		// Add the rate from the NC
		JPanel inRatePnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		inRatePnl.add(new JLabel("Rate of logs from NC: "));
		inRatePnl.add(inRateLbl);
		valuesPnl.add(inRatePnl);
		
		// Add the rate to the listeners
		JPanel outRatePnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		outRatePnl.add(new JLabel("Rate of logs sent to table: "));
		outRatePnl.add(outRateLbl);
		valuesPnl.add(outRatePnl);
		
		mainPnl.add(valuesPnl,BorderLayout.CENTER);
		
		// Add the refresh and the close buttons
		JPanel buttonPanel = new JPanel(new FlowLayout());
		monitorBtn.addActionListener(this);
		buttonPanel.add(monitorBtn);
		refreshBtn.addActionListener(this);
		buttonPanel.add(refreshBtn);
		closeBtn.addActionListener(this);
		buttonPanel.add(closeBtn);
		mainPnl.add(buttonPanel,BorderLayout.SOUTH);
		
		setContentPane(mainPnl);
		
		addWindowListener(new WindowAdapter(){
			public void windowClosing(WindowEvent we) {
				enableMonitoring(false);
			}
		});
	}
	/**
	 * Override <code>setVisible()</code> to move the statistic window
	 * over the logging client and in front of other windows
	 */
	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		// Move the statistic win on top of jlog
		if (visible && isShowing()) {
			Point loggingPos = logging.getLocationOnScreen();
			setLocation(loggingPos);
			toFront();
			refreshGUI();
		}
		if (!visible) {
			enableMonitoring(false);
		}
	}
	
	/**
	 * Refresh the values shown in the GUI
	 *
	 */
	private void refreshGUI() {
		/**
		 * A class to refresh the labels of the dialog
		 * The thread is executed inside the swing thread
		 * 
		 * @author acaproni
		 *
		 */
		class GuiRefresher implements Runnable {
			long totLogs;
			long visLogs;
			long hidLogs;
			long availMem;
			long totMem;
			String diskSize;
			int totCacheFiles;
			String timeFrameStr;
			String inputRateStr;
			String outputRateStr;
			public void run() {
				totNumOfLogsLbl.setText(Long.valueOf(totLogs).toString());
				visibleLogsLbl.setText(Long.valueOf(visLogs).toString());
				hiddenLogsLbl.setText(Long.valueOf(hidLogs).toString());
				availMemLbl.setText(""+(availMem/1024)+"Kb");
		        usedMemLbl.setText(""+((totMem-availMem)/1024)+"Kb");
		        diskSpace.setText(diskSize);
		        numOfCacheFiles.setText(""+totCacheFiles);
		        timeFrameLbl.setText(timeFrameStr);
		        inRateLbl.setText(inputRateStr);
		        outRateLbl.setText(outputRateStr);
		        pack();
			}
		}
		
		GuiRefresher refresher = new GuiRefresher();
		refresher.totLogs = logging.getLogEntryTable().getLCModel().totalLogNumber();
		refresher.visLogs = logging.getLogEntryTable().getViewRowCount();
		refresher.hidLogs = refresher.totLogs-refresher.visLogs;
		try {
			refresher.diskSize= ""+logging.getLogEntryTable().getLCModel().usedDiskSpace();	
		} catch (Throwable t) {
			System.err.println("Error getting the used disk space: "+t.getMessage());
			t.printStackTrace(System.err);
			refresher.diskSize=NotAvailable;
		}
		refresher.totCacheFiles=logging.getLogEntryTable().getLCModel().numberOfUsedFiles();
		
		Runtime rt = Runtime.getRuntime();
		refresher.availMem = rt.freeMemory();
		refresher.totMem = rt.totalMemory();
		Calendar timeFrame = logging.getLogEntryTable().getLCModel().getTimeFrame();
        StringBuilder str = new StringBuilder();
        str.append(timeFrame.get(Calendar.DAY_OF_YEAR)-1);
        str.append("days - ");
        str.append(timeFrame.get(Calendar.HOUR_OF_DAY));
        str.append(":");
        str.append(timeFrame.get(Calendar.MINUTE));
        str.append(":");
        str.append(timeFrame.get(Calendar.SECOND));
        str.append(".");
        str.append(timeFrame.get(Calendar.MILLISECOND));
        refresher.timeFrameStr=str.toString();
        
        StringBuilder in = new StringBuilder();
        in.append(logging.getEngine().getActualInputRate());
        in.append(" (");
        if (logging.getEngine().getMaxInputRate()==Integer.MAX_VALUE) {
        	in.append("unlimited)");
        } else {
        	in.append("limted to ");
        	in.append(logging.getEngine().getMaxInputRate());
        	in.append(')');
        }
        refresher.inputRateStr=in.toString();
        
        StringBuilder out = new StringBuilder();
        out.append(logging.getEngine().getActualOutputRate());
        out.append(" (");
        if (logging.getEngine().getMaxOutputRate()==Integer.MAX_VALUE) {
        	out.append("unlimited)");
        } else {
        	out.append("limted to ");
        	out.append(logging.getEngine().getMaxOutputRate());
        	out.append(')');
        }
        refresher.outputRateStr=out.toString();
        
        // Start the swing thread
        SwingUtilities.invokeLater(refresher);
	}
	
	/**
	 * @see <code>ActionListener<?code>
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==closeBtn) {
			setVisible(false);
		} if (e.getSource()==refreshBtn) {
			Thread t = new Thread("Stats dialog updater") {
				public void run() {
					refreshGUI();
				}
			};
			t.setDaemon(true);
			t.start();
		} if (e.getSource()==monitorBtn) {
				enableMonitoring(refreshBtn.isEnabled());
		}
	}
	
	/**
	 * Start or stop the monitoring
	 * 
	 * @param start If <code>true</code> start monitoring
	 */
	private void enableMonitoring(boolean start) {
		if (start) {
			if (timer!=null) {
				throw new IllegalStateException("Timer is already instantiated");
			}
			timer = new Timer("Statistics monitoring");
			TimerTask task = new TimerTask() {
				public void run() {
					refreshGUI();
				}
			};
			timer.schedule(task, 50, MONITORING_INTERVAL);
			refreshBtn.setEnabled(false);
			monitorBtn.setText("Stop monitoring");
		} else {
			if (timer!=null) {
				timer.cancel();
			}
			timer=null;
			refreshBtn.setEnabled(true);
			monitorBtn.setText("Start monitoring");
		}
	}
	
}
