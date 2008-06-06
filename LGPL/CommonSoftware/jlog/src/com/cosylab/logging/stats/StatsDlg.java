package com.cosylab.logging.stats;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Point;
import java.util.Calendar;

import com.cosylab.logging.LoggingClient;

/**
 * Shows statistics from loaded logs
 * 
 * @author acaproni
 *
 */
public class StatsDlg extends JDialog 
	implements ActionListener {
	
	private JLabel totNumOfLogsLbl = new JLabel("N/A");
	private JLabel visibleLogsLbl  = new JLabel("N/A");
	private JLabel hiddenLogsLbl = new JLabel("N/A");
	private JLabel availMemLbl = new JLabel("N/A");
	private JLabel usedMemLbl = new JLabel("N/A");
	private JLabel timeFrameLbl = new JLabel("N/A");
	
	private JButton closeBtn = new JButton("Close");
	private JButton refreshBtn = new JButton("Refresh");
	
	// A reference to the LoggingClient
	private LoggingClient logging;;
	
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
		
		JPanel valuesPnl = new JPanel(new GridLayout(6,1));
		
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
		
		// Add the time frame
		JPanel timeFramePnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		timeFramePnl.add(new JLabel("Time frame: "));
		timeFramePnl.add(timeFrameLbl);
		valuesPnl.add(timeFramePnl);
		
		mainPnl.add(valuesPnl,BorderLayout.CENTER);
		
		// Add the refresh and the close buttons
		JPanel buttonPanel = new JPanel(new FlowLayout());
		refreshBtn.addActionListener(this);
		buttonPanel.add(refreshBtn);
		closeBtn.addActionListener(this);
		buttonPanel.add(closeBtn);
		mainPnl.add(buttonPanel,BorderLayout.SOUTH);
		
		setContentPane(mainPnl);
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
			String timeFrameStr;
			public void run() {
				totNumOfLogsLbl.setText(Long.valueOf(totLogs).toString());
				visibleLogsLbl.setText(Long.valueOf(visLogs).toString());
				hiddenLogsLbl.setText(Long.valueOf(hidLogs).toString());
				availMemLbl.setText(""+(availMem/1024)+"Kb");
		        usedMemLbl.setText(""+((totMem-availMem)/1024)+"Kb");
		        timeFrameLbl.setText(timeFrameStr);
		        pack();
			}
		}
		
		/**
		 * The thread reading values from the table of logs.
		 * 
		 * This operation is too slow to be executed inside the swing thread.
		 * At the end it executes a swing thread to update the content of the 
		 * labels of the GUI.
		 */
		Runnable updater = new Runnable() {
			public void run() {
				GuiRefresher refresher = new GuiRefresher();
				refresher.totLogs = logging.getLogEntryTable().getLCModel().totalLogNumber();
				refresher.visLogs = logging.getLogEntryTable().getViewRowCount();
				refresher.hidLogs = refresher.totLogs-refresher.visLogs;
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
		        
		        // Start the swing thread
		        SwingUtilities.invokeLater(refresher);
			}
		};
		
		Thread t = new Thread(updater,"Stats dialog updater");
		t.setDaemon(true);
		t.start();
	}
	
	/**
	 * @see <code>ActionListener<?code>
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==closeBtn) {
			setVisible(false);
		}if (e.getSource()==refreshBtn) {
			refreshGUI();
		}
	}
	
}
