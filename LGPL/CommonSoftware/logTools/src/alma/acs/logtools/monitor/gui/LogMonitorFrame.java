/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.logtools.monitor.gui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

import alma.acs.logtools.monitor.LogMonitor;
import alma.acs.logtools.monitor.LogNumbersListener;
import alma.acs.logtools.monitor.TotalStatsData;

/**
 * The main window.
 * <P>
 * The frame listens for number for displaying the statistics,
 * The numbers are sent to the panels that shows the statistics.
 *  
 * @author acaproni
 * @since ACS 8.1.0
 *
 */
public class LogMonitorFrame extends JFrame 
implements ActionListener, LogNumbersListener {
	
	/**
	 * The object to handle the window closing event.
	 * 
	 * @author acaproni
	 *
	 */
	private class MyWindowAdapter extends WindowAdapter {

		/**
		 * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)
		 */
		@Override
		public void windowClosing(WindowEvent e) {
			logMonitor.close();
		}
	}

	/**
	 * The log monitor that owns this frame
	 */
	private final LogMonitor logMonitor;
	
	/**
	 * The panel showing the tabs
	 */
	private JTabbedPane tabbedPnl;
	
	/**
	 * The panel shown in the tab of the total number of logs
	 */
	private TotalStatisticsPnl totalStatsPnl;
	
	/**
	 * The panel with the graph of the flow of logs
	 */
	private TrendPnl trendPnl;
	
	private JButton closeBtn = new JButton("Close");
	
	public LogMonitorFrame(LogMonitor logMonitor) {
		if (logMonitor==null) {
			throw new IllegalArgumentException("Invalid null log monitor");
		}
		this.logMonitor=logMonitor;
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new MyWindowAdapter());
		initialize();
	}
	
	/**
	 * Init the GUI
	 */
	private void initialize() {
		setTitle("ACS logs monitor");
		JRootPane rootP=getRootPane();
		
		rootP.setLayout(new BorderLayout());
		// Add the tabs
		tabbedPnl = new JTabbedPane();
		totalStatsPnl = new TotalStatisticsPnl();
		tabbedPnl.add("Values", totalStatsPnl);
		trendPnl=new TrendPnl();
		tabbedPnl.add("Trend",trendPnl);
		rootP.add(tabbedPnl,BorderLayout.CENTER);
		
		// Add the close button at the bottom
		JPanel btnPnl = new JPanel(new FlowLayout(FlowLayout.CENTER));
		btnPnl.add(closeBtn);
		closeBtn.addActionListener(this);
		rootP.add(btnPnl,BorderLayout.SOUTH);
		
		pack();
		setVisible(true);
	}
	
	/**
	 * close the GUI and free the resources
	 */
	public void close() {
		setVisible(false);
		trendPnl.close();
		dispose();
	}

	/**
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==closeBtn) {
			closeBtn.setEnabled(false);
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					logMonitor.close();
				}
			});
		} else {
			System.err.print("Unrecognize source of events:"+e.getSource());
		}
		
	}

	/**
	 * @see alma.acs.logtools.monitor.LogNumbersListener#recvLogs(int[], int)
	 */
	@Override
	public void recvLogs(int[] nums, int secs) {
		trendPnl.addTrace(nums, secs);
	}

	/**
	 * @see alma.acs.logtools.monitor.LogNumbersListener#totalData(alma.acs.logtools.monitor.TotalStatsData)
	 */
	@Override
	public void totalData(TotalStatsData d) {
		totalStatsPnl.updateStats(d);
		
	}
}
