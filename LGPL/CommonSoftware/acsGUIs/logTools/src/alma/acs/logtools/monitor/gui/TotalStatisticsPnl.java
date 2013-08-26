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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import com.cosylab.logging.engine.log.LogTypeHelper;

import alma.acs.logtools.monitor.TotalStatsData;
import alma.acs.util.IsoDateFormat;

/**
 * Shows the statistics of the total number of logs and so on
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public class TotalStatisticsPnl extends JPanel {
	
	/**
	 * Extends the {@link TotalStatsData} to update the GUI 
	 * whenever new values arrived.
	 *  
	 * @author acaproni 
	 *
	 */
	private class MyStatsData extends TotalStatsData {

		/*
		 * @see alma.acs.logtools.monitor.TotalStatsData#updateErrors(int)
		 */
		@Override
		public synchronized void updateErrors(int errs) {
			if (errs!=errors) {
				super.updateErrors(errs);
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						totParsingErrsLbl.setText(""+errors);
					}
				});
			}
		}

		/**
		 * @see alma.acs.logtools.monitor.TotalStatsData#updateSizes(int, int)
		 */
		@Override
		public synchronized void updateSizes(int min, int max) {
			if (min!=shortestLogSize || max!=longestLogSize) {
				super.updateSizes(min, max);
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						shortestLogLbl.setText(""+shortestLogSize);
						longestLogLbl.setText(""+longestLogSize);
					}
				});
			}
		}

		/**
		 * @see alma.acs.logtools.monitor.TotalStatsData#updateTotalLogs(long[])
		 */
		@Override
		public synchronized void updateTotalLogs(long[] nums) {
			boolean changed=false;
			for (int t=0; t<nums.length && !changed; t++) {
				changed=nums[t]!=totalLogs[t];
			}
			if (changed) {
				super.updateTotalLogs(nums);
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						totNumOfLogsLbl.setText(""+numOfLogs);
						for (int t=0; t<totalLogs.length; t++) {
							totLogTypes[t].setText(
									String.format("%d (%.1f%%)", totalLogs[t],logsTypeDistribution[t]*100));						}
					}
				});
			}
		}
	}

	/**
	 * The data to display in the panel
	 */
	private final MyStatsData data = new MyStatsData();
	
	/**
	 * The time stamp of the last update
	 */
	private final JLabel timestampLbl = new JLabel("Not updated yet!");
	
	/**
	 * The total number of logs
	 */
	private final JLabel totNumOfLogsLbl = new JLabel("0");
	
	/**
	 * The total number of errors parsing XML logs
	 */
	private final JLabel totParsingErrsLbl = new JLabel("0");
	
	/**
	 * The size of the shortest log
	 */
	private final JLabel shortestLogLbl = new JLabel("0");
	
	/**
	 * The size of the longest log
	 */
	private final JLabel longestLogLbl = new JLabel("0");
	
	/**
	 * The labels for the value of each log type
	 */
	private final JLabel[] totLogTypes= new JLabel[LogTypeHelper.values().length]; 
	
	/**
	 * Constructor
	 */
	public TotalStatisticsPnl() {
		initialize();
	}
	
	/**
	 * Init the GUI
	 */
	private void initialize() {
		JPanel timestampPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		timestampPnl.add(new JLabel("Data last updated at"));
		timestampPnl.add(timestampLbl);
		setLayout(new BorderLayout());
		add(timestampPnl,BorderLayout.NORTH);
		
		// The logs
		JPanel logsPnl = new JPanel();
		logsPnl.setBorder(BorderFactory.createTitledBorder("Total num. of logs"));
		add(logsPnl,BorderLayout.CENTER);
		Insets insets1=new Insets(2, 2, 2, 5);
		GridBagLayout gb = new GridBagLayout();
        GridBagConstraints constr = new GridBagConstraints();
        logsPnl.setLayout(gb);
        constr.insets=insets1;
        constr.fill=GridBagConstraints.NONE;
        constr.anchor=GridBagConstraints.LINE_START;
        constr.weightx=0.5;
		for (int t=0; t<LogTypeHelper.values().length; t++) {
			LogTypeHelper logType=LogTypeHelper.values()[t];
			constr.gridx=0; constr.gridy=t;
			JLabel lbl = new JLabel(logType.toString());
			gb.setConstraints(lbl, constr);
			logsPnl.add(lbl);
			
			constr.gridx=1; constr.gridy=t;
			totLogTypes[t]=new JLabel("0");
			gb.setConstraints(totLogTypes[t], constr);
			logsPnl.add(totLogTypes[t]);
		}
		add(logsPnl,BorderLayout.CENTER);
		
		// The numbers
		JPanel globalPnl = new JPanel();
		Insets insets=new Insets(2, 2, 2, 5);
		GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.insets=insets;
        c.fill=GridBagConstraints.NONE;
        c.anchor=GridBagConstraints.LINE_START;
        c.weightx=0.5;
        globalPnl.setLayout(gridbag);
		globalPnl.setBorder(BorderFactory.createTitledBorder("Global"));
		
		c.gridx=0; c.gridy=0;
		JLabel totLbl = new JLabel("Total logs:");
		gridbag.setConstraints(totLbl, c);
		globalPnl.add(totLbl);
		
		c.gridx=1; c.gridy=0;
		gridbag.setConstraints(totNumOfLogsLbl, c);
		globalPnl.add(totNumOfLogsLbl);
		
		c.gridx=0; c.gridy=1;
		JLabel errorLbl = new JLabel("Errors parsing XML:");
		gridbag.setConstraints(errorLbl, c);
		globalPnl.add(errorLbl);
		
		c.gridx=1; c.gridy=1;
		gridbag.setConstraints(totParsingErrsLbl, c);
		globalPnl.add(totParsingErrsLbl);
		
		c.gridx=0; c.gridy=2;
		JLabel shortLbl = new JLabel("Length of shortest XML log");
		gridbag.setConstraints(shortLbl, c);
		globalPnl.add(shortLbl);
		
		c.gridx=1; c.gridy=2;
		gridbag.setConstraints(shortestLogLbl, c);
		globalPnl.add(shortestLogLbl);
		
		c.gridx=0; c.gridy=3;
		JLabel longLbl = new JLabel("Length of longest XML log");
		gridbag.setConstraints(longLbl, c);
		globalPnl.add(longLbl);
		
		c.gridx=1; c.gridy=3;
		gridbag.setConstraints(longestLogLbl, c);
		globalPnl.add(longestLogLbl);
		
		add(globalPnl,BorderLayout.SOUTH);
	}
	
	/**
	 * Update the values displayed in the panel
	 * 
	 * @param newData New statistics
	 */
	public void updateStats(TotalStatsData newData) {
		data.updateAll(newData);
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				timestampLbl.setText(IsoDateFormat.formatCurrentDate());
			}
		});
	}
}
