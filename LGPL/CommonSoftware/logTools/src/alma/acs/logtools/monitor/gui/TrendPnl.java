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
import java.awt.Color;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import javax.swing.BorderFactory;
import javax.swing.JPanel;

import alma.acs.logtools.monitor.LogDetailsDispatcher;

import com.cosylab.logging.engine.log.LogTypeHelper;

public class TrendPnl extends JPanel {
	
	/**
	 * The number of points per trace.
	 */
	private static final int pointsPerTrace=3000; // 5h
	
	/**
	 * The chart displayed in the panel
	 */
	private final Chart2D chart = new Chart2D();
	
	/**
	 * The trace to display in the chart (one for each log type)
	 */
	private final ITrace2D[] logTraces = new ITrace2D[LogTypeHelper.values().length];
	
	/**
	 * The X-coordinate to add the next point
	 */
	private double x=0;
	
	/**
	 * Constructor
	 */
	public TrendPnl() {
		initialize();
	}
	
	/**
	 * Destroy the chart and free the resources
	 */
	public void close() {
		chart.destroy();
	}
	
	/**
	 * Add the new logs number to the traces of the chart
	 * 
	 * @param nums Number of logs per type
	 * @param secs Interval in seconds
	 */
	public void addTrace(int[] nums, int secs) {
		for (int t=0; t<nums.length; t++) {
			logTraces[t].addPoint(x, nums[t]);
		}
		// Set colors
		logTraces[LogTypeHelper.TRACE.ordinal()].setColor(Color.lightGray);
		logTraces[LogTypeHelper.DEBUG.ordinal()].setColor(Color.pink);
		logTraces[LogTypeHelper.INFO.ordinal()].setColor(Color.blue);
		logTraces[LogTypeHelper.NOTICE.ordinal()].setColor(Color.black);
		logTraces[LogTypeHelper.WARNING.ordinal()].setColor(Color.orange);
		logTraces[LogTypeHelper.ERROR.ordinal()].setColor(Color.red);
		logTraces[LogTypeHelper.CRITICAL.ordinal()].setColor(Color.magenta);
		logTraces[LogTypeHelper.ALERT.ordinal()].setColor(Color.yellow);
		logTraces[LogTypeHelper.EMERGENCY.ordinal()].setColor(Color.cyan);
		x+=LogDetailsDispatcher.NUMBER_LISTENERS_INTERVAL;
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initialize() {
		setLayout(new BorderLayout());
		
		JPanel upperPnl = new JPanel(new BorderLayout());
		upperPnl.setBorder(BorderFactory.createTitledBorder("Number of logs in "+LogDetailsDispatcher.NUMBER_LISTENERS_INTERVAL+" secs"));
		upperPnl.add(chart,BorderLayout.CENTER);
		add(upperPnl,BorderLayout.CENTER);
		
		IAxis.AxisTitle xTitle = new IAxis.AxisTitle("secs");
		IAxis.AxisTitle yTitle = new IAxis.AxisTitle("num. of logs");
		chart.getAxisX().setAxisTitle(xTitle);
		chart.getAxisY().setAxisTitle(yTitle);
		// build (and add to the charts) the ITrace2D
		for (int i=0; i<logTraces.length; i++) {
			logTraces[i]=new Trace2DLtd(pointsPerTrace,LogTypeHelper.values()[i].toString());
			chart.addTrace(logTraces[i]);
		}
	}
}
