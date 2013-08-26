/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
package cl.utfsm.samplingSystemUI;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.IAxisLabelFormatter;
import info.monitorenter.gui.chart.TracePoint2D;
import info.monitorenter.gui.chart.ZoomableChart;
import info.monitorenter.gui.chart.axis.AAxis;
import info.monitorenter.gui.chart.axis.AxisLinear;
import info.monitorenter.gui.chart.controls.LayoutFactory;
import info.monitorenter.gui.chart.labelformatters.LabelFormatterDate;
import info.monitorenter.gui.chart.traces.Trace2DLtd;
import info.monitorenter.gui.chart.views.ChartPanel;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.text.SimpleDateFormat;
import java.util.ArrayList;

import alma.acs.util.UTCUtility;

public class PlotWidget extends SamplingWidget {

	// Data Handling Attributes
	private long samples=0;
	private int position;
	private ArrayList<Trace2DLtd> traces;
	private float frequency = (float) 1.0;  // Hertz
	private int timewindow = 1; // seconds
	
	// UI Attributes
	private static final long serialVersionUID = 4823621192367385664L;
	private Chart2D chart;
	private Color[] colors = {Color.RED, Color.BLUE, Color.GREEN, Color.MAGENTA, Color.GRAY, Color.ORANGE, Color.BLACK, Color.CYAN, Color.DARK_GRAY, Color.PINK};
	private int currentColor = 0;

	public PlotWidget() {
		super();
		initialize();
	}
	
	public PlotWidget(int position) {
		super();
		initialize();
		this.position = position;
	}
	/**
	 * This method initializes this
	 * 
	 */
	private void initialize(){
		
		if( position == 0){
			LayoutFactory lfct = LayoutFactory.getInstance();
			lfct.setShowTraceNameMenu(false);
			lfct.setShowPhysicalUnitsMenu(false);
			chart = new ZoomableChart();
			
			// Changing X Axis for time presentation
			IAxis xAxis = new AxisLinear();
			IAxis yAxis = new AxisLinear();
			
			chart.setAxisXBottom((AAxis)xAxis,0);
			chart.setAxisYLeft((AAxis)yAxis,0);
			chart.setGridColor(Color.BLACK);
           
			xAxis.setFormatter( (IAxisLabelFormatter) new LabelFormatterDate(new SimpleDateFormat("H:mm:ss")));
			xAxis.getAxisTitle().setTitle("Time [seconds]");
			yAxis.getAxisTitle().setTitle("Property Value");
            
			traces = new ArrayList<Trace2DLtd>();
			chart.setSize(800,600);
			
			// add the chart to the panel
			this.setLayout(new GridBagLayout());
			GridBagConstraints gbc = new GridBagConstraints();
			gbc.anchor = GridBagConstraints.CENTER;
			gbc.ipadx = 10;
			gbc.fill = GridBagConstraints.BOTH;
			gbc.weightx = 1;
			gbc.weighty = 1;
			
        		ChartPanel cp = new ChartPanel(chart);
			this.add(cp,gbc);
			
			
			// create new trace for the data.
			Trace2DLtd tempTrace = new Trace2DLtd(timewindow*(int)frequency); //now are seconds no minutes
			tempTrace.setColor(colors[currentColor]);
			currentColor++;
			if( currentColor >= colors.length ){
				currentColor = 0;
			}
			//trace.setPhysicalUnits("Ticks", "Voltage");
			chart.addTrace(tempTrace);
			traces.add(tempTrace);
		}

	}
	
	public void initializeNewPosition(int position){
		if( position != 0 ){
			Trace2DLtd tempTrace = new Trace2DLtd(timewindow*(int)frequency);//now are seconds no minutes
			tempTrace.setColor(colors[currentColor]);
			currentColor++;
			if( currentColor >= colors.length ){
				currentColor = 0;
			}
			//trace.setPhysicalUnits("Ticks", "Voltage");
			chart.addTrace(tempTrace);
			traces.add(tempTrace);
		}
	}

	public void updateValues(long time, double value, int position) {
		samples++;
		traces.get(position).addPoint(new TracePoint2D(UTCUtility.utcOmgToJava(time), value));
		//dataArray.get(position).add( new Millisecond( new Date(UTCUtility.utcOmgToJava(time) ) ), value);
	}

	public void setValues(String component, String property, int position) {
		traces.get(position).setName(component + "#" + property);
	}

	public void setComponentAvailable(boolean tmp, String reason, int position) {
	}
	
	public void resetSampleCount() {
		for (Trace2DLtd trace : traces){
			trace.removeAllPoints();
		}
	}
	
	public void removeTrace(String component, String property) {
		int index=0;
		for (Trace2DLtd trace:traces) {
			if(trace.getName().equals(component + "#" + property)) {
				chart.removeTrace(trace);
				//chart.setPaintLabels(false);
				traces.remove(index);
				break;
			}
			index++;
		}
	}

	public void setTimeWindow(double frequency, int time) {
		// TODO Auto-generated method stub
		for (Trace2DLtd trace : traces){
			trace.setMaxSize((int)(frequency*time) + 1);//now are seconds no minutes
		}
	}
}
