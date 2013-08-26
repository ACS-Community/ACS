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
package alma.acs.alarmsanalyzer.view;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.swtchart.Chart;
import org.swtchart.IAxis;
import org.swtchart.IAxisSet;
import org.swtchart.IAxisTick;
import org.swtchart.ILineSeries;
import org.swtchart.ISeriesSet;
import org.swtchart.ITitle;
import org.swtchart.ILineSeries.PlotSymbolType;
import org.swtchart.ISeries.SeriesType;

import alma.acs.alarmsanalyzer.document.TenMinutesContainer;


public class TenMinutesView extends ChartViewBase {
	
	/**
	 * The chart
	 */
	private Chart chart;
	
	/**
	 * The serie showing the number of alarms
	 */
	private ILineSeries nAlarmsSerie;
	
	/**
	 * The series used to show the error.
	 */
	private ILineSeries errorSerie;
	
	/**
	 * The threshold
	 */
	private static final double errorThreshold=10;
	
	/**
	 * The Color of the threshold
	 */
	private Color errorColor;
	
	/**
	 * Constructor
	 */
	public TenMinutesView() {}

	@Override
	public void createPartControl(Composite parent) {
		chart= new Chart(parent, SWT.NONE);
		ITitle chartTitle=chart.getTitle();
		chartTitle.setText("Alarms per 10 minutes");
		IAxisSet axisSet=chart.getAxisSet();
		IAxis[] xAxis=axisSet.getXAxes();
		ITitle xTitle=xAxis[0].getTitle();
		xTitle.setText("Time");
		IAxis[] yAxis=axisSet.getYAxes();
		ITitle yTitle=yAxis[0].getTitle();
		yTitle.setText("# alarms");
		ISeriesSet seriesSet = chart.getSeriesSet();
		nAlarmsSerie = (ILineSeries) seriesSet.createSeries(SeriesType.LINE,"nAlarms");
		nAlarmsSerie.enableStep(true);
		nAlarmsSerie.setSymbolType(PlotSymbolType.NONE);
		IAxisTick xTick=axisSet.getXAxis(0).getTick();
		DateFormat format = new SimpleDateFormat("HH:mm");
		xTick.setFormat(format);
		
		errorSerie= (ILineSeries) seriesSet.createSeries(SeriesType.LINE,"Threshold");
		errorSerie.setSymbolType(PlotSymbolType.NONE);
		errorColor = new Color(Display.getDefault(), 255, 0, 0);
		errorSerie.setLineColor(errorColor);
		TenMinutesContainer.getInstance().setChartViewer(this);
	}
	
	@Override
	public void dispose() {
		errorColor.dispose();
		super.dispose();
	}
	
	@Override
	public void refreshChart(final double[] values, final Date[] times) {
		final double[] errorValues = new double[values.length];
		for (int t=0; t<values.length; t++) {
			errorValues[t]=errorThreshold;
		}
		Runnable refresh = new Runnable() {
			public void run() {
				System.out.println("asyncSec");
				errorSerie.setYSeries(errorValues);
				errorSerie.setXDateSeries(times);
				nAlarmsSerie.setYSeries(values);
				nAlarmsSerie.setXDateSeries(times);
				IAxisSet axisSet = chart.getAxisSet();
				axisSet.adjustRange();
				chart.redraw();
			}
		};
		Display.getDefault().asyncExec(refresh);
	}

	@Override
	public void setFocus() {}
}
