package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (28.1.2002 16:35:19)
 * @author: 
 */
public class DebugChartArea {
	public DefaultChartArea defaultChartArea;
/**
 * DebugChartArea constructor comment.
 */
public DebugChartArea() {
	super();
}
/**
 * DebugChartArea constructor comment.
 */
public DebugChartArea(DefaultChartArea ca) {
	super();
	defaultChartArea= ca;
}
/**
 * Insert the method's description here.
 * Creation date: (28.1.2002 16:39:06)
 * @return com.cosylab.gui.chart.ChartService
 * @param model com.cosylab.gui.chart.ChartDataModel
 */
public java.util.HashMap model2service() {
	return defaultChartArea.model2service;
}
}
