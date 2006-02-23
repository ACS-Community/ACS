package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (17.12.2001 19:35:37)
 * @author: 
 */
public interface DataManager {
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 20:48:58)
 * @param listener com.cosylab.gui.chart.ChartDataListener
 */
void addDataChartListener(ChartDataListener listener);
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 20:48:58)
 * @param listener com.cosylab.gui.chart.ChartDataListener
 */
void removeDataChartListener(ChartDataListener listener);
}
