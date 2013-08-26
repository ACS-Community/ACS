package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/24/00 18:23:12)
 * @author: 
 */
public interface ChartService {
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:23:43)
 * @return int
 */
int getChartDisplayablePointCount();
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:31:47)
 * @return si.ijs.kgb.chart.ChartStyleManager
 */
ChartStyleManager getChartStyleManager();
/**
 * Insert the method's description here.
 * Creation date: (24/12/01 16:18:19)
 * @return com.cosylab.gui.chart.ChartViewManager
 */
ChartViewManager getViewManager();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:23:56)
 */
void updateChart(ChartUpdateRequest request);
}
