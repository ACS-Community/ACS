package com.cosylab.gui.components.r2.chart;

/**
 * This is link between ChartArea and ChartDataModel. 
 * Performs such operations as preprocessing of points and autoscaling.
 */
interface ChartDataService {
	int DONE = 0;
	int FAILED = 1;
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 23:44:00)
 * @param service com.cosylab.gui.chart.ChartService
 */
ChartService getChartService();
public ChartDataModel getDataModel();
public PointTransformerModel getPointTransformer();
/*
 * This service is asked to prepare itself for transformation. 
 * Operations like autoscaling and filtering could be done at this time.
 * ChartAred only guarantee to call this method before transformation. 
 * Some implementation need this method to be called synchronized with other transformers.
 */
public void prepare();
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 23:44:00)
 * @param service com.cosylab.gui.chart.ChartService
 */
void setChartService(ChartService service);
/*
 * Performes transformation from data points to chart points.
 * Return code signalize success of transformation.
 * @return boolean valid return codes are
 * <ul>
 *   <li><code>DONE</code> - if transformation was performed</li>
 *   <li><code>FAILED</code> - if transformation failed ofr some reason, e.g. no data available.</li>
 * </ul>
 */
public int transform();
}
