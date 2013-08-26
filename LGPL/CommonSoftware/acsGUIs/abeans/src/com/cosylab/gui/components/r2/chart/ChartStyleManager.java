package com.cosylab.gui.components.r2.chart;

/**
 * ChartStypeManager is interface of an object, which creates and manages styles for chart series.
 * Implementator of chart.
 */
public interface ChartStyleManager {
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:30:05)
 * @return si.ijs.kgb.chart.ChartStyle
 * @param data si.ijs.kgb.chart.ChartDataModel
 */
void getStyleForSerie(ChartDataModel data);
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:31:08)
 * @param data si.ijs.kgb.chart.ChartDataModel
 */
void returnStyle(ChartDataModel data);
}
