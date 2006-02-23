package com.cosylab.gui.components.r2.chart;

/**
 * This is interface that should be implemented by classes, which are data models for <code>BaseChart</code>.
 */
public interface ChartDataModel {
/**
 * This method is to return <code>ChartStyle</code>.
 * @return int
 */
ChartStyle getChartStyle();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 17:58:25)
 * @return int
 */
int getPointCount();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 17:58:05)
 * @return si.ijs.anka.databush.utilities.PointIterator
 */
PointIterator getPointIterator();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 17:59:10)
 * @return si.ijs.anka.databush.utilities.Interval
 */
Interval getPreferedXScale();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 17:59:10)
 * @return si.ijs.anka.databush.utilities.Interval
 */
Interval getPreferedYScale();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:27:16)
 */
void setChartService(ChartService s);
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 18:15:19)
 * @return int
 */
void setChartStyle(ChartStyle style);
}
