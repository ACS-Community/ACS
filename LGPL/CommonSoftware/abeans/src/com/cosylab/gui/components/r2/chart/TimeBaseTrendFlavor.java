package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (28.1.2002 12:01:53)
 * @author: 
 */
public class TimeBaseTrendFlavor extends HistoryTrendFlavor {
/**
 * TrendFlavor constructor comment.
 */
public TimeBaseTrendFlavor() {
	super();
}
/**
 * Returns description text of this flavor.
 * @return java.lang.String
 */
public String getDescription() {
	return "Trend flavor that saves and redraws catched points. X axis is time based";
}
/**
 * Returns unique name of this flavor. 
 * @return java.lang.String
 */
public String getName() {
	return "TimeBaseTrendChartFlavor";
}
/**
 * Returns ChartXAxis fot this decoration
 * @return si.ijs.kgb.chart.ChartXAxis
 */
public ChartXAxis getXAxis() {
	return new DefaultTimeXAxis();
}
}
