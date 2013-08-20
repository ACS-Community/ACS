package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (28.1.2002 11:31:26)
 * @author: 
 */
public class DefaultChartDecorator extends AbstractChartDecorator {
/**
 * DefaultChartDecorator constructor comment.
 */
public DefaultChartDecorator() {
	super();
	add(flavor= new DefaultChartFlavor());
	add(new SimpleTrendFlavor());
	add(new HistoryTrendFlavor());
	add(new TimeBaseTrendFlavor());
}
/**
 * Returns description text of this flavor.
 * @return java.lang.String
 */
public java.lang.String getDescription() {
	return "This decorator contains default preset flavors for normal and trend chart.";
}
/**
 * Returns unique name of this flavor. 
 * @return java.lang.String
 */
public java.lang.String getName() {
	return "Default Chart Decorator";
}
}
