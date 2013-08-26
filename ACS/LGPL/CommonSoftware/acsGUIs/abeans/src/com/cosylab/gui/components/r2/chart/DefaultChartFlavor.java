package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (28.1.2002 11:50:22)
 * @author: 
 */
public class DefaultChartFlavor implements ChartFlavor {
	protected java.util.ArrayList models = new java.util.ArrayList();
/**
 * DefaultChartFlavor constructor comment.
 */
public DefaultChartFlavor() {
	super();
}
/**
 * Returns <code>ChartArea</code> interface.
 * @return si.ijs.anka.databush.utilities.ChartArea
 */
public ChartArea getChartArea() {
	return new DefaultChartArea();
}
/**
 * Returns ChartStyleManager
 * @return si.ijs.kgb.chart.ChartStyleManager
 */
public ChartStyleManager getChartStyleManager() {
return new DefaultChartStyleManager();
}
/**
 * Returns array of default ChartDataModels for tests and demonstration.
 * @return com.cosylab.gui.chart.ChartDataModel[]
 */
public com.cosylab.gui.components.r2.chart.ChartDataModel[] getDefaultModels() {
	com.cosylab.gui.components.r2.chart.ChartDataModel[] m= new com.cosylab.gui.components.r2.chart.ChartDataModel[models.size()];
	models.toArray(m);
	return m;
}
/**
 * Returns description text of this flavor.
 * @return java.lang.String
 */
public String getDescription() {
	return "Default implamantation of ChartFlavor interface.";
}
/**
 * Returns unique name of this flavor. 
 * @return java.lang.String
 */
public String getName() {
	return "DefaultChartFlavor";
}
/**
 * Returns ChartViewManager
 * @return com.cosylab.gui.chart.ChartViewManager
 */
public ChartViewManager getViewManager() {
	return new DefaultChartViewManager();
}
/**
 * Returns ChartXAxis fot this decoration
 * @return si.ijs.kgb.chart.ChartXAxis
 */
public ChartXAxis getXAxis() {
	return new DefaultChartXAxis();
}
/**
 * Returns ChartYAxis for this decoration.
 * @return si.ijs.kgb.chart.ChartYAxis
 */
public ChartYAxis getYAxis() {
	return new DefaultChartYAxis();
}
}
