package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (25.1.2002 12:10:17)
 * @author: 
 */
public interface ChartDecorator {
/**
 * Applys selected flavor to the chart. Return value is true if succeeded.
 * @return boolean true if OK
 * @param chart com.cosylab.gui.chart.BaseChart
 * @param flavor com.cosylab.gui.chart.ChartFlavor
 */
boolean applyFlavor(BaseChart chart, ChartFlavor flavor);
/**
 * Applys models to charts DataArea.
 * @return boolean ture if OK
 * @param chart com.cosylab.gui.chart.BaseChart
 * @param models com.cosylab.gui.chart.ChartDataModel[]
 */
boolean applyModels(BaseChart chart, ChartDataModel[] models);
/**
 * Returns ChartFlavor by its name string.
 * @return com.cosylab.gui.chart.ChartFlavor
 */
ChartFlavor getChartFlavor(String name);
/**
 * Returns number of all ChartFlavor object containd in this decorator.
 * @return com.cosylab.gui.chart.ChartFlavor[]
 */
int getChartFlavorCount();
/**
 * Returns names of all ChartFlavor object contained in this decorator.
 * @return String[]
 */
String[] getChartFlavorNames();
/**
 * Returns all ChartFlavor object containd in this decorator.
 * @return com.cosylab.gui.chart.ChartFlavor[]
 */
ChartFlavor[] getChartFlavors();
/**
 * Returns default ChartFlavor for this decorator.
 * @return com.cosylab.gui.chart.ChartFlavor
 */
ChartFlavor getDefultChartFlavor();
/**
 * Returns description text of this flavor.
 * @return java.lang.String
 */
String getDescription();
/**
 * Returns unique name of this flavor. 
 * @return java.lang.String
 */
String getName();
}
