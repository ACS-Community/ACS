package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (28.1.2002 11:31:26)
 * @author: 
 */
public abstract class AbstractChartDecorator implements ChartDecorator {
	protected java.util.ArrayList flavors = new java.util.ArrayList();
	protected ChartFlavor flavor;
	protected java.util.HashMap hash = new java.util.HashMap();
/**
 * DefaultChartDecorator constructor comment.
 */
public AbstractChartDecorator() {
	super();
}
/**
 * Insert the method's description here.
 * Creation date: (28.1.2002 11:44:27)
 * @return boolean
 * @param flavor com.cosylab.gui.chart.ChartFlavor
 */
protected boolean add(ChartFlavor flavor) {
	Object o= hash.put(flavor.getName(),flavor);
	flavors.remove(o);
	flavors.add(flavor);
	return true;
}
/**
 * Applys selected flavor to the chart. Return value is true if succeeded.
 * @return boolean true if OK
 * @param chart com.cosylab.gui.chart.BaseChart
 * @param flavor com.cosylab.gui.chart.ChartFlavor
 */
public boolean applyFlavor(BaseChart chart, ChartFlavor flavor) {
	chart.setChartArea(flavor.getChartArea());
	chart.setChartStyleManager(flavor.getChartStyleManager());
	chart.setViewManager(flavor.getViewManager());
	chart.setXAxis(flavor.getXAxis());
	chart.setYAxis(flavor.getYAxis());
	return true;
}
/**
 * Applys models to charts DataArea.
 * @return boolean ture if OK
 * @param chart com.cosylab.gui.chart.BaseChart
 * @param models com.cosylab.gui.chart.ChartDataModel[]
 */
public boolean applyModels(BaseChart chart, com.cosylab.gui.components.r2.chart.ChartDataModel[] models) {
	for (int i=0; i<models.length;i++) chart.getChartArea().addDataModel(models[i]);
	return true;
}
/**
 * Returns ChartFlavor by its name.
 * @return com.cosylab.gui.chart.ChartFlavor
 */
public ChartFlavor getChartFlavor(java.lang.String name) {
	return (ChartFlavor)hash.get(name);
}
/**
 * Returns number of all ChartFlavor object containd in this decorator.
 * @return com.cosylab.gui.chart.ChartFlavor[]
 */
public int getChartFlavorCount() {
	return flavors.size();
}
/**
 * Returns names of all ChartFlavor object contained in this decorator.
 * @return String[]
 */
public java.lang.String[] getChartFlavorNames() {
	String[] s= new String[flavors.size()];
	for (int i=0; i<flavors.size();i++) s[i]=((ChartFlavor)flavors.get(i)).getName();
	return s;
}
/**
 * Returns all ChartFlavor object containd in this decorator.
 * @return com.cosylab.gui.chart.ChartFlavor[]
 */
public com.cosylab.gui.components.r2.chart.ChartFlavor[] getChartFlavors() {
	com.cosylab.gui.components.r2.chart.ChartFlavor[] m= new com.cosylab.gui.components.r2.chart.ChartFlavor[flavors.size()];
	flavors.toArray(m);
	return m;
}
/**
 * Returns default ChartFlavor for this decorator.
 * @return com.cosylab.gui.chart.ChartFlavor
 */
public ChartFlavor getDefultChartFlavor() {
	return flavor;
}
}
