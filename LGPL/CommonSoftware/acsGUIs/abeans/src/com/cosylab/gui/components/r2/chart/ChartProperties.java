package com.cosylab.gui.components.r2.chart;

public class ChartProperties {
	private static ChartProperties prop;
/* 
 * Chart property for debugging the performance of chart.
 */
	public final static java.lang.String DEBUG_PERFORMANCE = "cosylab.chart.debug.performance";
/* 
 * Chart property for debugging data transformation and handling.
 */
	public final static java.lang.String DEBUG_DATA = "cosylab.chart.debug.data";
/* 
 * Chart property for debugging the drawing of graphics on screen.
 */
	public final static java.lang.String DEBUG_GRAPHICS = "cosylab.chart.debug.graphics";
	private boolean debugData = false;
	private boolean debugGraphics = false;
	private boolean debugPerformance = false;
	public final static java.lang.String CHART_RESOURCES = "chart_properties.data";
/**
 * ChartProperties constructor comment.
 */
private ChartProperties() {
	super();

	java.util.Properties p= new java.util.Properties();

	try {
		p.load(ClassLoader.getSystemResourceAsStream(CHART_RESOURCES));
		loadProperties(p);
	} catch (Throwable e) {
		System.out.println("[BaseChart] Debug chart properties not loaded.");
	}
}
/**
 * Insert the method's description here.
 * Creation date: (24/12/01 16:49:35)
 * @return com.cosylab.gui.chart.ChartProperties
 */
private synchronized static void createChartProperties() {
	if (prop==null) {
		prop= new ChartProperties();
	}
}
/**
 * Insert the method's description here.
 * Creation date: (24/12/01 16:49:35)
 * @return com.cosylab.gui.chart.ChartProperties
 */
public static ChartProperties getChartProperties() {
	if (prop==null)
		createChartProperties();
	return prop;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 21:09:57)
 * @return boolean
 */
public boolean isDebugData() {
	return debugData;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 21:10:49)
 * @return boolean
 */
public boolean isDebugGraphics() {
	return debugGraphics;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 21:11:09)
 * @return boolean
 */
public boolean isDebugPerformance() {
	return debugPerformance;
}
/**
 * Insert the method's description here.
 * Creation date: (9.8.2002 21:01:23)
 * @param properties java.util.Properties
 */
public void loadProperties(java.util.Properties properties) {
	if (properties.containsKey(DEBUG_DATA)) debugData= Boolean.valueOf(properties.getProperty(DEBUG_DATA)).booleanValue();
	if (properties.containsKey(DEBUG_GRAPHICS)) debugGraphics= Boolean.valueOf(properties.getProperty(DEBUG_GRAPHICS)).booleanValue();
	if (properties.containsKey(DEBUG_PERFORMANCE)) debugPerformance= Boolean.valueOf(properties.getProperty(DEBUG_PERFORMANCE)).booleanValue();
}
/**
 * Insert the method's description here.
 * Creation date: (9.8.2002 21:01:23)
 * @param properties java.util.Properties
 */
public void saveProperties(java.util.Properties properties) {
	properties.setProperty(DEBUG_DATA,new Boolean(debugData).toString());
	properties.setProperty(DEBUG_GRAPHICS,new Boolean(debugGraphics).toString());
	properties.setProperty(DEBUG_PERFORMANCE,new Boolean(debugPerformance).toString());
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 21:09:57)
 * @param newDebugData boolean
 */
public void setDebugData(boolean newDebugData) {
	debugData = newDebugData;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 21:10:49)
 * @param newDebugGraphics boolean
 */
public void setDebugGraphics(boolean newDebugGraphics) {
	debugGraphics = newDebugGraphics;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 21:11:09)
 * @param newDebugPerformance boolean
 */
public void setDebugPerformance(boolean newDebugPerformance) {
	debugPerformance = newDebugPerformance;
}
}
