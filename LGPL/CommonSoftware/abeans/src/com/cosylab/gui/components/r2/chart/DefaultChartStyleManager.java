package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (31.03.2001 00:33:47)
 * @author: 
 */
public class DefaultChartStyleManager implements ChartStyleManager {
	protected java.util.ArrayList styles = new java.util.ArrayList();
	protected int last = 0;
	protected java.awt.Color[] colors= {java.awt.Color.black,java.awt.Color.red,java.awt.Color.blue,java.awt.Color.green,java.awt.Color.magenta,java.awt.Color.cyan};
/**
 * DefaultChartStyleManager constructor comment.
 */
public DefaultChartStyleManager() {
	super();
	for (int i=0; i<colors.length; i++) styles.add(new PlainChartStyle(PlainChartStyle.LINE_PLAIN,colors[i],PlainChartStyle.SYMBOL_DOT,7,colors[i]));
	for (int i=0; i<colors.length; i++) styles.add(new PlainChartStyle(PlainChartStyle.LINE_PLAIN,colors[i],PlainChartStyle.SYMBOL_DOT,7,colors[colors.length-i-1]));
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:33:47)
 * @return si.ijs.kgb.chart.ChartStyle
 * @param data si.ijs.kgb.chart.ChartDataModel
 */
public void getStyleForSerie(ChartDataModel data) {
	if (data.getChartStyle()!=null) return;
	data.setChartStyle((ChartStyle)styles.get(last++));
	if (last==styles.size()) last=0;
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:36:12)
 * @return java.util.ArrayList
 */
public java.util.ArrayList getStyles() {
	return styles;
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:33:47)
 * @param data si.ijs.kgb.chart.ChartDataModel
 */
public void returnStyle(ChartDataModel data) {}
}
