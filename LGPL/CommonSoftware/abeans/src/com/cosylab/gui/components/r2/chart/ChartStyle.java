package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (31.03.2001 00:08:58)
 * @author: 
 */
public interface ChartStyle {
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:09:57)
 * @param x int[]
 * @param y int[]
 * @param g java.awt.Graphics
 */
void drawGraph(IntPointIterator points, java.awt.Graphics g);
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:13:37)
 * @return boolean
 */
boolean isEditable();
}
