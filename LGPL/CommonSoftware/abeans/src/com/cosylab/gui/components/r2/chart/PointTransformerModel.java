package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/24/00 11:15:52)
 * @author: 
 */
public interface PointTransformerModel {
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 14:34:46)
 */
void clear();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 11:18:35)
 * @return java.awt.Dimension
 */
java.awt.Rectangle getChartRectangle();
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 14:57:51)
 * @return si.ijs.kgb.chart.IntPoint
 */
IntPoint getFirstIntPoint();
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 14:57:51)
 * @return si.ijs.kgb.chart.IntPoint
 */
IntPoint getLastIntPoint();
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 14:57:51)
 * @return si.ijs.kgb.chart.IntPoint
 */
Point getLastPoint();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:47:56)
 * @return si.ijs.anka.databush.utilities.Point
 */
Point getMaxValue();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:48:11)
 * @return si.ijs.anka.databush.utilities.Point
 */
Point getMinValue();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 12:20:32)
 * @return si.ijs.anka.databush.utilities.Interval
 */
Interval getXScale();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 12:20:32)
 * @return si.ijs.anka.databush.utilities.Interval
 */
Interval getYScale();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 11:20:50)
 * @param count int
 * @param points si.ijs.anka.databush.utilities.PointIterator
 */
IntPointIterator pointIterator();
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 11:18:57)
 * @param size java.awt.Dimension
 */
void setChartRectangle(java.awt.Rectangle area);
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 12:21:03)
 * @param scale si.ijs.anka.databush.utilities.Interval
 */
void setXScale(Interval scale);
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 12:21:03)
 * @param scale si.ijs.anka.databush.utilities.Interval
 */
void setYScale(Interval scale);
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 11:20:50)
 * @param count int
 * @param points si.ijs.anka.databush.utilities.PointIterator
 */
void transform(PointIterator points);
}
