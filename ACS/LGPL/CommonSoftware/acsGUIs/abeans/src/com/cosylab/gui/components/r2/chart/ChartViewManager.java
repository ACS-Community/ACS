package com.cosylab.gui.components.r2.chart;

/**
 * Interface for managing the scale with which chart is drawn. 
 * Implementatnion of this class must handle multiple prefered scales from data models and single external scale.
 * Implementation must calculate one or more extracted scale form prefered scales.
 * Basicly each user may have it's own extracted chart.<br>
 * This interface might be used for zooming in to the chart.
 */
public interface ChartViewManager {
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 19:15:35)
 * @param scale com.cosylab.gui.chart.Interval
 */
Interval addUserXScale(Interval scale);
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 19:15:35)
 * @param scale com.cosylab.gui.chart.Interval
 */
Interval addUserYScale(Interval scale);
/**
 * Insert the method's description here.
 * Creation date: (20.12.2001 16:27:01)
 * @return java.util.Iterator
 */
java.util.Iterator extractedXScaleIterator();
/**
 * Insert the method's description here.
 * Creation date: (20.12.2001 16:27:01)
 * @return java.util.Iterator
 */
java.util.Iterator extractedYScaleIterator();
/**
 * Insert the method's description here.
 * Creation date: (21.12.2001 14:29:41)
 * @return int
 */
int getExctractedXScaleCount();
/**
 * Insert the method's description here.
 * Creation date: (21.12.2001 14:29:41)
 * @return int
 */
int getExctractedYScaleCount();
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 19:15:35)
 * @param scale com.cosylab.gui.chart.Interval
 */
Interval getExtractedXScale(Interval scale);
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 19:15:35)
 * @param scale com.cosylab.gui.chart.Interval
 */
Interval getExtractedYScale(Interval scale);
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:43:23)
 * @return si.ijs.anka.databush.utilities.Interval
 */
public Interval getXScale();
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:43:23)
 * @return si.ijs.anka.databush.utilities.Interval
 */
public Interval getYScale();
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:21:12)
 * @return boolean
 */
public boolean isUserXScaleUsed();
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:21:12)
 * @return boolean
 */
public boolean isUserYScaleUsed();
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 19:15:35)
 * @param scale com.cosylab.gui.chart.Interval
 */
void removeUserXScale(Interval scale);
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 19:15:35)
 * @param scale com.cosylab.gui.chart.Interval
 */
void removeUserYScale(Interval scale);
/**
 * Insert the method's description here.
 * Creation date: (25/12/01 11:54:18)
 */
void revalidate();
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:21:12)
 * @param newPreferedXScaleUsed boolean
 */
public void setUserXScaleUsed(boolean newPreferedScaleUsed);
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:21:12)
 * @param newPreferedXScaleUsed boolean
 */
public void setUserYScaleUsed(boolean newPreferedScaleUsed);
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:43:23)
 * @param newXScale si.ijs.anka.databush.utilities.Interval
 */
public void setXScale(Interval newScale);
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:43:23)
 * @param newXScale si.ijs.anka.databush.utilities.Interval
 */
public void setYScale(Interval newScale);
/**
 * Insert the method's description here.
 * Creation date: (20.12.2001 16:27:01)
 * @return java.util.Iterator
 */
java.util.Iterator xScaleIterator();
/**
 * Insert the method's description here.
 * Creation date: (20.12.2001 16:27:01)
 * @return java.util.Iterator
 */
java.util.Iterator yScaleIterator();
}
