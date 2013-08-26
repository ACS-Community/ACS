package com.cosylab.gui.components.r2.chart;

/**
 * This is defaul implementation of chart view manager interface. 
 * Chart view manager collects scales and extracts from them which particular scale is to 
 * be used instead particular user scale by point trasformation.
 */
public class DefaultChartViewManager implements ChartViewManager {
	protected boolean preferedYScaleUsed = true;
	protected boolean preferedXScaleUsed = true;
	protected Interval yScale= new Interval(0.0,1.0);
	protected Interval xScale= new Interval(0.0,1.0);
	protected java.util.ArrayList xScales = new java.util.ArrayList(10);
	protected java.util.ArrayList yScales = new java.util.ArrayList(10);
	protected Interval extXScale= new Interval(0.0,1.0);
	protected Interval extYScale= new Interval(0.0,1.0);
	protected IntervalListener xScaleListener = new IntervalListener() {
		public void intervalChange(Interval source) {
			if (!preferedXScaleUsed) extXScale.set(xScale);
		}
	};
	protected IntervalListener yScaleListener = new IntervalListener() {
		public void intervalChange(Interval source) {
			if (!preferedYScaleUsed) extYScale.set(yScale);
		}
	};
	protected IntervalListener yUserScaleListener = new IntervalListener() {
		public void intervalChange(Interval source) {
			if (preferedYScaleUsed) setToPreferedYScale();
		}
	};
	protected IntervalListener xUserScaleListener = new IntervalListener() {
		public void intervalChange(Interval source) {
			if (preferedXScaleUsed) setToPreferedXScale();
		}
	};
/**
 * ChartArea default constructor.
 */
public DefaultChartViewManager() {
	super();
}
/**
 * Insert the method's description here.
 * Creation date: (21.12.2001 14:33:40)
 * @param scale com.cosylab.gui.chart.Interval
 */
public Interval addUserXScale(Interval scale) {
	if (scale==null) return extXScale;
	xScales.add(scale);
	scale.addIntervalListener(xUserScaleListener);
	if (preferedXScaleUsed) {
		setToPreferedXScale();
	}
	return extXScale;
}
/**
 * Insert the method's description here.
 * Creation date: (21.12.2001 14:33:40)
 * @param scale com.cosylab.gui.chart.Interval
 */
public Interval addUserYScale(Interval scale) {
	if (scale==null) return extYScale;
	yScales.add(scale);
	scale.addIntervalListener(yUserScaleListener);
	if (preferedYScaleUsed) {
		setToPreferedYScale();
	}
	return extYScale;
}
/**
 * Insert the method's description here.
 * Creation date: (21.12.2001 14:33:40)
 * @return java.util.Iterator
 */
public java.util.Iterator extractedXScaleIterator() {
	return new java.util.Iterator() {
		Interval i=extXScale;
		public boolean hasNext() {
			return i!=null;
		}
		public Object next() {
			Interval t=i;
			i=null;
			return t;
		}
		public void remove() {}
	};
}
/**
 * Insert the method's description here.
 * Creation date: (21.12.2001 14:33:40)
 * @return java.util.Iterator
 */
public java.util.Iterator extractedYScaleIterator() {
	return new java.util.Iterator() {
		Interval i=extYScale;
		public boolean hasNext() {
			return i!=null;
		}
		public Object next() {
			Interval t=i;
			i=null;
			return t;
		}
		public void remove() {}
	};
}
/**
 * Insert the method's description here.
 * Creation date: (21.12.2001 14:33:40)
 * @return int
 */
public int getExctractedXScaleCount() {
	return 1;
}
/**
 * Insert the method's description here.
 * Creation date: (21.12.2001 14:33:40)
 * @return int
 */
public int getExctractedYScaleCount() {
	return 1;
}
/**
 * Insert the method's description here.
 * Creation date: (24/12/01 16:21:29)
 * @param scale com.cosylab.gui.chart.Interval
 */
public Interval getExtractedXScale(Interval scale) {
	return extXScale;
}
/**
 * Insert the method's description here.
 * Creation date: (24/12/01 16:21:29)
 * @param scale com.cosylab.gui.chart.Interval
 */
public Interval getExtractedYScale(Interval scale) {
	return extYScale;
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:43:23)
 * @return si.ijs.anka.databush.utilities.Interval
 */
public Interval getXScale() {
	return xScale;
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:43:49)
 * @return si.ijs.anka.databush.utilities.Interval
 */
public Interval getYScale() {
	return yScale;
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:21:12)
 * @return boolean
 */
public boolean isUserXScaleUsed() {
	return preferedXScaleUsed;
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:21:42)
 * @return boolean
 */
public boolean isUserYScaleUsed() {
	return preferedYScaleUsed;
}
/**
 * Insert the method's description here.
 * Creation date: (20.12.2001 17:13:25)
 * @param scale com.cosylab.gui.chart.Interval
 */
public void removeUserXScale(Interval scale) {
	xScales.remove(scale);
	scale.removeIntervalListener(xUserScaleListener);
}
/**
 * Insert the method's description here.
 * Creation date: (20.12.2001 17:13:25)
 * @param scale com.cosylab.gui.chart.Interval
 */
public void removeUserYScale(Interval scale) {
	yScales.remove(scale);
	scale.removeIntervalListener(yUserScaleListener);
}
/**
 * Insert the method's description here.
 * Creation date: (25/12/01 11:54:28)
 */
public void revalidate() {
	if (preferedXScaleUsed) {
		setToPreferedXScale();
	} else {
		extXScale.set(xScale);
	}
	if (preferedYScaleUsed) {
		setToPreferedYScale();
	} else {
		extYScale.set(yScale);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:44:27)
 */
protected void setToPreferedXScale() {
	java.util.Iterator it;
	Interval s;

	boolean first=true;

	it= xScales.iterator();
	while (it.hasNext()) {
		s= (Interval)it.next();
		if (first) {
			extXScale.set(s);
			first=false;
		} else extXScale.unionWith(s);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:44:27)
 */
protected void setToPreferedYScale() {
	java.util.Iterator it;
	Interval s;

	boolean first=true;

	it= yScales.iterator();
	while (it.hasNext()) {
		s= (Interval)it.next();
		if (first) {
			extYScale.set(s);
			first=false;
		} else extYScale.unionWith(s);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:21:12)
 * @param newPreferedXScaleUsed boolean
 */
public void setUserXScaleUsed(boolean newPreferedXScaleUsed) {
	preferedXScaleUsed = newPreferedXScaleUsed;
	if (preferedXScaleUsed) {
		setToPreferedXScale();
	} else {
		extXScale.set(xScale);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:21:42)
 * @param newPreferedYScaleUsed boolean
 */
public void setUserYScaleUsed(boolean newPreferedYScaleUsed) {
	preferedYScaleUsed = newPreferedYScaleUsed;
	if (preferedYScaleUsed) {
		setToPreferedYScale();
	} else {
		extYScale.set(yScale);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:43:23)
 * @param newXScale si.ijs.anka.databush.utilities.Interval
 */
public void setXScale(Interval newXScale) {
	if (newXScale==null) throw new NullPointerException("New scale is null");
	if (xScale!=null) xScale.removeIntervalListener(xScaleListener);
	xScale=newXScale;
	xScale.addIntervalListener(xScaleListener);
	preferedXScaleUsed=false;
	extXScale.set(xScale);
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 12:43:49)
 * @param newYScale si.ijs.anka.databush.utilities.Interval
 */
public void setYScale(Interval newYScale) {
	if (newYScale==null) throw new NullPointerException("New scale is null");
	if (yScale!=null) yScale.removeIntervalListener(yScaleListener);
	yScale=newYScale;
	yScale.addIntervalListener(yScaleListener);
	preferedYScaleUsed=false;
	extYScale.set(yScale);
}
/**
 * Insert the method's description here.
 * Creation date: (20.12.2001 17:13:25)
 * @return java.util.Iterator
 */
public java.util.Iterator xScaleIterator() {
	return xScales.iterator();
}
/**
 * Insert the method's description here.
 * Creation date: (20.12.2001 17:13:25)
 * @return java.util.Iterator
 */
public java.util.Iterator yScaleIterator() {
	return yScales.iterator();
}
}
