package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (17.12.2001 19:42:04)
 * @author: 
 */
public final class DataChange {
	public final static int RESET_ALL_CODE = 0;
	public final static int ALL_POINTS_CHANGED_CODE = 1;
	public final static int POINTS_CHANGED_CODE = 2;
	public final static int POINTS_ADDED_CODE = 3;
	public final static int POINTS_REMOVED_CODE = 4;
	private int code;
	public final static DataChange RESET_ALL = new DataChange(RESET_ALL_CODE);
	public final static DataChange ALL_POINTS_CHANGED = new DataChange(ALL_POINTS_CHANGED_CODE);
	public final static DataChange POINTS_CHANGED = new DataChange(POINTS_CHANGED_CODE);
	public final static DataChange POINTS_ADDED = new DataChange(POINTS_ADDED_CODE);
	public final static DataChange POINTS_REMOVED = new DataChange(POINTS_REMOVED_CODE);
private DataChange(int c) {
	code=c;
}
public int getCode() {
	return code;
}
}
