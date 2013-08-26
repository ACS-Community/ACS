package com.cosylab.gui.components.r2.chart;

/**
 * This class is used for updating or reloading. Its code tells what to update or reload.
 */
public final class ChartUpdateRequest {
	public final static int RELOAD_ALL_CODE = 0;
	public final static int UPDATE_ALL_CODE = 1;
	public final static int UPDATE_SERIE_CODE = 2;
	private int code = 0;
	public final static ChartUpdateRequest RELOAD_ALL = new ChartUpdateRequest(RELOAD_ALL_CODE);
	public final static ChartUpdateRequest UPDATE_ALL = new ChartUpdateRequest(UPDATE_ALL_CODE);
	public final static ChartUpdateRequest UPDATE_SERIE = new ChartUpdateRequest(UPDATE_SERIE_CODE);
	private java.lang.String name;
/**
 * ChartUpdateRequest constructor. Its parameter is code that indicates what to update or reload.
 */
private ChartUpdateRequest(int type) {
	super();
	this.code= type;
	switch (code) {
		case RELOAD_ALL_CODE: {
			name= "ChartUpdateRequest.RELOAD_ALL";
		} break;
		case UPDATE_ALL_CODE: {
			name= "ChartUpdateRequest.UPDATE_ALL";
		} break;
		case UPDATE_SERIE_CODE: {
			name= "ChartUpdateRequest.UPDATE_SERIE";
		} break;
	}
}
/**
 * This method return code.
 */
public int getCode() {
	return code;
}
/**
 * This method returns string which is name of this class object.
 */
public String toString() {
	return name;
}
}
