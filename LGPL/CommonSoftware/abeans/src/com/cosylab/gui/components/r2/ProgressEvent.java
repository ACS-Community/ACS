package com.cosylab.gui.components.r2;

import java.util.EventObject;
/**
 * Insert the type's description here.
 * Creation date: (4/14/2002 16:17:50)
 * @author: 
 */
public class ProgressEvent extends EventObject {
	private String status;
	private int current;
	private int total;

	public ProgressEvent(Object source, String status, int current, int total) {
		super(source);
		this.status = status;
		this.current = current;
		this.total = total;
	}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:17:50)
 * @return int
 */
public int getCurrent() {
	return current;
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:21:38)
 * @return java.lang.String
 */
public java.lang.String getStatus() {
	return status;
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:17:50)
 */
public int getTotal() {
	return total;
}
}
