package com.cosylab.gui.components.r2;

import java.util.ArrayList;
/**
 * Insert the type's description here.
 * Creation date: (4/14/2002 16:25:02)
 * @author: 
 */
public abstract class AbstractProgressTask extends Thread implements ProgressMonitor {

	private final ArrayList listeners = new ArrayList();
/**
 * AbstractProgressTask constructor comment.
 */
public AbstractProgressTask() {
	super();
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:25:02)
 * @param pl com.cosylab.logging.engine.LogArchive.ProgressListener
 */
public void addProgressListener(ProgressListener pl) {
	listeners.add(pl);	
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:27:58)
 * @param status java.lang.String
 * @param current int
 * @param total int
 */
protected void fireProgressEvent(String status, int current, int total) {
	yield();
	ProgressEvent e = new ProgressEvent(this, status, current, total);
	
	for (int i = 0; i < listeners.size(); i++) {
		Object listener = listeners.get(i);
		if (listener instanceof ProgressListener) {
			((ProgressListener)listener).progress(e);
		}
	}	
	yield();
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:35:24)
 */
public void fireTaskComplete() {
	yield();
	ProgressEvent e = new ProgressEvent(this, getStatus(), getCurrent(), getTotal());
	
	for (int i = 0; i < listeners.size(); i++) {
		Object listener = listeners.get(i);
		if (listener instanceof ProgressListener) {
			((ProgressListener)listener).taskComplete(e);
		}
	}	
	yield();
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:44:44)
 */
public void fireTaskInterrupted(String reason) {
	yield();
    ProgressEvent e = new ProgressEvent(this, reason, 0, -1);

    for (int i = 0; i < listeners.size(); i++) {
        Object listener = listeners.get(i);
        if (listener instanceof ProgressListener) {
            ((ProgressListener) listener).taskInterruped(e);
        }
    }
	yield();
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:34:27)
 */
protected void fireTaskStarted() {
	yield();
	ProgressEvent e = new ProgressEvent(this, getStatus(), getCurrent(), getTotal());
	
	for (int i = 0; i < listeners.size(); i++) {
		Object listener = listeners.get(i);
		if (listener instanceof ProgressListener) {
			((ProgressListener)listener).taskStarted(e);
		}
	}	
	yield();
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:30:34)
 */
protected void immediateUpdate() {
	fireProgressEvent(getStatus(), getCurrent(), getTotal());	
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:25:02)
 * @param pl com.cosylab.logging.engine.LogArchive.ProgressListener
 */
public void removeProgressListener(ProgressListener pl) {
	listeners.remove(pl);
}
}
