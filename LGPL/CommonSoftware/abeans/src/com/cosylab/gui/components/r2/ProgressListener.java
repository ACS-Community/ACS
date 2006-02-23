package com.cosylab.gui.components.r2;

/**
 * Insert the type's description here.
 * Creation date: (4/14/2002 16:23:36)
 * @author: 
 */
public interface ProgressListener {
	public void progress(ProgressEvent e);
	public void taskComplete(ProgressEvent e);
	public void taskInterruped(ProgressEvent e);
	public void taskStarted(ProgressEvent e);
}
