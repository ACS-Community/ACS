package com.cosylab.gui.components.r2;

/**
 * Insert the type's description here.
 * Creation date: (4/14/2002 16:16:31)
 * @author: 
 */
public interface ProgressMonitor {
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:24:13)
 * @param pl com.cosylab.logging.engine.LogArchive.ProgressListener
 */
void addProgressListener(ProgressListener pl);
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:17:02)
 * @return int
 */
int getCurrent();
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:22:22)
 */
String getStatus();
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:17:11)
 */
int getTotal();
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 16:24:27)
 * @param pl com.cosylab.logging.engine.LogArchive.ProgressListener
 */
void removeProgressListener(ProgressListener pl);
}
