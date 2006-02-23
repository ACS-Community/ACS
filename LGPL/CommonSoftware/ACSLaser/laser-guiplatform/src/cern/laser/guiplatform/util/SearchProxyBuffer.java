/*
 * SearchProxyBuffer.java
 *
 * Created on October 25, 2004, 2:44 PM
 */

package cern.laser.guiplatform.util;

import java.util.Iterator;

import org.apache.log4j.Logger;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSearchListener;
import cern.laser.client.services.selection.LaserSearchException;
import cern.laser.util.buffer.PullEvent;
import cern.laser.util.buffer.PullException;
import cern.laser.util.buffer.SynchroBuffer;
import cern.laser.util.buffer.SynchroBufferListener;

/**
 *
 * @author  woloszyn
 */
public class SearchProxyBuffer implements AlarmSearchListener, SynchroBufferListener {
    
    private static final Logger logger = LogFactory.getLogger(SearchProxyBuffer.class.getName());
    
    private SynchroBuffer buffer = null;
    
    private java.util.Vector listeners;
    
    private static SearchProxyBuffer INSTANCE = null;
    
    private boolean isSearchCancelled;
    
    /** Creates a new instance of SearchProxyBuffer */
    public SearchProxyBuffer() {
        isSearchCancelled = false;
        buffer = new SynchroBuffer(200, 2000, 100, SynchroBuffer.DUPLICATE_OK);
        buffer.setSynchroBufferListener(this);
    }
    
    public void enable() {
        buffer.enable();
    }
    
    public void disable() {
        buffer.disable();
    }
    
    public void close() {        
        buffer.close();
        listeners = null;
        INSTANCE = null;
    }
    
    public boolean isSearchCancelled() {
        return isSearchCancelled;
    }
    
    public static synchronized SearchProxyBuffer getDefault() {
        if ( INSTANCE == null)
            INSTANCE = new SearchProxyBuffer();
        
        return INSTANCE;
    }
    
    public void onSearchAlarm(Alarm alarm) {
        logger.debug("pushed: " + alarm.getTriplet().toString());
        buffer.push(alarm);
    }
    
    public void onSearchException(LaserSearchException laserSearchException) {
        if ( listeners != null )
            for ( Iterator iter = listeners.iterator(); iter.hasNext(); )
                ((AlarmSearchListener) iter.next()).onSearchException(laserSearchException);
    }
    
    public void searchFinished() {
        // wait until buffer not empty
        // CAN'T IMPLEMENT THIS NOW, (SynchroBuffer doesn't have needed methods)
        
        // notify all listeners
        if ( listeners != null )
            for ( Iterator iter = listeners.iterator(); iter.hasNext(); )
                ((AlarmSearchListener) iter.next()).searchFinished();            
    }

    public void searchCancelled() {
        isSearchCancelled = true;
    }    
    
    public void pull(PullEvent pullEvent) throws PullException {
        if ( listeners != null )
            for ( Iterator listenerIter = listeners.iterator(); listenerIter.hasNext(); ) {
                AlarmSearchListener listener = (AlarmSearchListener) listenerIter.next();
                for (Iterator alarmIter = pullEvent.getPulled().iterator(); alarmIter.hasNext(); )
                    listener.onSearchAlarm((Alarm) alarmIter.next());
            }
    }
    
    public synchronized void registerAlarmSearchListener(AlarmSearchListener listener) {
        if (listeners == null) {
            listeners = new java.util.Vector();
        }
        listeners.addElement(listener);
    }
    
    public synchronized void unregisterAlarmSearchListener1(AlarmSearchListener listener) {
        if (listeners == null) {
            return;
        }
        listeners.removeElement(listener);
    }
}
