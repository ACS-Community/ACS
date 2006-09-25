/*
 * ProxyBuffer.java
 *
 * Created on October 6, 2003, 4:52 PM
 */

package cern.laser.guiplatform.util;

import java.util.Iterator;

import org.apache.log4j.Logger;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.util.buffer.PullEvent;
import cern.laser.util.buffer.PullException;
import cern.laser.util.buffer.SynchroBuffer;
import cern.laser.util.buffer.SynchroBufferListener;

/**
 *
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class ProxyBuffer implements AlarmSelectionListener, SynchroBufferListener {
    
    private static final Logger logger = 
            LogFactory.getLogger(ProxyBuffer.class.getName());
    
    private AlarmSelectionListener listener = null;
    
    private SynchroBuffer buffer = null;
    
    private static ProxyBuffer INSTANCE = null;

    /** Creates a new instance of ProxyBuffer */
    private ProxyBuffer() {
        
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
        listener = null;
        INSTANCE = null;
    }
    
    public static ProxyBuffer getDefault() {
        if ( INSTANCE == null) 
            INSTANCE = new ProxyBuffer();
        
        return INSTANCE;
    }
    
    public void onAlarm(Alarm alarm) {
        logger.debug("pushed: " + alarm.getTriplet().toString() + " isActive: " +
                alarm.getStatus().isActive()  + " isMasked: " + 
               alarm.getStatus().isMasked()  +
               " isReduced: " + alarm.getStatus().isReduced());
        buffer.push(alarm);
    }
    
    public void onException(LaserSelectionException e) {
        if ( listener != null )
            listener.onException(e);
    }
    
    public void pull(PullEvent pullEvent) throws PullException {
        if ( listener != null )
            for (Iterator iter = pullEvent.getPulled().iterator();
                    iter.hasNext(); )
            listener.onAlarm((Alarm) iter.next());
    }
    
    public void registerAlarmSelectionListener(AlarmSelectionListener listener) {
    	
        this.listener = listener;
    }
    
    public void unregisterAlarmSelectionListener() {
        listener = null;
    }
}
