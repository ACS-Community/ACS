/*
 * InfoAlarmBean.java
 *
 * Created on November 17, 2003, 5:31 PM
 */

package cern.laser.guiplatform.alarms;

import cern.laser.client.data.Triplet;
import cern.laser.console.CommentedAlarm;
import cern.laser.guiplatform.capabilities.DetailsCapability;

/**
 * This class should be used to display alarm on InfoExplorer (Node Children and
 * Multiplicity Children), 
 * This class has different isActive() method which is used to calculate background
 * and foreground color
 *
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class InfoAlarmBean extends AlarmBean implements DetailsCapability {
    
    /** Creates a new instance of InfoAlarmBean */
    public InfoAlarmBean(CommentedAlarm commentedAlarm) {
        super(commentedAlarm);
    }

    public boolean isActive() {
        return commentedAlarm.getAlarm().getStatus().isActive();
    }


    /** */
    public String getName() {
        //return getPrivateIdentifier().toString();
        
        Triplet triplet = commentedAlarm.getAlarm().getTriplet();
        return triplet.getFaultFamily() + " " + triplet.getFaultMember() +
               " " + triplet.getFaultCode();

    }
  
}
