package cern.laser.test;

import java.util.Iterator;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.console.CommentedAlarm;
import cern.laser.console.CommentedAlarmMap;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConfigurationNotFoundException;
import cern.laser.console.User;
import cern.laser.console.UserHandler;

/**
 *
 * @author  pawlowsk
 */
public class TestSelectionAndConfiguration  implements AlarmSelectionListener {


    public TestSelectionAndConfiguration() throws Exception {
        // like login user
        
        UserHandler userHandler = UserHandler.get();
        User user = userHandler.getUser("laser");
        
    
        String DEFAULT_CONFIGURATION_NAME = "DEFAULT_CONF_NAME_CHANGE_IT";
        try {
            if ( user.getConfiguration(DEFAULT_CONFIGURATION_NAME) != null )
                user.removeConfiguration(DEFAULT_CONFIGURATION_NAME);
        } catch (LaserConfigurationNotFoundException lcnf) {
            lcnf.printStackTrace();
        }
        Configuration loadedConfiguration = null;
        Configuration confTemp = null;
        if ( (confTemp = user.getDefaultConfiguration()) == null ) {
            System.out.println("user: " + user.getName() + " does not have default configuration");
            loadedConfiguration = user.createConfiguration(
                                            DEFAULT_CONFIGURATION_NAME);
        } else {
            loadedConfiguration = confTemp;
        }
        
        System.out.println("selection: ");
        System.out.println(loadedConfiguration.getSelection()); 

        System.out.println("inhibited alarms: ");
        CommentedAlarmMap inhibitList = loadedConfiguration.getInhibited();
        for (java.util.Iterator iter = inhibitList.values().iterator(); iter.hasNext();) {
            CommentedAlarm commentedAlarm = (CommentedAlarm) iter.next();
            System.out.println(commentedAlarm.getAlarm().getAlarmId() + 
                    " " + commentedAlarm.getAlarm().getTriplet().getFaultFamily() +
                    " " + commentedAlarm.getAlarm().getTriplet().getFaultMember() +
                    " " + commentedAlarm.getAlarm().getTriplet().getFaultCode() 
                    ); 
        }

        //AlarmSelectionHandler jms_selectionHandler = AlarmSelectionHandler
        //    AlarmSelectionHandlerFactory.getHandler();
        AlarmSelectionHandler jms_selectionHandler =
            AlarmSelectionHandler.get();
 
        System.out.println("here I am");
            
        System.out.println("selecting ........... ");
        java.util.Map _activeList = 
            jms_selectionHandler.select(loadedConfiguration.getSelection(), this);
        System.out.println("selected " + _activeList.size() + " alarms "); 

                // process inhibit list
        System.out.println("processing inhibit list");
        for (Iterator iter = inhibitList.values().iterator(); iter.hasNext(); ) {
            // remove these alarms from active list           
            Alarm alarm = ((CommentedAlarm) iter.next()).getAlarm();
            System.out.println("inhibited alarm: " +
                alarm.getTriplet().getFaultFamily() + " " +
                alarm.getTriplet().getFaultMember() + " " +
                alarm.getTriplet().getFaultCode());
            //if ( _activeList.contains(alarm) ) {
            //    logger.debug("active list has this alarm, and this alarm is inhibited");
            //    _activeList.remove(alarm);
            //}
            for (Iterator iter1 = _activeList.values().iterator(); iter1.hasNext(); ) {
                Alarm alarm1 = (Alarm) iter1.next();
                //System.out.println("active alarm: " +
                //alarm1.getTriplet().getFaultFamily() + " " +
                //alarm1.getTriplet().getFaultMember() + " " +
                //alarm1.getTriplet().getFaultCode());
                //if ( alarm.getTriplet().getFaultFamily().equals(alarm1.getTriplet().getFaultFamily()) &&
                //     alarm.getTriplet().getFaultMember().equals(alarm1.getTriplet().getFaultMember()) &&
                //     alarm.getTriplet().getFaultCode().equals(alarm1.getTriplet().getFaultCode()) ) 
                //{
                //    System.out.println(alarm.getAlarmId() + " == " + alarm1.getAlarmId());
                //}                   
                if ( alarm.getAlarmId()  == alarm1.getAlarmId() ) 
                    System.out.println(alarm.getAlarmId() + " == " + alarm1.getAlarmId());
                if ( alarm.getAlarmId().equals(alarm1.getAlarmId()) ) 
                    System.out.println(alarm.getAlarmId() + ".equals(" + alarm1.getAlarmId() + ")");
                
            }

            //_activeList.remove(((CommentedAlarm) iter.next()).getAlarm());
        }

        System.out.println("after processing inhibit list");

    }


    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws Exception, Error {
        
        TestSelectionAndConfiguration test = new TestSelectionAndConfiguration();

 
    }
    
    public void onAlarm(Alarm alarm) 
    {
        System.out.println("received :\n" + alarm.getTriplet() + alarm.getStatus());
    }

    public void onException(LaserSelectionException e) 
    {
        System.out.println(e.getCode());
    }

 
}
 
