/*
 * Constants.java
 *
 * Created on April 11, 2003, 2:04 PM
 */

package cern.laser.guiplatform.util;

import org.openide.util.NbBundle;

/**
 * This class stores constant variables.
 * @author  pawlowsk
 */
public final class Constants {
    
    /** alaram workspace name */
    public static final String ALARM_WORKSPACE_NAME = "alarmconsole";
    
    /** login mode name */
    public static final String ALARM_LOGIN_MODE_NAME = "login";
    
    /** alarm list mode name */
    public static final String ALARM_LIST_MODE_NAME = "alarmlist";

    /** alarm details mode name */
    public static final String ALARM_DETAILS_MODE_NAME = "alarmdetails";

    /** instant list mode name */
    public static final String INSTANT_LIST_MODE_NAME  = "instantlist"; 
    
    /** output mode name 
     * @deprecated never use
     */
    public static final String OUTPUT_MODE_NAME = "output";
    
    /** output TopComponent name */
    public static final String OUTPUT_TOP_COPOMONENT_NAME = "Output";

    /** 
     * AlarmBean property names  
     * used for actvie list explorer
     */
    private static final String [] propertyNames = 
        new String[] {//"timestamp",
                      "date",
                      "time",
                      "faultFamily", 
                      "faultMember",
                      "faultCode", 
                      "systemName",
                      "identifier", 
                      "priority", 
                      //"isActive", 
                      "problemDescription",
                      "sourceName",
        };        
    /** property names for explorers different than active list 
     */
    private static final String [] otherThanActvieExplPropNames =
        new String [] { "identifier", "systemName", "problemDescription"};

    /** displayable columns names for each property */
    private static final String [] displayColumnNames = 
        new String [] { 
            //NbBundle.getMessage(Constants.class, "TIMESTAMP_displayName"), 
            NbBundle.getMessage(Constants.class, "date_displayName"), 
            NbBundle.getMessage(Constants.class, "time_displayName"), 
            NbBundle.getMessage(Constants.class, "FAULT_FAMILY_displayName"), 
            NbBundle.getMessage(Constants.class, "FAULT_MEMBER_displayName"), 
            NbBundle.getMessage(Constants.class, "FAULT_CODE_displayName"), 
            NbBundle.getMessage(Constants.class, "SYSTEM_NAME_displayName"), 
            NbBundle.getMessage(Constants.class, "IDENTIFIER_displayName"), 
            NbBundle.getMessage(Constants.class, "PRIORITY_displayName"), 
            //NbBundle.getMessage(Constants.class, "ACTIVE_displayName"), 
            NbBundle.getMessage(Constants.class, "PROBLEM_DESCRIPTION_displayName"), 
            NbBundle.getMessage(Constants.class, "SOURCE_NAME_displayName"), 
                        };
   

    /** enable display column name (icon path) */
    public static final String ENABLE_COLUMN_ICON_PATH = 
        "cern/laser/guiplatform/images/status_unlocked.gif";
    
    /** disable dispaly column name (icon path) */
    public static final String DISABLE_COLUMN_ICON_PATH = 
        "cern/laser/guiplatform/images/status_lock.gif";

    /** login window height */
    public static final int LOGIN_WINDOW_HEIGHT = 300;
    /** login window width */
    public static final int LOGIN_WINDOW_WIDTH = 415;

    /** this variable should be used when listener wants to listen for
     * active list (as parameter for AlarmNodeManager
     * active list
     */ 
    public static final String ACTIVE_LISTENER_KEY = "ACTIVE_LIST_LISTENER";
    /** this variable should be used when listener wants to listen for
     * inhibit list (as parameter for AlarmNodeManager
     * inhibit list
     */
    public static final String INHIBIT_LISTENER_KEY = "INHIBIT_LIST_LISTENER";
    /** this variable should be used when listener wants to listen for
     * masked list (as parameter for AlarmNodeManager
     * masked list
     */
    public static final String MASKED_LISTENER_KEY = "MASKED_LIST_LISTENER";
    /** this variable should be used when listener wants to listen for
     * auto highlited list (as parameter for AlarmNodeManager
     * auto highlited list
     */
    public static final String HIGHLITED_LISTENER_KEY = "AUTO_HIGHLITED_LIST_LISTENER";
    /** this variable should be used when listener wants to listen for
     * instant list (as parameter for AlarmNodeManager
     * highlited list
     */
    public static final String INSTANT_LISTENER_KEY = "INSTANT_LIST_LISTENER";

    public static final String SEARCH_LISTENER_KEY = "SEARCH_LISTENER_KEY";
    
    /** This variable indicates that console is conneted to test alarm generator
     */ 
    public static final int TEST_WORKING_MODE = 1000;

    /** This variable indicates that console is conneted to business layer 
     */ 
    public static final int BUSINESS_LAYER_WORKING_MODE = 2000;

    /** This is default configuration name, this name should be always changed
     * by user. This variable is used for creating ConsoleConfigurationWindow 
     * when does not have any configurations or when user does not have default 
     * configuration    
     * Basically this is empty configuration
     */
    public static final String DEFAULT_CONFIGURATION_NAME =
        "NOT SAVED DEFAULT CONFIGURATION";
    
    /** This is recently applied onfiguratoin by user, when user clicks Apply
     * and new configuration should be created
     */
    public static final String RECENTLY_APPLIED_CONFIGURATION = 
        "NOT SAVED RECENTLY LOADED CONFIGURATION";
    
    /**
     * Login and password of hidden user - guest user
     */
    public static final String GUEST_USER_NAME = "GUEST";
    public static final String GUEST_USER_PASSWORD = "guest";
    
    /**
     * Modes in which Search Window can be opened
     */
    public static final int SEARCH_WINDOW_MODE_ARCHIVE_SEARCH = 1;
    public static final int SEARCH_WINDOW_MODE_GET_ALARMINFO = 2;
    public static final int SEARCH_WINDOW_MODE_SEARCH_ACTIVE_LIST = 3;
    
    //
    // -- methods ---------------------------------------------------------
    //
    /** */
    private Constants() {
        // not instantiate
    }
    
    /** This method returns dispaly names for <code>AlarmBean</code>
     * property names
     */
    public static final String [] getDisplayColumnNames() {
        return displayColumnNames;
    }
    
    /**
     * This method returns <code>AlarmBean</code> property
     * names
     */
    public static final String [] getPropertyNames() {
        return propertyNames;
    }

    /**
     * This method returns <code>AlarmBean</code> property
     * names, for explorers like InhibitList explorer
     */
    public static final String [] getColumnsToDisplay() {
        return otherThanActvieExplPropNames;
    }
    /**
     * This method returns <code>AlarmBean</code> property
     * names, for search explorer
     */
    public static final String [] getColumnsToDisplayInSearch() {
        return otherThanActvieExplPropNames;
    }
    /**
     * This method returns default working mode, that is, console is connected to
     * test alarm generator, or to business layer
     */
    public static int getDefaultWorkingMode() {
        //return TEST_WORKING_MODE;
        return BUSINESS_LAYER_WORKING_MODE;
    } 
    

}
