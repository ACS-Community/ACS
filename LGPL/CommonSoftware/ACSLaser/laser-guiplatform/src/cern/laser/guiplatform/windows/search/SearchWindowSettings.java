/*
 * SearchWindowSettings.java
 *
 * Created on October 18, 2004, 2:38 PM
 */

package cern.laser.guiplatform.windows.search;

import cern.laser.guiplatform.util.Constants;


/** This class contains settings of SearchWindow.
 * @author woloszyn
 */
public class SearchWindowSettings {
    // window mode
    private int windowMode; // one of Constants.SEARCH_WINDOW_MODE_* constants
    private int switchMode; 
    
    // -- CONSTANTS
    
    // Radio buttons setting constants
    /** Identifier field set up to ALL */
    public static final int ID_ALL = 1;     // ALL alarm identifiers
    /** Identifier field set up to EQUAL */
    public static final int ID_EQUAL = 2;   // alarm identifiers EQUALS given text
    /** Identifier field set up to CONTAINS */
    public static final int ID_CONTAINS = 3;// alarm identifiers CONTAINS given text
    
    // Sort options constants
    /** Type of sorting - WITHOUT SORTING */
    public static final int SORT_NONE = 0;
    /** Type of sorting - BY TIME */
    public static final int SORT_BY_TIME = 1;
    /** Type of sorting - BY PRIORITY */
    public static final int SORT_BY_PRIORITY = 2;
    /** Type of sorting - GROUP NEW & TERMINATED */
    public static final int SORT_BY_GROUP_NT = 3;
    /** Type of sorting - GROUP NEW & TERMINATED BY PRIORITY */
    public static final int SORT_BY_GROUP_NT_BY_PR = 4;
    
    // CAP host name constants
    /** CAP Host name Radio buttons set to ALL */
    public static final int CAP_HOST_NAME_ALL = 1;
    /** CAP Host name Radio buttons set to EQUALS */
    public static final int CAP_HOST_NAME_EQUALS = 2;
    
    /** Window in Mode SystemName/Identifier/ProblemDescription */
    public static int SWITCH_MODE_SN_I_PD = 1; 
    /** Window in Mode FF/FM/FC */
    public static int SWITCH_MODE_FF_FM_FC = 2; 
    
    // -- LOCAL VARIABLES
    
    // Identifiers Fields
    
    private AlarmIdentifierField SNFF;      // System Name / FF
    private AlarmIdentifierField IFM;       // Identifier  / FM
    private AlarmIdentifierField PDFC;      // ProblemDesc / FC
    
    // priority fields
    private boolean priority_0; // priority 0 selected?
    private boolean priority_1; // ...
    private boolean priority_2;
    private boolean priority_3;
    
    // timestamp fields
    private String after;
    private String before;
    
    // status fields
    private boolean active; // priority 0 selected?
    private boolean acknowledged; // ...
    private boolean highlighted;
    private boolean masked;
    private boolean terminated;
    
    // sort fields
    private int sortState; // what is selected
    
    // CAP host name fields
    private int CAPHostNameState; // all, equals
    private String CAPHostNameEquals;
    
    // -- CONSTRUCTORS
    
    /** Creates a new instance of SearchWindowSettings
     * @param windowMode mode which SearchWindow will work in
     */
    public SearchWindowSettings( int windowMode ) {
        this.windowMode = windowMode;
        
        switchMode = SWITCH_MODE_SN_I_PD;
        
        SNFF = new AlarmIdentifierField(windowMode);
        IFM  = new AlarmIdentifierField(windowMode);
        PDFC = new AlarmIdentifierField(windowMode);
        
        priority_0 = false;
        priority_1 = false;
        priority_2 = false;
        priority_3 = false;
        
        after = "";
        before = "";
        
        active = false;
        acknowledged = false;
        highlighted = false;
        masked = false;
        terminated = false;
        
        sortState = SORT_BY_TIME;
        
        CAPHostNameState = CAP_HOST_NAME_ALL;
        CAPHostNameEquals = "";
    }
    
    // -- PUBLIC METHODS
    
    // Window Mode
    public int getWindowMode() {
        return windowMode;
    }
    public void setWindowMode( int newWindowMode ) {
        windowMode = newWindowMode;
        SNFF.setWindowMode(newWindowMode);
        IFM.setWindowMode(newWindowMode);
        PDFC.setWindowMode(newWindowMode);
    }
    
    // SWITCH MODE
    public int getSwitchMode() {
        return switchMode;
    }
    public void setSwitchMode( int newSwitchMode ) {
        switchMode = newSwitchMode;
    }
    
    // IDENTIFIERS FIELDS
    
    // System Name / FF
    
    /** getter for SystemName/FF radio buttons group
     * @return state (one of ID_* constants)
     */
    public int getSNFFState() {
        return SNFF.getState();
    }
    /** setter for SystemName/FF radio buttons group
     * @param newState newState (one of ID_* constants)
     */
    public void setSNFFState(int newState){
        SNFF.setState(newState);
    }
    /** getter for SystemName/FF text used to search when Contains radio button selected
     * @return value from text field Contains
     */
    public String getSNFFContains() {
        return SNFF.getContains();
    }
    /** setter for SystemName/FF text used to search when Contains radio button selected
     * @param newContains new value for text field Contains
     */
    public void setSNFFContains( String newContains ){
        SNFF.setContains(newContains);
    }
    private void setSNFFWindowMode( int newWindowMode ) {
        SNFF.setWindowMode(newWindowMode);
    }
    private int getSNFFWindowMode() {
        return SNFF.getWindowMode();
    }
    // Identifier / FM
    
    public int getIFMState() {
        return IFM.getState();
    }
    public void setIFMState(int newState){
        IFM.setState(newState);
    }
    public String getIFMContains() {
        return IFM.getContains();
    }
    public void setIFMContains( String newContains ){
        IFM.setContains(newContains);
    }
    private void setIFMWindowMode( int newWindowMode ) {
        IFM.setWindowMode(newWindowMode);
    } 
    private int getIFMWindowMode() {
        return IFM.getWindowMode();
    }    
    // Problem Description / FC
    
    public int getPDFCState() {
        return PDFC.getState();
    }
    public void setPDFCState(int newState){
        PDFC.setState(newState);
    }
    public String getPDFCContains() {
        return PDFC.getContains();
    }
    public void setPDFCContains( String newContains ){
        PDFC.setContains(newContains);
    }
    private void setPDFCWindowMode( int newWindowMode ) {
        PDFC.setWindowMode(newWindowMode);
    } 
    private int getPDFCWindowMode() {
        return PDFC.getWindowMode();
    } 
    
    // PRIORITY FIELDS
    
    public boolean getPriority_0() {
        return priority_0;
    }
    public boolean getPriority_1() {
        return priority_1;
    }
    public boolean getPriority_2() {
        return priority_2;
    }
    public boolean getPriority_3() {
        return priority_3;
    }
    public void setPriority_0( boolean newPriority_0) {
        priority_0 = newPriority_0;
    }
    public void setPriority_1( boolean newPriority_1) {
        priority_1 = newPriority_1;
    }
    public void setPriority_2( boolean newPriority_2) {
        priority_2 = newPriority_2;
    }
    public void setPriority_3( boolean newPriority_3) {
        priority_3 = newPriority_3;
    }
    
    // TIMESTAMP FIELDS
    
    public boolean isTimeStampEnabled() {
        if ( windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO)
            return false;
        else
            return true;
    }
    public String getTimeStampAfter() {
        // timestamp is disabled and empty when in GetAlarmInfo Mode
        if ( windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO)
            return "";
        else
            return after;
    }
    public void setTimeStampAfter(String newAfter) {
        after = newAfter;
    }
    public String getTimeStampBefore() {
        return before;
    }
    public void setTimeStampBefore(String newBefore) {
        before = newBefore;
    }
    
    // STATUS FIELD
    
    public boolean getStatusActive() {
        if ( ! isStatusActiveEnabled() )
            return false;
        else
            return active;
    }
    public boolean getStatusAcknowledged() {
        if ( ! isStatusAcknowledgedEnabled() )
            return false;
        else
            return acknowledged;
    }
    public boolean getStatusHighlighted() {
        if ( ! isStatusHighlightedEnabled() )
            return false;
        else
            return highlighted;
    }
    public boolean getStatusMasked() {
        if ( ! isStatusMaskedEnabled() )
            return false;
        else
            return masked;
    }
    public boolean getStatusTerminated() {
        if ( ! isStatusTerminatedEnabled() )
            return false;
        else
            return terminated;
    }
    public void setStatusActive( boolean newActive) {
        active = newActive;
    }
    public void setStatusAcknowledged( boolean newAcknowledged) {
        acknowledged = newAcknowledged;
    }
    public void setStatusHighlighted( boolean newHighlighted) {
        highlighted = newHighlighted;
    }
    public void setStatusMasked( boolean newMasked) {
        masked = newMasked;
    }
    public void setStatusTerminated( boolean newTerminated) {
        terminated = newTerminated;
    }
    public boolean isStatusActiveEnabled() {
        if ( windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO)
            return false;
        else
            return true;
    }
    public boolean isStatusAcknowledgedEnabled() {
        if ( windowMode == Constants.SEARCH_WINDOW_MODE_ARCHIVE_SEARCH || windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO)
            return false;
        else
            return true;
    }
    public boolean isStatusHighlightedEnabled() {
        if ( windowMode == Constants.SEARCH_WINDOW_MODE_ARCHIVE_SEARCH || windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO)
            return false;
        else
            return true;
    }
    public boolean isStatusMaskedEnabled() {
        if ( windowMode == Constants.SEARCH_WINDOW_MODE_ARCHIVE_SEARCH || windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO)
            return false;
        else
            return true;
    }
    public boolean isStatusTerminatedEnabled() {
        if ( windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO)
            return false;
        else
            return true;
    }
    
    // SORT OPTION FIELDS
    
    public int getSortState() {
        if (windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO)
            return SORT_NONE;
        else
            return sortState;
    }
    public void setSortState(int newState){
        sortState = newState;
    }
    public boolean isSortByPriorityEnabled() {
        if (windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO)
            return false;
        else
            return true;
    }
    public boolean isSortByTimeEnabled() {
        if (windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO)
            return false;
        else
            return true;
    }
    public boolean isSortGroupNTEnabled() {
        if (windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO || windowMode == Constants.SEARCH_WINDOW_MODE_SEARCH_ACTIVE_LIST)
            return false;
        else
            return true;
    }
    public boolean isSortGroupNTByPriorityEnabled() {
        if (windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO || windowMode == Constants.SEARCH_WINDOW_MODE_SEARCH_ACTIVE_LIST)
            return false;
        else
            return true;
    }
    
    // CAPE Host Name fields
    
    public int getCAPHostNameState() {
        return CAPHostNameState;
    }
    public void setCAPHostNameState(int newState){
        CAPHostNameState = newState;
    }
    public String getCAPHostNameEquals() {
        return CAPHostNameEquals;
    }
    public void setCAPHostNameEquals( String newEquals ){
        CAPHostNameEquals = newEquals;
    }
    public boolean isCAPHostNameEqualsEnabled() {
        if (windowMode == Constants.SEARCH_WINDOW_MODE_GET_ALARMINFO )
            return false;
        else
            return true;
    }
    
    /**
     * Class represents setting of radio button group used to display
     * state of alarm descriptor ( FF, FM, FC or system name, identif, problem descr.)
     */
    private class AlarmIdentifierField {
        private int windowMode;
        private int state; // what is selected (all, equals or contains)
        private String contains;
        
        public AlarmIdentifierField( int windowMode ) {
            this(windowMode, ID_ALL, "");
        }
        public AlarmIdentifierField( int windowMode, int initState, String contains) {
            this.windowMode = windowMode;
            this.state = initState;
            this.contains = contains;
        }
        public int getState() {
            return state;
        }
        public void setState(int newState){
            state = newState;
        }
        public int getWindowMode() {
            return windowMode;
        }
        public void setWindowMode( int newWindowMode ) {
            windowMode = newWindowMode;
        }
        public String getContains() {
            return contains;
        }
        public void setContains( String newContains ){
            contains = newContains;
        }
    }
}
