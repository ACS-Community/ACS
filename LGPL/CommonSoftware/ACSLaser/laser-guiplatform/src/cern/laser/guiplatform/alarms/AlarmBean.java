/*
 * AlarmBean.java
 *
 * Created on May 16, 2003, 2:51 PM
 */

package cern.laser.guiplatform.alarms;

import java.awt.Color;
import java.awt.Font;
import java.awt.Image;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;
import org.openide.util.actions.SystemAction;

import alma.acs.container.ContainerServicesBase;

import cern.laser.client.LaserException;
import cern.laser.client.data.Alarm;
import cern.laser.client.data.Location;
import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;
import cern.laser.client.data.Triplet;
import cern.laser.client.services.browsing.AlarmBrowsingHandler;
import cern.laser.console.CommentedAlarm;
import cern.laser.guiplatform.actions.alarms.AcknowledgeAction;
import cern.laser.guiplatform.actions.alarms.DetailsAction;
import cern.laser.guiplatform.actions.alarms.HighlightAction;
import cern.laser.guiplatform.actions.alarms.HighlightedAndKlaxonAction;
import cern.laser.guiplatform.actions.alarms.InhibitAction;
import cern.laser.guiplatform.actions.alarms.MaskAction;
import cern.laser.guiplatform.actions.alarms.ShowActiveMultiplicityChildrenAction;
import cern.laser.guiplatform.actions.alarms.ShowActiveNodeChildrenAction;
import cern.laser.guiplatform.actions.alarms.ShowHelpUrlInBrowserAction;
import cern.laser.guiplatform.actions.alarms.ShowMultiplicityChildrenAction;
import cern.laser.guiplatform.actions.alarms.ShowNodeChildrenAction;
import cern.laser.guiplatform.actions.alarms.ShowTimestampsAction;
import cern.laser.guiplatform.actions.alarms.UnacknowledgeAction;
import cern.laser.guiplatform.actions.alarms.UnhighlightAction;
import cern.laser.guiplatform.actions.alarms.UnhighlightedAndKlaxonAction;
import cern.laser.guiplatform.actions.alarms.UninhibitAction;
import cern.laser.guiplatform.actions.alarms.UnmaskAction;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.DateTimeUtils;
import cern.laser.guiplatform.util.LabelValueBean;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.source.alarmsysteminterface.FaultState;


/**
 * Adapater class wrapping <code>Alarm</code>
 *
 * @author  pawlowsk
 */
public class AlarmBean implements PropertyChangeListener, Cloneable, Comparable {
    
    /** logger */
    private static final Logger LOGGER =
    LogFactory.getLogger(AlarmBean.class.getName());
    
    
    /** alarm console timestamp */
    //private Date currDate = new Date();
    protected String consoleTimeStamp = null;
    
    protected PropertyChangeListener propertyListener = null;
    private PropertyChangeSupport changes = new PropertyChangeSupport(this);
    
    /** node actions */
    private static String [] NODE_ACTIONS = new String[] {
        UnacknowledgeAction.class.getName(),
        AcknowledgeAction.class.getName(),
        null,
        HighlightAction.class.getName(),
        UnhighlightAction.class.getName(),
        null,
        InhibitAction.class.getName(),
        UninhibitAction.class.getName(),
        null,
        MaskAction.class.getName(),
        UnmaskAction.class.getName(),
        null,
        HighlightedAndKlaxonAction.class.getName(),
        UnhighlightedAndKlaxonAction.class.getName(),
        null,
        DetailsAction.class.getName(),
        null,
        ShowTimestampsAction.class.getName(),
        null,
        ShowMultiplicityChildrenAction.class.getName(),
        ShowNodeChildrenAction.class.getName(),
        null,
        ShowHelpUrlInBrowserAction.class.getName(),
    };
    protected String [] nodeActions = null;
    
    /** instant node actions */
    protected static final String [] INSTANT_NODE_ACTIONS = new String [] {
        DetailsAction.class.getName()
    };
    protected String [] instantNodeActions = null;
    
    /** default action name */
    //private final static String DEFAULT_ACTION_NAME = DetailsAction.class.getName();
    protected final static String DEFAULT_ACTION_NAME = null;
    
    protected String defaultActionName = null;
    
    
    /** foregournd */
    protected Color foregroundColor = Color.black;
    /** background */
    protected Color backgroundColor = new Color(70, 92, 113);
    
    
    /** this indicates whether alarm is on active list */
    protected boolean isAlarmNodeActive = true;
    
    //    /** this indicate whether alarm is actvie or not, even if is
    //     * on actvie iist, (reduction mechanism --- alarm can be on active
    //     * list but can be displayed as terminated)
    //     */
    
    
    
    /** this means that alarm in inhibited and is on inhibit list
     *(inhibit explorer)
     */
    protected boolean isAlarmNodeInhibited = false;
    
    /** alarm in masked and is on masked list (masked explorer) */
    protected boolean isAlarmNodeMasked = false;
    
    /** */
    protected boolean isAlarmNodeHighlighted = false;
    
    /** used to enabling or disabling appropriate actions,
     * Auto highlighted */
    protected boolean isAlarmNodeHighlightedAndKlaxon = false;
    
    /** alarm is acknowledged */
    protected boolean isAlarmNodeAcknowledged = false;
    
    /** this value indicates wheter alarm should be only highlighted or
     * should have klaxon or maybe both
     *
     * TODO: reimplement this use AutKlaxon and AutoHighlighed lists
     */
    protected int highlitedOrKlaxonOrBoth = AlarmConstants.HIGHLIGHTED_ONLY;
    
    
    /** alarm is instant and is on insttant list (instant explorer) */
    protected boolean isAlarmNodeInstant = false;
    
    /** alarm is new */
    protected boolean isNew = true;
    
    
    /** info mode when alarm displayed on lists like "alarm by category",
     * "search list", etc
     */
    protected boolean infoMode = false;
    
    /** this variable should be set just after creating new AlarmBean object,
     * and indicates that this alarm is on highlighted list, used when alarm is on
     * highlighted list
     */
    protected boolean isOnHighlightedList = false;
    
    /** CommentedAlarm     */
    protected CommentedAlarm commentedAlarm = null;
    
    /** Creates new AlarmAdapter from an Alarm object
     */
    public AlarmBean(CommentedAlarm commentedAlarm) {
        consoleTimeStamp = DateTimeUtils.getCurrentTimestamp();
        this.commentedAlarm = commentedAlarm;
        changeBackgroundForeground();
        nodeActions = NODE_ACTIONS;
        instantNodeActions = INSTANT_NODE_ACTIONS;
        defaultActionName = DEFAULT_ACTION_NAME;
        //this.addPropertyChangeListener(new ListSelectionListener());
    }
    
    /**
     * This constructor should be used only for changing column names
     * setTableColumns(Object bean, String[] propertyNames)
     *
     * Constructor made only because of GP ListTableExplorer.setTableColumns
     * method.
     */
    public AlarmBean() {
        consoleTimeStamp = "";
    }
    
    /**
     * This method is used by AlarmContainer when new Commnet shoud be set
     * (inhibit, mask, etc)
     *
     * @return CommentedAlarm object, which is used to build this
     *          AlarmBean object or null if AlarmBean() constructor was used
     */
    public CommentedAlarm getCommentedAlarm() {
        return commentedAlarm;
    }
    
    /**
     */
    public void setCommentedAlarm(CommentedAlarm newCommentedAlarm) {
        consoleTimeStamp = DateTimeUtils.getCurrentTimestamp();
        this.commentedAlarm = newCommentedAlarm;

        changeBackgroundForeground();
        changes.firePropertyChange("name", null, getName());
        //fireNamePropertyChange(getName());
    }
    
    //
    // ------- methods used by actions and alarm container -----------------
    //
    /**
     * @return indicates whether this Node is on active list
     */
    public boolean isAlarmNodeActive() {
        return isAlarmNodeActive;
    }
    /** This method sets isAlarmNodeActive property
     * param flag true or false
     */
    public void setIsAlarmNodeActive(/*boolean flag*/) {
        isAlarmNodeActive = true;//flag;
        isAlarmNodeInhibited = isAlarmNodeMasked =
        isAlarmNodeInstant = !isAlarmNodeActive;
        
        //isCloned = false;
        changeBackgroundForeground();
    }
    
    /** */
    public boolean isAlarmNodeInhibited() {
        return isAlarmNodeInhibited;
    }
    /**
     * @return true if AlarNode is inhibited. otherwise false
     */
    public void setIsAlarmNodeInhibited(boolean flag) {
        isAlarmNodeInhibited = flag;
        if ( flag )
            isAlarmNodeMasked = isAlarmNodeActive = isAlarmNodeInstant =
            !isAlarmNodeInhibited;
        else
            setIsAlarmNodeActive(/*true*/);
        
        changeBackgroundForeground();
    }
    
    /**
     * @return true if AlarNode is masked. otherwise false
     */
    public boolean isAlarmNodeMasked() {
        return isAlarmNodeMasked;
    }
    /** */
    public void setIsAlarmNodeMasked(boolean flag) {
        isAlarmNodeMasked = flag;
        if ( flag )
            isAlarmNodeInhibited = isAlarmNodeActive = isAlarmNodeInstant =
            !isAlarmNodeMasked;
        else
            setIsAlarmNodeActive(/*true*/);
        
        changeBackgroundForeground();
    }
    
    /**
     * @return true if AlarNode is highlited. otherwise false
     */
    public boolean isAlarmNodeHighlighted() {
        return isAlarmNodeHighlighted;
    }
    /** */
    public void setIsAlarmNodeHighlighted(boolean flag) {
        
        isAlarmNodeHighlighted = flag;
        changeBackgroundForeground();
        // fire property change
        // ### firePropertyChange(super.NAME_PROPERTY_NAME, null, getName());
        changes.firePropertyChange("isAlarmNodeHighlighted", null, new Boolean(flag));
        
    }
    
    /**
     * @return true if AlarmNode is on highlithed list
     */
    public boolean isAlarmNodeHighlightedAndKlaxon() {
        return isAlarmNodeHighlightedAndKlaxon;
    }
    
    /** */
    public void setIsAlarmNodeHighlightedAndKlaxon(boolean flag) {
        isAlarmNodeHighlightedAndKlaxon = flag;
        changeBackgroundForeground();
        changes.firePropertyChange("isAlarmNodeHighlightedAndKlaxon", null, new Boolean(flag));
    }
    
    /**
     * this metod wheter alarm should be only highlighted or
     * should have klaxon or maybe both
     *
     * @depracated should not be used, this should be check on AutoKlaxon list
     */
    public int getHighlightedOrKlaxon() {
        return highlitedOrKlaxonOrBoth;
    }
    
    /**
     * Sets flag, which indicates whether, this alarm should be only
     * highlighted, or should a klaxon or maybe both
     * @param highlightedOrKlaxon AlarmConstans.HIGHLIGHTED_ONLY or
     *           AlarmConstans.KLAXON_ONLY or
     *           AlarmConstans.HIGHLIGHTED_AND_KLAXON
     */
    public void setHighlightedOrKlaxon(final int highlightedOrKlaxon) {
        // TODO: check if highlightedOrKlaxon is OK
        highlitedOrKlaxonOrBoth = highlightedOrKlaxon;
        changeBackgroundForeground();
    }
    
    
    /**
     */
    public void setIsAlarmNodeAcknowledged(boolean flag) {
        LOGGER.debug("setIsAlarmNodeAcknowledged: " + flag);
        isAlarmNodeAcknowledged = flag;
        //firePropertyChange(super.NAME_PROPERTY_NAME, null, getName());
        // ### firePropertyChange(super.NODE_ICON_PROPERTY_NAME, null, getNodeIcon());
        changeBackgroundForeground();
        changes.firePropertyChange("isAlarmNodeAcknowledged", null, new Boolean(flag));
    }
    /**
     */
    public boolean isAlarmNodeAcknowledged() {
        return isAlarmNodeAcknowledged;
    }
    
    /**
     * @return true if AlarNode is instant, otherwise false
     */
    public boolean isAlarmNodeInstant() {
        return isAlarmNodeInstant;
    }
    
    /** */
    public void setIsAlarmNodeInstant(boolean flag) {
        isAlarmNodeInstant = flag;
        if ( flag )
            isAlarmNodeInhibited = isAlarmNodeActive = isAlarmNodeMasked =
            !isAlarmNodeInstant;
        else
            setIsAlarmNodeActive(/*true*/);
    }
    
    /** */
    public boolean isOnHighlightedList() {
        return isOnHighlightedList ;
    }
    /** */
    public void setIsOnHighlightedList(boolean flag) {
        isOnHighlightedList = flag;
        changeBackgroundForeground();
    }
    
    /** */
    public boolean isNew() {
        return isNew;
    }
    
    /** */
    public void setIsNew(boolean flag) {
        isNew = flag;        
        changeBackgroundForeground();
        changes.firePropertyChange("nodeIcon", null, getNodeIcon());
    }
    
    public void setInfoMode(boolean flag) {
        infoMode = flag;
    }
    public boolean infoMode() {
        return infoMode;
    }
    
    
    
    //
    // -- methods from Alarm --------------------------------
    //
    /**
     * @return the private identifier as an int
     */
    public String getAlarmId() {
        return commentedAlarm.getAlarm().getAlarmId();
    }
    
    /*********************** static info ************************************/
    
    /** getter for the priority of the alarm
     * @return the priority of the alarm
     */
    public Integer getPriority() {
        return commentedAlarm.getAlarm().getPriority();
    }
    
    /** getter for the categories to which the alarm belongs
     * @return a collection containing the names of the categories to which commentedAlarm.getAlarm(). alarm belongs
     */
    public Collection getCategories() {
        return commentedAlarm.getAlarm().getCategories();
    }
    
    /** getter for the responsible person for this equipement
     * @return the name of the repsonsible person from the static info
     */
    public ResponsiblePerson getResponsiblePerson() {
        return commentedAlarm.getAlarm().getResponsiblePerson();
    }
    
    /** getter for the location of the equipment
     * @return the location of the equipment
     */
    public Location getLocation() {
        return commentedAlarm.getAlarm().getLocation();
    }
    
    /** getter for the problem description
     * @return the description of the problem from static info
     */
    public String getProblemDescription() {
        return getPrefix()               + " "
        + commentedAlarm.getAlarm().getProblemDescription() + " "
        + getSuffix();
    }
    
    /** getter for the alarm source
     * @return the alarm source from static info
     */
    public Source getSource() {
        return commentedAlarm.getAlarm().getSource();
    }
    
    
    /** getter for the alarm source name
     * @return the alarm source name
     */
    public String getSourceName() {
        return commentedAlarm.getAlarm().getSource().getName();
    }
    
    /************************** Identifier ************************************/
    
    /** getter for the fault family
     * @return the "fault family" of the alarm (as defined in the LEP alarm system)
     */
    public String getFaultFamily() {
        return commentedAlarm.getAlarm().getTriplet().getFaultFamily();
    }
    
    /** getter for the fault Member
     * @return the "fault member" of the alarm (as defined in the LEP alarm system)
     */
    public String getFaultMember() {
        return commentedAlarm.getAlarm().getTriplet().getFaultMember();
    }
    
    /** getter for the fault code
     * @return the "fault code" of the alarm (as defined in the LEP alarm system)
     */
    public Integer getFaultCode() {
        return commentedAlarm.getAlarm().getTriplet().getFaultCode();
    }
    
    /************************** Dynamic Info **********************************/
    
    /**
     * @return Timestamp from the DynamicInfo class
     */
    public String getTimestamp() {
        return commentedAlarm.getAlarm().getStatus().getSystemTimestamp().toString();
    }
    
    /**
     * @return SourceTimestamp from the DynamicInfo class
     */
    public String getSourceTimestamp() {
        return commentedAlarm.getAlarm().getStatus().getSourceTimestamp().toString();
    }
    /**
     * @return UserTimestamp
     */
    public String getUserTimestamp() {
        return commentedAlarm.getAlarm().getStatus().getUserTimestamp().toString();
    }
    /**
     * @return Timestamp used for sorting
     */
    public Timestamp getTimestampForSorting() {
        return commentedAlarm.getAlarm().getStatus().getUserTimestamp();
    }
    
    /**
     * @return source name from the DynamicInfo class
     */
    public String getSourceHostName() {
        return commentedAlarm.getAlarm().getStatus().getSourceHostname();
    }
    
    /**
     * @return UserData from the DynamicInfo class
     */
    public String getUserData() {
        return commentedAlarm.getAlarm().getStatus().getUserProperties().toString();
    }
    
    /**
     * @return alarm identifier
     */
    public String getIdentifier() {
        return commentedAlarm.getAlarm().getIdentifier();
    }
    
    /**
     * @return system name
     */
    public String getSystemName() {
        return commentedAlarm.getAlarm().getSystemName();
    }
    
    /************************** Status Info **********************************/
    
    /**
     * This is bussines method
     * @return isActive field from the StatusInfo class
     */
    public boolean isActive() {
        
        boolean active = true;
        
        boolean reducedMaskedSet = false;
        
        
        // this should be rewritten
        if ( AppRegister.getInstance().getSelection() != null )
            reducedMaskedSet =
            AppRegister.getInstance().getSelection().getReducedMaskedSelection();
        
        boolean isMasked = commentedAlarm.getAlarm().getStatus().isMasked();
        boolean isReduced = commentedAlarm.getAlarm().getStatus().isReduced();
        boolean isActive = commentedAlarm.getAlarm().getStatus().isActive();
        
        //return commentedAlarm.getAlarm().getStatus().isActive();
        if ( isActive )  {
            if ( reducedMaskedSet && (isMasked || isReduced) )
                active = false;
        } else {
            active = false;
        }
        return active;
        
    }
    
    /** debug method used to dispaly status on the console */
    public boolean getIsActive() {
        return isActive();
    }
    
    /**
     * @return isMasked field from the StatusInfo class
     */
    public boolean isMasked() {
        return commentedAlarm.getAlarm().getStatus().isMasked();
    }
    
    /** Tests if the alarm is a node child
     * @return boolean
     */
    public boolean isNodeChild() {
        return commentedAlarm.getAlarm().isNodeChild();
    }
    
    /** Tests if the alarm is a node parent
     * @return boolean
     */
    public boolean isNodeParent() {
        return commentedAlarm.getAlarm().isNodeParent();
    }
    
    /** Tests if a node reduction is applied on this alarm
     * @return boolean
     */
    public boolean isNodeReduced() {
        return commentedAlarm.getAlarm().getStatus().isReduced();
    }
    
    
    /**
     * @return isMultiplicityParent field from the StatusInfo class
     */
    public boolean isMultiplicityParent() {
        return commentedAlarm.getAlarm().isMultiplicityParent();
    }
    
    /**
     * @return isMultiplicityChild field from the StatusInfo class
     */
    public boolean isMultiplicityChild() {
        return commentedAlarm.getAlarm().isMultiplicityChild();
    }
    
    /**
     * @return isMaintenanceMasked field from the StatusInfo class
     */
    /*
    public boolean isMaintenanceMasked() {
        return commentedAlarm.getAlarm().isMaintenanceMasked();
    }
     */
    /**
     * @return isModeMasked field from the StatusInfo class
     */
    /*
    public boolean isModeMasked() {
        return commentedAlarm.getAlarm().isModeMasked();
    }
     */
    /**
     * @return isReduced field from the StatusInfo class
     */
    public boolean isReduced() {
        return commentedAlarm.getAlarm().getStatus().isReduced();
    }
    
    /**
     * @return dynamic properties as <code>List</code> with <code>LabelValueBean</code>
     *          objects
     */
    public List getTimestamps() {
        List timestamps = new ArrayList();
        // time stamps should be moved to another action
        
        // user timestamp
        timestamps.add(new LabelValueBean(
        NbBundle.getMessage(
        AlarmBean.class,
        "ALARM_USER_TIMESTAMP_dispaly_name"),
        commentedAlarm.getAlarm().getStatus().getUserTimestamp().toString()));
        
        // SOURCE TIMESTAMP
        timestamps.add(new LabelValueBean(
        NbBundle.getMessage(
        AlarmBean.class,
        "ALARM_API_TIMESTAMP_dispaly_name"),
        commentedAlarm.getAlarm().getStatus().getSourceTimestamp().toString()));
        
        // business layer timestamp
        timestamps.add(new LabelValueBean(
        NbBundle.getMessage(
        AlarmBean.class,
        "ALARM_BUSINESS_LAYER_TIMESTAMP_dispaly_name"),
        commentedAlarm.getAlarm().getStatus().getSystemTimestamp().toString()));
        
        // alarm console timestamp
        timestamps.add(new LabelValueBean(
        NbBundle.getMessage(
        AlarmBean.class,
        "ALARM_CONSOLE_TIMESTAMP_dispaly_name"),
        consoleTimeStamp));
        
        // small test
        LOGGER.debug("User="+
        commentedAlarm.getAlarm().getStatus().getUserTimestamp().toString()+
        " nanos=" +
        commentedAlarm.getAlarm().getStatus().getUserTimestamp().getNanos()
        );
        LOGGER.debug("Source="+
        commentedAlarm.getAlarm().getStatus().getSourceTimestamp().toString()+
        " nanos=" +
        commentedAlarm.getAlarm().getStatus().getSourceTimestamp().getNanos()
        );
        LOGGER.debug("System="+
        commentedAlarm.getAlarm().getStatus().getSystemTimestamp().toString()+
        " nanos=" +
        commentedAlarm.getAlarm().getStatus().getSystemTimestamp().getNanos()
        );
        return timestamps;
    }
    
    /**
     * @param withEmptyFields indicates whether all fileds should be returned
     *       or only not empty
     * @return dynamic properties as <code>List</code> with <code>LabelValueBean</code>
     *          objects
     */
    public List getDynamicInfo(boolean withEmptyFields) {
        List dynProps = new ArrayList();
        // fault descriptor never empty
        //dynProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
        //                                    "ALARM_FAULT_DESCRIPTOR_dispaly_name"),
        //                                commentedAlarm.getAlarm().getStatus().isActive() ? "T" : "N"));
        
        // prefix suffix
        //addToList(dynProps,
        //          NbBundle.getMessage(AlarmBean.class,
        //                              "ALARM_PREFIX_SUFFIX_dispaly_name"),
        //          getPrefix() + "" + getSuffix(),
        //          withEmptyFields);
        
        // is active  -- not good idea, there can be different informations
        // alarm can be not active but on details panel might be displayed as
        // active and vice versa
        dynProps.add(
        new LabelValueBean(
        NbBundle.getMessage(AlarmBean.class,
        "ALARM_ACTIVE_dispaly_name"),
        commentedAlarm.getAlarm().getStatus().isActive() ? "Yes" : "No"
        )
        );
        
        // source host
        // alarm.getStatus().getSourceHostname();
        addToList(dynProps,
        //NbBundle.getMessage(AlarmBean.class,
        //                    "ALARM_PREFIX_SUFFIX_dispaly_name"),
        "Source hostname",       // this should be in Bundle.properites file
        commentedAlarm.getAlarm().getStatus().getSourceHostname(),
        withEmptyFields);
        
        
        
        // dynamic user properties
        Properties properties = commentedAlarm.getAlarm().getStatus().getUserProperties();
        
        for (Enumeration e = properties.propertyNames(); e.hasMoreElements();) {
            String key = (String) e.nextElement();
            addToList(dynProps, key, (String)properties.get(key), withEmptyFields);
        }
        
        return dynProps;
    }
    
    /**
     * @param withEmptyFields indicates whether all fileds should be returned
     *       or only not empty
     * @return static properties as <code>List</code> with <code>LabelValueBean</code>
     *          objects
     */
    public List getStaticInfo(boolean withEmptyFields) {
        List staticProps = new ArrayList();

        String urlString = null;
        java.net.URL url = null;
        
        
        //
        // -- start: alarm static info --------------------------
        //
        
        // fault family never empty
        staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
        "ALARM_FAULT_FAMILY_dispaly_name"),
        getFaultFamily()));
        // fault member never empty
        staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
        "ALARM_FAULT_MEMBER_dispaly_name"),
        getFaultMember()));
        
        // fault code never empty
        staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
        "ALARM_FAULT_CODE_dispaly_name"),
        getFaultCode().toString()));
        
        
        // installation concerned
        //staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
        //                                    "ALARM_INSTALLATION_CONCERNED_dispaly_name"),
        //                                commentedAlarm.getAlarm().getLocation().getInstallation()));
        
        // problem description
        staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
        "ALARM_PROBLEM_DESC_dispaly_name"),
        commentedAlarm.getAlarm().getProblemDescription()));
        
        // priority
        staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
        "ALARM_PRIORITY_dispaly_name"),
        commentedAlarm.getAlarm().getPriority().toString()));
        // FSGM user's source name
        /*
        staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
                                            "ALARM_FSGM_USERS_SOURCE_NAME_dispaly_name"),
                                        commentedAlarm.getAlarm().getSource().getName()));
         
        addToList(staticProps,
                  NbBundle.getMessage(AlarmBean.class,
                                      "ALARM_FSGM_USERS_SOURCE_NAME_dispaly_name"),
                  commentedAlarm.getAlarm().getSource().getName(), withEmptyFields);
         */
        
        // action to be taken
        staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
        "ALARM_ACTION_TO_BE_TAKEN_dispaly_name"),
        commentedAlarm.getAlarm().getAction()));
        // list possible causes of the problem
        staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
        "ALARM_CAUSE_OF_THE_PROBLEM_dispaly_name"),
        commentedAlarm.getAlarm().getCause()));
        
        // list possible consequences of the problem
        staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
        "ALARM_CONSEQUENCES_OF_THE_PROBLEM_dispaly_name"),
        commentedAlarm.getAlarm().getConsequence()));
        
        // help text url
        if ( (url = commentedAlarm.getAlarm().getHelpURL()) == null )
            urlString = "";
        else
            urlString = url.toExternalForm();
        
        addToList(staticProps,
        NbBundle.getMessage(AlarmBean.class,
        "ALARM_HELP_TEXT_URL_dispaly_name"),
        urlString, withEmptyFields);
        
        // piquest GSM details
        addToList(staticProps,
        NbBundle.getMessage(AlarmBean.class,
        "ALARM_PIQUET_GSM_DETAILS_dispaly_name"),
        (commentedAlarm.getAlarm().getPiquetGSM() == null ? "" :
            commentedAlarm.getAlarm().getPiquetGSM()),
            withEmptyFields);
            
            // piquet email
            addToList(staticProps,
            NbBundle.getMessage(AlarmBean.class,
            "ALARM_PIQUET_EMAIL_dispaly_name"),
            (commentedAlarm.getAlarm().getPiquetEmail() == null ? "" :
                commentedAlarm.getAlarm().getPiquetEmail()),
                withEmptyFields);
                
                
                // diagnostic program path name
                //addToList(staticProps,
                //          NbBundle.getMessage(AlarmBean.class,
                //                             "ALARM_DIAGNOSTIC_PROGRAM_PATH_NAME_dispaly_name"),
                //          "does not exists in cilent API",
                //          withEmptyFields);
                
                // parent child reduction information
                //addToList(staticProps,
                //          NbBundle.getMessage(AlarmBean.class,
                //                              "ALARM_PARENT_CHILD_REDUCTION_INFO_dispaly_name"),
                //          "I do not know where it is in client API",
                //          withEmptyFields);
                
                // reduction thresholds
                //addToList(staticProps,
                //          NbBundle.getMessage(AlarmBean.class,
                //                              "ALARM_REDUCtION_THRESHOLDS_dispaly_name"),
                //          "I do not know where it is in client API",
                //          withEmptyFields);
                
                //
                // -- end: alarm static info ---------------------------
                //
                
                //
                // -- start:  location -----
                //
                
                // site
                staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
                "ALARM_SITE_dispaly_name"),
                commentedAlarm.getAlarm().getLocation().getSite()));
                // building number
                staticProps.add(new LabelValueBean(
                NbBundle.getMessage(
                AlarmBean.class,
                "ALARM_BUILDING_NUMBER_dispaly_name"
                ),
                commentedAlarm.getAlarm().getLocation().getBuilding()
                )
                );
                
                // building mnemonic
                addToList(staticProps,
                NbBundle.getMessage(AlarmBean.class,
                "ALARM_BUILDING_MNEMONIC_dispaly_name"),
                (commentedAlarm.getAlarm().getLocation().getMnemonic() == null ? "" :
                    commentedAlarm.getAlarm().getLocation().getMnemonic()),
                    withEmptyFields);
                    
                    // floor
                    addToList(staticProps,
                    NbBundle.getMessage(AlarmBean.class, "ALARM_FLOOR_dispaly_name"),
                    checkIfNull(commentedAlarm.getAlarm().getLocation().getFloor()),
                    withEmptyFields);
                    // room
                    addToList(staticProps,
                    NbBundle.getMessage(AlarmBean.class, "ALARM_ROOM_dispaly_name"),
                    checkIfNull(commentedAlarm.getAlarm().getLocation().getRoom()),
                    withEmptyFields);
                    
                    // position
                    addToList(staticProps,
                    NbBundle.getMessage(AlarmBean.class,
                    "ALARM_POSITION_dispaly_name"),
                    (commentedAlarm.getAlarm().getLocation().getPosition() != null ?
                    commentedAlarm.getAlarm().getLocation().getPosition() : "" ),
                    withEmptyFields);
                    
                    // rack
                    //staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
                    //                                    "ALARM_RACK_dispaly_name"),
                    //                                "does not exists in API"));
                    
                    // floor coordinates
                    //staticProps.add(new LabelValueBean(NbBundle.getMessage(AlarmBean.class,
                    //                                    "ALARM_FLORR_COORDINATES_dispaly_name"),
                    //                                "does not exists in API"));
                    
                    // CERN map reference/URL
                    addToList(staticProps,
                    NbBundle.getMessage(AlarmBean.class,
                    "ALARM_MAP_REFERENCE_dispaly_name"),
                    (commentedAlarm.getAlarm().getLocation().getMap() != null ?
                    commentedAlarm.getAlarm().getLocation().getMap() : "")
                    , withEmptyFields);
                    
                    // safety zone (1-34)
                    addToList(staticProps,
                    NbBundle.getMessage(AlarmBean.class,
                    "ALARM_SAFETY_ZONE_dispaly_name"),
                    (commentedAlarm.getAlarm().getLocation().getZone() != null ?
                    commentedAlarm.getAlarm().getLocation().getZone().toString() : ""),
                    withEmptyFields);
                    
                    // ST GMA maintenance code
                    //addToList(staticProps,
                    //          NbBundle.getMessage(AlarmBean.class,
                    //                              "ALARM_ST_GMA_MAINTENANCE_CODE_dispaly_name"),
                    //          "does not exists in API",
                    //          withEmptyFields);
                    
                    //
                    // -- end: location --------------------
                    //
                    
                    //
                    // -- start: responsible person ----------------
                    //
                    

		    addToList(staticProps,
                    NbBundle.getMessage(AlarmBean.class,
                    "ALARM_RESPONSIBLE_PERSON_dispaly_name"),
                    commentedAlarm.getAlarm().getResponsiblePerson().getFirstName() +
                    commentedAlarm.getAlarm().getResponsiblePerson().getFamilyName(),
                    withEmptyFields);
                    
                    
                    // GSM details
                    addToList(staticProps,
                    NbBundle.getMessage(AlarmBean.class,
                    "ALARM_RESPONSIBLE_PERSON_GSM_DETAILS_dispaly_name"),
                    checkIfNull(commentedAlarm.getAlarm().getResponsiblePerson().getGsmNumber()),
                    withEmptyFields);
                    
                    // phone number
                    addToList(staticProps,
                    NbBundle.getMessage(AlarmBean.class,
                    "ALARM_RESPONSIBLE_PERSON_PHONE_NUMBER_dispaly_name"),
                    checkIfNull(commentedAlarm.getAlarm().getResponsiblePerson().getPhoneNumber()),
                    withEmptyFields);
                    
                    
                    // email constact
                    addToList(staticProps,
                    NbBundle.getMessage(AlarmBean.class,
                    "ALARM_RESPONSIBLE_PERSON_EMAIL_CONTACT_dispaly_name"),
                    checkIfNull(commentedAlarm.getAlarm().getResponsiblePerson().getEMail()),
                    withEmptyFields);
                    
                    //
                    // -- end: responsible person ----------------
                    //
                    
                    //
                    // -- start: source --------------------------
                    //
                    // source name
                    staticProps.add(new LabelValueBean(
                    NbBundle.getMessage(
                    AlarmBean.class,
                    "ALARM_SOURCE_NAME_dispaly_name"),
                    commentedAlarm.getAlarm().getSource().getName()));
                    // source description
                    addToList(staticProps,
                    NbBundle.getMessage(AlarmBean.class,
                    "ALARM_SOURCE_DESC_dispaly_name"),
                    (commentedAlarm.getAlarm().getSource().getDescription() == null ? "" :
                        commentedAlarm.getAlarm().getSource().getDescription()),
                        withEmptyFields);
                        
                        
                        // source responsible person
                        ResponsiblePerson sourceRespPerson =
                        commentedAlarm.getAlarm().getSource().getResponsiblePerson();
                        String sourceRespPersonName = null;
                        
                        if ( sourceRespPerson != null )
                            sourceRespPersonName  =
                            sourceRespPerson.getFirstName() +
                            sourceRespPerson.getFamilyName();
                        else
                            sourceRespPersonName = "";
                        
                        addToList(staticProps,
                        NbBundle.getMessage(AlarmBean.class,
                        "ALARM_SOURCE_RESPONSIBLE_PERSON_dispaly_name"),
                        sourceRespPersonName, withEmptyFields);
                        
                        // other details for source responsible person
                        
                        //
                        // -- end: source ----------------------------
                        //
                        
                        
                        return staticProps;
    }
    /** helper method, which adds static properites to list depending on
     * withEmptyFields value
     * @param list
     * @param key key for LabelValueBean
     * @param value  value for LabelValueBean (property value)
     * @param withEmptyFields
     */
    private void addToList(List list, String key, String value,
    boolean withEmptyFields) {
        
        if ( (value.length() > 0) || (value.length() == 0 && withEmptyFields) )
            list.add(new LabelValueBean(key, value));
        
        
    }
    
    
    /*
    public String toShortString() {
        return alarm.toShortString();
    }
     */
    
    //
    // -- method from BeanSupport --------------------------------------
    //
    public String getName() {
        //return getPrivateIdentifier().toString();
        
        // test
        if ( isAlarmNodeActive && !isOnHighlightedList ) {
            //return getUserTimestamp().substring( 0,getUserTimestamp().lastIndexOf(":")+3);
            return "";
        } else  {
            Triplet triplet = commentedAlarm.getAlarm().getTriplet();
            return triplet.getFaultFamily() + " " + triplet.getFaultMember() +
            " " + triplet.getFaultCode();
        }
    }
    
    public String getDisplayName() {
        //return getProblemDescription();
        return getName();
    }
    public String getDate() {
    	// marekm 
    	String americanStyleDate = getUserTimestamp().substring(5, 10);
    	String[] dateParts = americanStyleDate.split("-");
    	if (dateParts.length == 2)
    		return dateParts[1] + "/" + dateParts[0];
    	else
    		return americanStyleDate.replace('-','/');
    }
    public String getTime() {
        return getUserTimestamp().substring( 11,19);
    }    
    public String getNodeDefaultAction() {
        return defaultActionName;
    }
    public Image getNewIcon() {
        Image image = null;
        switch ( getPriority().intValue() ) {
            case AlarmConstants.PRIORITY_0 :
                image = Utilities.loadImage("cern/laser/guiplatform/images/alarm_gray24_24new.gif");
                break;
            case AlarmConstants.PRIORITY_1 :
                image = Utilities.loadImage("cern/laser/guiplatform/images/alarm_blue24_24new.gif");
                break;
            case AlarmConstants.PRIORITY_2 :
                image = Utilities.loadImage("cern/laser/guiplatform/images/alarm_yellow24_24new.gif");
                break;
            case AlarmConstants.PRIORITY_3 :
                image = Utilities.loadImage("cern/laser/guiplatform/images/alarm_red24_24new.gif");
                break;
        }
        if ( isNew() ){
            return image;
        }
        else {
            return null;
        }
    }
    public Image getNodeIcon() {
        return getNodeIcon(false);
    }
    public Image getNodeIcon(boolean isSelected) {
        
        Image image = null;
        switch (getPriority().intValue()) {
            case AlarmConstants.PRIORITY_0 :
                if( isSelected == true ) {
                    image = prepareIcon("cern/laser/guiplatform/images/alarm_gray24_24new.gif",
                    "cern/laser/guiplatform/images/alarm_gray24_24sel.gif");
                }
                else {
                    image = prepareIcon("cern/laser/guiplatform/images/alarm_gray24_24new.gif",
                    "cern/laser/guiplatform/images/alarm_24_24unselected.gif");
                }
                break;
            case AlarmConstants.PRIORITY_1 :
                if( isSelected == true ) {
                    image = prepareIcon("cern/laser/guiplatform/images/alarm_blue24_24new.gif",
                    "cern/laser/guiplatform/images/alarm_blue24_24sel.gif");
                }
                else {
                    image = prepareIcon("cern/laser/guiplatform/images/alarm_blue24_24new.gif",
                    "cern/laser/guiplatform/images/alarm_24_24unselected.gif");
                }
                break;
            case AlarmConstants.PRIORITY_2 :
                if( isSelected == true ) {
                    image = prepareIcon("cern/laser/guiplatform/images/alarm_yellow24_24new.gif",
                    "cern/laser/guiplatform/images/alarm_yellow24_24sel.gif");
                }
                else {
                    image = prepareIcon("cern/laser/guiplatform/images/alarm_yellow24_24new.gif",
                    "cern/laser/guiplatform/images/alarm_24_24unselected.gif");
                }
                break;
            case AlarmConstants.PRIORITY_3 :
                if( isSelected == true ) {
                    image = prepareIcon("cern/laser/guiplatform/images/alarm_red24_24new.gif",
                    "cern/laser/guiplatform/images/alarm_red24_24sel.gif");
                }
                else {
                    image = prepareIcon("cern/laser/guiplatform/images/alarm_red24_24new.gif",
                    "cern/laser/guiplatform/images/alarm_24_24unselected.gif");
                }
                break;
        }
        
        return image;
    }
    
    /**
     * @param imagePathNew
     * @param imagePathNotNew
     */
    private Image prepareIcon(String imagePathNew, String imagePathNotNew) {
        Image image = null;
        
        final String ACKNOWLEDGED_ICON_PATH = "cern/laser/guiplatform/images/dock.gif";
        final String HELP_URL_ICON_PATH = "cern/laser/guiplatform/images/url.gif";
        final String EMPYT_ICON_PATH = "cern/laser/guiplatform/images/empty_icon.png";
        final String MULTIPLICITY_PARENT_ICON_PATH = "cern/laser/guiplatform/images/multiplicity_icon.png";
        
        
        if ( commentedAlarm.getAlarm().isInstant() ||
        isOnHighlightedList || isAlarmNodeInhibited  ||
        isAlarmNodeMasked || infoMode ) {
            
            image = Utilities.loadImage(imagePathNotNew);
            //"cern/laser/guiplatform/images/alarm_gray24_24.gif");
        } else {
            //if ( isNew  ) {
            //    image = Utilities.loadImage(imagePathNew);
            //    //"cern/laser/guiplatform/images/alarm_gray24_24new.gif");
            //    if ( isAlarmNodeAcknowledged )
            //        image = Utilities.mergeImages(image,
            //        Utilities.loadImage(ACKNOWLEDGED_ICON_PATH), 16, 0);
            //    
            //} else { // not new
                image = Utilities.loadImage(imagePathNotNew);
                //"cern/laser/guiplatform/images/alarm_gray24_24.gif");
                
                //if ( isAlarmNodeAcknowledged )
                //    image = Utilities.mergeImages(image,
                //    Utilities.loadImage(ACKNOWLEDGED_ICON_PATH), 16, 0);
                if ( getCommentedAlarm().getAlarm().getHelpURL() != null )
                    image = Utilities.mergeImages(image,Utilities.loadImage(HELP_URL_ICON_PATH), 16, 0);
                
            //}
            image = Utilities.mergeImages(image, Utilities.loadImage(EMPYT_ICON_PATH), 16, 0);
            if ( commentedAlarm.getAlarm().isMultiplicityParent()  ||
            commentedAlarm.getAlarm().isNodeParent() )
                image = Utilities.mergeImages(
                Utilities.loadImage(MULTIPLICITY_PARENT_ICON_PATH),
                image, 16, 0);
            else
                image = Utilities.mergeImages(Utilities.loadImage(EMPYT_ICON_PATH),image, 16, 0);
            
        }
        
        return image;
    }
    
    
    public String[] getNodeActions() {
        if ( !commentedAlarm.getAlarm().isInstant() )
            return nodeActions;
        else
            return instantNodeActions;
    }
    
    /*
     * @deprecated should not be used
    public void setNodeActions(String [] actions) {
        this.nodeActions = actions;
    }
     */
    
    /* ###
    public PropertyInfo[] getPropertyInfo() {
        return new PropertyInfo[] {
            //new PropertyInfoSupport("hashCode", true)
            new PropertyInfoSupport("faultFamily",
            NbBundle.getMessage(Constants.class,
            "FAULT_FAMILY_displayName"),
            AlarmColoredEditor.class),
            new PropertyInfoSupport("faultMember",
            NbBundle.getMessage(Constants.class,
            "FAULT_MEMBER_displayName"),
            AlarmColoredEditor.class),
            new PropertyInfoSupport("faultCode",
            NbBundle.getMessage(Constants.class,
            "FAULT_CODE_displayName"),
            AlarmColoredEditor.class),
            new PropertyInfoSupport("priority",
            NbBundle.getMessage(Constants.class,
            "PRIORITY_displayName"),
            AlarmColoredEditor.class),
            new PropertyInfoSupport("isActive",
            NbBundle.getMessage(Constants.class,
            "ACTIVE_displayName"),
            AlarmColoredEditor.class),
            new PropertyInfoSupport("timestamp",
            NbBundle.getMessage(Constants.class,
            "TIMESTAMP_displayName"),
            AlarmColoredEditor.class),
            new PropertyInfoSupport("problemDescription",
            NbBundle.getMessage(Constants.class,
            "PROBLEM_DESCRIPTION_displayName"),
            AlarmColoredEditor.class),
            new PropertyInfoSupport("systemName",
            NbBundle.getMessage(Constants.class,
            "SYSTEM_NAME_displayName"),
            AlarmColoredEditor.class),
            new PropertyInfoSupport("identifier",
            NbBundle.getMessage(Constants.class,
            "IDENTIFIER_displayName"),
            AlarmColoredEditor.class),
            new PropertyInfoSupport("sourceName",
            NbBundle.getMessage(Constants.class,
            "SOURCE_NAME_displayName"),
            AlarmColoredEditor.class),
     
     
     
     
        };
    }
     */
    
    //
    // -- implements ColorMaster ---------------------------------------------
    //
    /**
     * @see cern.gp.beans.editors.ColorMaster#getBackgroundColor()
     */
    public Color getBackgroundColor() {
        if ( !isOnHighlightedList &&
        (isAlarmNodeHighlighted() ||
        (isAlarmNodeHighlightedAndKlaxon() &&
        (getHighlightedOrKlaxon()) == AlarmConstants.HIGHLIGHTED_ONLY ||
        getHighlightedOrKlaxon() == AlarmConstants.HIGHLIGHTED_AND_KLAXON))
        
        ) {
            return foregroundColor;
        }
        
        return backgroundColor;
    }
    
    /** from colormaster */
    //public FontDescriptor getFontDescriptor() {
    //    return new FontDescriptor("Arial",2,false,Font.PLAIN);
    //}
    /**
     * @see cern.gp.beans.editors.ColorMaster#getForegroundColor()
     */
    public Color getForegroundColor() {
        if ( !isOnHighlightedList &&
        (isAlarmNodeHighlighted() ||
        (isAlarmNodeHighlightedAndKlaxon() &&
        (getHighlightedOrKlaxon()) == AlarmConstants.HIGHLIGHTED_ONLY ||
        getHighlightedOrKlaxon() == AlarmConstants.HIGHLIGHTED_AND_KLAXON))
        
        ) {
            return backgroundColor;
        }
        
        return foregroundColor;
    }
    
    public Font getFont() {
        int fontStyle = Font.PLAIN;
        if ( isAlarmNodeAcknowledged()) {
            fontStyle = fontStyle | Font.ITALIC;
        }
        if ( isMultiplicityParent() || isNodeParent() ) {
            fontStyle = fontStyle | Font.BOLD;
        }
        return new Font(AppRegister.getInstance().getFontName(),fontStyle, AppRegister.getInstance().getFontSize());
    }
    
    /** */
    private void changeBackgroundForeground() {
        if ( isActive() || isOnHighlightedList || isAlarmNodeMasked || isAlarmNodeInhibited ) {
            switch (getPriority().intValue()) {
                case AlarmConstants.PRIORITY_0 :
                    if ( isAlarmNodeAcknowledged() )
                        foregroundColor = new Color( 177, 177, 177);
                    else
                        foregroundColor = new Color( 229, 229, 229);
                    break;
                case AlarmConstants.PRIORITY_1 :
                    if ( isAlarmNodeAcknowledged() )
                        foregroundColor = new Color( 0, 176, 176);
                    else
                        foregroundColor = new Color( 0, 255, 255);
                    break;
                case AlarmConstants.PRIORITY_2 :
                    if ( isAlarmNodeAcknowledged() )
                        foregroundColor = new Color( 193, 193, 0);
                    else
                        foregroundColor = new Color( 255, 255, 0);
                    break;
                case AlarmConstants.PRIORITY_3 :
                    if ( isAlarmNodeAcknowledged() )
                        foregroundColor = new Color( 200, 91, 91);
                    else
                        foregroundColor = new Color( 255, 106, 106);
                    break;
            }
        } else {
            foregroundColor = new Color(33, 138, 33);
        }
    }
    
    private String getPrefix() {
        return (commentedAlarm.getAlarm().getStatus().getUserProperties().getProperty(FaultState.ASI_PREFIX_PROPERTY) == null ? "" :
            commentedAlarm.getAlarm().getStatus().getUserProperties().getProperty(FaultState.ASI_PREFIX_PROPERTY));
    }
    private String getSuffix() {
        return (commentedAlarm.getAlarm().getStatus().getUserProperties().getProperty(FaultState.ASI_SUFFIX_PROPERTY) == null ? "" :
            commentedAlarm.getAlarm().getStatus().getUserProperties().getProperty(FaultState.ASI_SUFFIX_PROPERTY));
    }
    
    
    //
    // -- methods from capabilites ------------------------------------
    //
    /** show details */
    public void details(ContainerServicesBase contSvcs) {
        
        AcWindowManager.setStatusText("Show details for: " + getName() + " is running ....");
        // before displaying this window ask BL for new Alarm
        AlarmBrowsingHandler browser =  null;
        
        try  {
            browser = AlarmBrowsingHandlerFactory.getHandler(contSvcs);
            Alarm alarm = browser.getAlarmById(getAlarmId());
            
            AcWindowManager.showDetails(
            new AlarmBean(new CommentedAlarm(alarm, null)));
            
        } catch (LaserException le) {
            LOGGER.error(le, le.fillInStackTrace());
            LOGGER.error(le.getRootCause(), le.getRootCause().fillInStackTrace());
            AcWindowManager.notifyError("AlrmBrowsinHandler can't be found.\n" +
            "Can't connect to database.");
            
        }
        
        // this was in previous version
        //AcWindowManager.showDetails(this);
        AcWindowManager.setStatusText("Show details for: " + getName() + " finished");
    }
    
    //
    // -- clone method --------------------------------------------------
    //
    public Object clone() throws CloneNotSupportedException {
        AlarmBean newAlarm = (AlarmBean) super.clone();
        newAlarm.commentedAlarm = (CommentedAlarm) commentedAlarm.clone();
        //newAlarm.alarm = (Alarm) alarm.clone();
        
        return newAlarm;
    }
    
    public void propertyChange(java.beans.PropertyChangeEvent evt) {
        LOGGER.debug(getName() + " propertyChange()");
        LOGGER.debug("propertyName: " + evt.getPropertyName());
        if ( evt.getPropertyName().equals("isAlarmNodeHighlightedAndKlaxon") ) {
            isAlarmNodeHighlightedAndKlaxon = ((Boolean) evt.getNewValue()).booleanValue();
        } else if ( evt.getPropertyName().equals("isAlarmNodeHighlighted") ) {
            isAlarmNodeHighlighted = ((Boolean) evt.getNewValue()).booleanValue();
        }
    }
    
    
    //
    // -- toString methods not defined ---------------
    //
    public boolean equals(Object obj) {
        if ( !(obj instanceof AlarmBean) )
            return false;
        
        AlarmBean alarmBean = (AlarmBean) obj;
        
        return getAlarmId().equals(alarmBean.getAlarmId());
    }
    
    /** Return the object hashcode
     * @return the object hashcode
     */
    public int hashCode() {
        // hashCode() method can be introspected. If it returns null, an Exception is thrown.
        if (commentedAlarm != null && commentedAlarm.getAlarm() != null)
            return commentedAlarm.getAlarm().hashCode();
        else
            return 0;
    }
    
    
    public void registerPropertyChangeListener(PropertyChangeListener listener) {
        if ( propertyListener != null )
            removePropertyChangeListener(propertyListener);
        
        this.propertyListener = listener;
        addPropertyChangeListener(propertyListener);
    }
    
    public void addPropertyChangeListener( PropertyChangeListener listener) {
        changes.addPropertyChangeListener( listener );
    }
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        changes.removePropertyChangeListener( listener );
    }
    //
    // -- utility methods -----------------------------
    //
    /**
     * @param str string to be checked if it is null
     * @return if str is null returm empty string ""
     *          else retur str
     */
    public String checkIfNull(String str) {
        return str == null ? "" : str;
    }
    
    // returns array of actions which are added to the popup menu
    public SystemAction[] getActions() {
        /*
        String [] actionsNames = getNodeActions();
        SystemAction [] actions = new SystemAction[actionsNames.length];
         
        try {
            for(int i=0; i<actionsNames.length; i++) {
                actions[i] = SystemAction.get( Class.forName(actionsNames[i]) );
            }
            logger.debug("returned getActions = "  + actions );
            return actions;
        }
        catch( ClassNotFoundException cnfe){
            logger.debug(cnfe.getMessage() );
            return null;
        }
         */
        SystemAction [] actions = {
            SystemAction.get(UnacknowledgeAction.class),
            SystemAction.get(AcknowledgeAction.class),
            SystemAction.get(HighlightAction.class),
            SystemAction.get(UnhighlightAction.class),
            SystemAction.get(InhibitAction.class),
            SystemAction.get(UninhibitAction.class),
            SystemAction.get(MaskAction.class),
            SystemAction.get(UnmaskAction.class),
            SystemAction.get(HighlightedAndKlaxonAction.class),
            SystemAction.get(UnhighlightedAndKlaxonAction.class),
            SystemAction.get(DetailsAction.class),
            SystemAction.get(ShowTimestampsAction.class),
            SystemAction.get(ShowMultiplicityChildrenAction.class),
            SystemAction.get(ShowActiveMultiplicityChildrenAction.class),
            SystemAction.get(ShowNodeChildrenAction.class),
            SystemAction.get(ShowActiveNodeChildrenAction.class),
            SystemAction.get(ShowHelpUrlInBrowserAction.class)
        };
        return actions;
    }
    
    public int compareTo(Object o) {
        AlarmBean alarm = (AlarmBean) o;
        
        int priorityResult = alarm.getPriority().compareTo( getPriority() );
        
        if ( priorityResult == 0 ) {
            int timestampResult = alarm.getTimestampForSorting().compareTo( getTimestampForSorting() );
            if ( timestampResult  == 0) {
                return alarm.getAlarmId().compareTo( getAlarmId() );
            }
            else {
                return timestampResult ;
            }
        }
        else{
            return priorityResult;
        }
        
    }
    public void fireFontChanged() {
        changes.firePropertyChange("fontChanged", null, null);
    }
    public Color getStandardBackgroundColor() {
        return backgroundColor;
    }
    public Color getStandardForegroundColor() {
        return foregroundColor;
    }
}
