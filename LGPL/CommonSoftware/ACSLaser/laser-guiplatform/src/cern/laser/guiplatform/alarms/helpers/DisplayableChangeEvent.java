/*
 * DisplayableChangeEventAdapter.java
 *
 * Created on October 1, 2002, 5:31 PM
 */

package cern.laser.guiplatform.alarms.helpers;

import java.util.Collection;


//import cern.laser.business.alarm.data.StaticInfo;
//import cern.laser.business.alarm.data.DisplayableChangeEvent;
//import cern.laser.business.alarm.data.Identifier;
//import cern.laser.guiplatform.alarmbeans.DummyCapability;

/** 
 * This is test class. In final version should be replaced by 
 * cern.laser.business.alarm.data.DisplayableChangeEvent;
 * 
 * Adapter class wrapping DisplayableChangeEvent. Follows the JavaBeans specifications.
 * Actual properties are all the one contained in the StaticInfo, DynamicInfo & 
 * StatusInfo classes 
 *
 *
 *  @depracated should not be used
 *
 * @author  claustre
 */
public class DisplayableChangeEvent implements Cloneable{
    
    /** will have to be private. But at the moment, it must be public to set internaly
     *  some parameters (no set function must be written to make the fields not editable)
     */
    //public DisplayableChangeEvent dsplyChngEvt;
    private String name = null;
    private Integer privateIdent = null;
    private boolean isActive = true;


    private String faultFamily = null;
    private String faultMember = null;
    private int faultCode = -1;

    /** Creates new DisplayableChangeEventAdapter from an AlarmDisplayableChangeEvent*/
    //public DisplayableChangeEvent(DisplayableChangeEvent dsplyChngEvt) {
    //    this.dsplyChngEvt = dsplyChngEvt;
        //setUserData("Enter your comments");
    //}
    
    /** Creates new empty DisplayableChangeEventAdapter 
     * Should be removed as soon as the tests will be done with an external alarm
     * generator. In this case, the Dummy bean creted in CustomiseFields class should 
     * use the other constructor: DisplayableChangeEventAdapter(null).
     *
     *  WARNING: this constructor should not be used
     */
    public DisplayableChangeEvent(String name) {
        //this(name, new Integer(-100), true);
        this.name = name;
    }
    /*
    public DisplayableChangeEvent(String name, Integer privateIdent, boolean isActive) {
        //this(null);
        this.name = name;
        this.privateIdent = privateIdent;
        this.isActive = isActive;
    }
   public DisplayableChangeEvent(String name, Integer privateIdent) {
        this(name, privateIdent, true);
    } 
    */
    public DisplayableChangeEvent(String name, Integer privateIdent, 
                                    boolean isActive, String faultFamily,
                                    String faultMember, int faultCode) {
        this.name = name;
        this.privateIdent = privateIdent;
        this.isActive = isActive;
        this.faultFamily = faultFamily;
        this.faultMember = faultMember;
        this.faultCode = faultCode;
    }


    /**
     * The private identifier is the name of the node
     * @return Name
     */    
    public String getName() {
        //return getPrivateIdentifier().toString();
        return name;

    }
    
    /**
     * At the moment, the field chosen to be the "Display Name of an Alarm" is the problem description
     * @return DisplayName
     */    
    public String getDisplayName() {
        //return getProblemDescription();
        return "DisplayName";
    }
    
    /**
     * @return the private identifier as an int
     */
    //private Integer privateIdent = new Integer(100);
    public Integer getPrivateIdentifier() {
        //return dsplyChngEvt.getPrivateIdentifier();
        return privateIdent; 
    }
    
    /*********************** static info ************************************/
    
    /** getter for the priority of the alarm
     * @return the priority of the alarm
     */    
    public int priority = -1;
    public int getPriority() {
        //return dsplyChngEvt.getPriority();
        return priority;
    }
    /** this is method only for testing */
    public void setPriority(int priority) {
        this.priority = priority;
    }
    
    /** getter for the categories to which the alarm belongs
     * @return a collection containing the names of the categories to which dsplyChngEvt. alarm belongs
     */    
    public Collection getCategories() {
        //return dsplyChngEvt.getCategories();
        return null;
    }
        
    /** getter for the responsible person for this equipement
     * @return the name of the repsonsible person from the static info
     */    
    public String getResponsiblePerson() {
        //return dsplyChngEvt.getResponsiblePerson();
        return "BARTEK";
    }
    
    /** getter for the location of the equipment
     * @return the location of the equipment
     */    
    public String getLocation() {
        //return dsplyChngEvt.getLocation();
        return "936";
    }
    
    /** getter for the problem description
     * @return the description of the problem from static info
     */    
    public String getProblemDescription() {
        //return dsplyChngEvt.getProblemDescription();
        return "Description";
    }
    
    /** getter for the alarm source
     * @return the alarm source from static info
     */    
    public String getSource() {
        //return dsplyChngEvt.getSource();
        return "Source";
    }
    
    /************************** Identifier ************************************/
    
    /** getter for the fault family
     * @return the "fault family" of the alarm (as defined in the LEP alarm system)
     */    
    public String getFaultFamily() {
        //return dsplyChngEvt.getFaultFamily();
        return faultFamily;
    }
    
    /** getter for the fault Member
     * @return the "fault member" of the alarm (as defined in the LEP alarm system)
     */    
    public String getFaultMember() {
        //return dsplyChngEvt.getFaultMember();
        return faultMember;
    }
    
    /** getter for the fault code
     * @return the "fault code" of the alarm (as defined in the LEP alarm system)
     */    
    public int getFaultCode() {
        //return dsplyChngEvt.getFaultCode();
        return faultCode;
    }
        
    /************************** Dynamic Info **********************************/
        
    /**
     * @return Timestamp from the DynamicInfo class
     */    
    public long getTimestamp() {
        //return dsplyChngEvt.getTimestamp();
        return 1000;
    }
    
    /**
     * @return SourceTimestamp from the DynamicInfo class
     */    
    public long getSourceTimestamp() {
        //return dsplyChngEvt.getSourceTimestamp();
        return 1000;
    }
    
    /**
     * @return source name from the DynamicInfo class
     */    
    public String getSourceHostName() {
        //return dsplyChngEvt.getSourceHostName();
        return "SourceHostName";
    }
            
    /**
     * @return UserData from the DynamicInfo class
     */    
    public String getUserData() {
        //return dsplyChngEvt.getUserData();
        return "UserData";
    }
    
    /************************** Status Info **********************************/
        
    /**
     * @return isActive field from the StatusInfo class
     */
    public boolean isActive() {
        //return dsplyChngEvt.isActive();
        return isActive;
    }
    
    /**
     * @return isMasked field from the StatusInfo class
     */    
    private boolean isMasked = false;
    public boolean isMasked() {
        //return dsplyChngEvt.isMasked();
        return isMasked;
    }
    
    /** Tests if the alarm is a node child
     * @return boolean
     */
    public boolean isNodeChild() {
        //return dsplyChngEvt.isNodeChild();
        return true;
    }
    
    /** Tests if the alarm is a node parent
     * @return boolean
     */
    public boolean isNodeParent() {
        //return dsplyChngEvt.isNodeParent();
        return true;
    }
    
    /** Tests if a node reduction is applied on this alarm
     * @return boolean
     */    
    public boolean isNodeReduced() {
        //return dsplyChngEvt.isNodeReduced();
        return true;
    }
    
    /**
     * @return isMultiplicityReduced field from the StatusInfo class
     */    
    public boolean isMultiplicityReduced() {
        //return dsplyChngEvt.isMultiplicityReduced();
        return true;
    }
    
    /**
     * @return isMultiplicityParent field from the StatusInfo class
     */
    public boolean isMultiplicityParent() {
        //return dsplyChngEvt.isMultiplicityParent();
        return true;
    }
        
    /**
     * @return isMultiplicityChild field from the StatusInfo class
     */
    public boolean isMultiplicityChild() {
        //return dsplyChngEvt.isMultiplicityChild();
        return true;
    }
    
    /**
     * @return isMaintenanceMasked field from the StatusInfo class
     */    
    public boolean isMaintenanceMasked() {
        //jreturn dsplyChngEvt.isMaintenanceMasked();
        return true;
    }
    
    /**
     * @return isModeMasked field from the StatusInfo class
     */    
    public boolean isModeMasked() {
        //return dsplyChngEvt.isModeMasked();
        return true;
    }
    
    /**
     * @return isReduced field from the StatusInfo class
     */    
    public boolean isReduced() {
        //return dsplyChngEvt.isReduced();
        return true;
    }
   
    /** Checks for equality
     * @return boolean: 1 if the object is equal, 0 else
     */
    //public boolean equals(Object o) {
    //    return dsplyChngEvt.equals(o);
    //}
    
    public String toShortString() {
        //return dsplyChngEvt.toShortString();
        return "toString";
    }
    
    
    /*
    public Image getNodeIcon() {
        switch(getPriority()) {
            case 0 : return getNodeIconFromPathname("christmas_white.gif");
            case 1 : return getNodeIconFromPathname("christmas_green.gif");
            case 2 : return getNodeIconFromPathname("christmas_blue.gif");
            case 3 : return getNodeIconFromPathname("christmas_red.gif");
            default : return null;
        }
    }
    */
    /*
    public String[] getNodeActions() {
        return new String[] {"cern.laser.guiplatform.alarmbeans.DummyAction",
                             null,
                             "cern.gp.actions.OpenLocalExplorerAction"};
    }
    
    public PropertyInfo[] getPropertyInfo() {
        return new PropertyInfo[] {
            new PropertyInfoSupport("hashCode", true)
        };
    }
    */
    /******  toString(), hashCode() and toString methods not defined  ********/
   public String toString() {
       return "name: " + name + " privIdent: " + privateIdent  + " FF: " + 
            faultFamily + " FM: " + faultMember + " FC: " + faultCode + 
            " isActive " + isActive;
   } 
   /** Return the object hashcode
     * @return the object hashcode
     */
    /*
    public int hashCode() {
        // hashCode() method can be introspected. If it returns null, an Exception is thrown.
        if (dsplyChngEvt != null) 
            return dsplyChngEvt.hashCode();
        else 
            return 0;
    }
    */ 
   public Object clone() throws CloneNotSupportedException {
       DisplayableChangeEvent newEvent = (DisplayableChangeEvent)
           super.clone();
       return newEvent;
   }
}
