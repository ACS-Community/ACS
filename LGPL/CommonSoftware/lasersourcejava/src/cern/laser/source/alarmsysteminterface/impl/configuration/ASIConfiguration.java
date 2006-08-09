/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.6</a>, using an XML
 * Schema.
 * $Id: ASIConfiguration.java,v 1.1 2006/08/09 22:41:22 sharring Exp $
 */

package cern.laser.source.alarmsysteminterface.impl.configuration;

  //---------------------------------/
 //- Imported classes and packages -/
//---------------------------------/

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;
import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;
import org.xml.sax.ContentHandler;

/**
 * Class ASIConfiguration.
 * 
 * @version $Revision: 1.1 $ $Date: 2006/08/09 22:41:22 $
 */
public class ASIConfiguration implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    /**
     * Field _ASIVersion
     */
    private java.lang.String _ASIVersion;

    /**
     * Field _alarmsTopic
     */
    private java.lang.String _alarmsTopic;

    /**
     * Field _backupDeliveryMode
     */
    private int _backupDeliveryMode;

    /**
     * keeps track of state for field: _backupDeliveryMode
     */
    private boolean _has_backupDeliveryMode;

    /**
     * Field _backupPriority
     */
    private int _backupPriority;

    /**
     * keeps track of state for field: _backupPriority
     */
    private boolean _has_backupPriority;

    /**
     * Field _backupTimeToLive
     */
    private long _backupTimeToLive;

    /**
     * keeps track of state for field: _backupTimeToLive
     */
    private boolean _has_backupTimeToLive;

    /**
     * Field _changesDeliveryMode
     */
    private int _changesDeliveryMode;

    /**
     * keeps track of state for field: _changesDeliveryMode
     */
    private boolean _has_changesDeliveryMode;

    /**
     * Field _changesPriority
     */
    private int _changesPriority;

    /**
     * keeps track of state for field: _changesPriority
     */
    private boolean _has_changesPriority;

    /**
     * Field _changesTimeToLive
     */
    private long _changesTimeToLive;

    /**
     * keeps track of state for field: _changesTimeToLive
     */
    private boolean _has_changesTimeToLive;

    /**
     * Field _channelPoolSize
     */
    private int _channelPoolSize;

    /**
     * keeps track of state for field: _channelPoolSize
     */
    private boolean _has_channelPoolSize;

    /**
     * Field _sourceNameProperty
     */
    private java.lang.String _sourceNameProperty;

    /**
     * Field _sourceHostnameProperty
     */
    private java.lang.String _sourceHostnameProperty;

    /**
     * Field _backupProperty
     */
    private java.lang.String _backupProperty;

    /**
     * Field _alarmsNumberProperty
     */
    private java.lang.String _alarmsNumberProperty;

    /**
     * Field _channelProperty
     */
    private java.lang.String _channelProperty;


      //----------------/
     //- Constructors -/
    //----------------/

    public ASIConfiguration() {
        super();
    } //-- cern.laser.source.alarmsysteminterface.impl.configuration.ASIConfiguration()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Method deleteBackupDeliveryMode
     * 
     */
    public void deleteBackupDeliveryMode()
    {
        this._has_backupDeliveryMode= false;
    } //-- void deleteBackupDeliveryMode() 

    /**
     * Method deleteBackupPriority
     * 
     */
    public void deleteBackupPriority()
    {
        this._has_backupPriority= false;
    } //-- void deleteBackupPriority() 

    /**
     * Method deleteBackupTimeToLive
     * 
     */
    public void deleteBackupTimeToLive()
    {
        this._has_backupTimeToLive= false;
    } //-- void deleteBackupTimeToLive() 

    /**
     * Method deleteChangesDeliveryMode
     * 
     */
    public void deleteChangesDeliveryMode()
    {
        this._has_changesDeliveryMode= false;
    } //-- void deleteChangesDeliveryMode() 

    /**
     * Method deleteChangesPriority
     * 
     */
    public void deleteChangesPriority()
    {
        this._has_changesPriority= false;
    } //-- void deleteChangesPriority() 

    /**
     * Method deleteChangesTimeToLive
     * 
     */
    public void deleteChangesTimeToLive()
    {
        this._has_changesTimeToLive= false;
    } //-- void deleteChangesTimeToLive() 

    /**
     * Method deleteChannelPoolSize
     * 
     */
    public void deleteChannelPoolSize()
    {
        this._has_channelPoolSize= false;
    } //-- void deleteChannelPoolSize() 

    /**
     * Returns the value of field 'ASIVersion'.
     * 
     * @return String
     * @return the value of field 'ASIVersion'.
     */
    public java.lang.String getASIVersion()
    {
        return this._ASIVersion;
    } //-- java.lang.String getASIVersion() 

    /**
     * Returns the value of field 'alarmsNumberProperty'.
     * 
     * @return String
     * @return the value of field 'alarmsNumberProperty'.
     */
    public java.lang.String getAlarmsNumberProperty()
    {
        return this._alarmsNumberProperty;
    } //-- java.lang.String getAlarmsNumberProperty() 

    /**
     * Returns the value of field 'alarmsTopic'.
     * 
     * @return String
     * @return the value of field 'alarmsTopic'.
     */
    public java.lang.String getAlarmsTopic()
    {
        return this._alarmsTopic;
    } //-- java.lang.String getAlarmsTopic() 

    /**
     * Returns the value of field 'backupDeliveryMode'.
     * 
     * @return int
     * @return the value of field 'backupDeliveryMode'.
     */
    public int getBackupDeliveryMode()
    {
        return this._backupDeliveryMode;
    } //-- int getBackupDeliveryMode() 

    /**
     * Returns the value of field 'backupPriority'.
     * 
     * @return int
     * @return the value of field 'backupPriority'.
     */
    public int getBackupPriority()
    {
        return this._backupPriority;
    } //-- int getBackupPriority() 

    /**
     * Returns the value of field 'backupProperty'.
     * 
     * @return String
     * @return the value of field 'backupProperty'.
     */
    public java.lang.String getBackupProperty()
    {
        return this._backupProperty;
    } //-- java.lang.String getBackupProperty() 

    /**
     * Returns the value of field 'backupTimeToLive'.
     * 
     * @return long
     * @return the value of field 'backupTimeToLive'.
     */
    public long getBackupTimeToLive()
    {
        return this._backupTimeToLive;
    } //-- long getBackupTimeToLive() 

    /**
     * Returns the value of field 'changesDeliveryMode'.
     * 
     * @return int
     * @return the value of field 'changesDeliveryMode'.
     */
    public int getChangesDeliveryMode()
    {
        return this._changesDeliveryMode;
    } //-- int getChangesDeliveryMode() 

    /**
     * Returns the value of field 'changesPriority'.
     * 
     * @return int
     * @return the value of field 'changesPriority'.
     */
    public int getChangesPriority()
    {
        return this._changesPriority;
    } //-- int getChangesPriority() 

    /**
     * Returns the value of field 'changesTimeToLive'.
     * 
     * @return long
     * @return the value of field 'changesTimeToLive'.
     */
    public long getChangesTimeToLive()
    {
        return this._changesTimeToLive;
    } //-- long getChangesTimeToLive() 

    /**
     * Returns the value of field 'channelPoolSize'.
     * 
     * @return int
     * @return the value of field 'channelPoolSize'.
     */
    public int getChannelPoolSize()
    {
        return this._channelPoolSize;
    } //-- int getChannelPoolSize() 

    /**
     * Returns the value of field 'channelProperty'.
     * 
     * @return String
     * @return the value of field 'channelProperty'.
     */
    public java.lang.String getChannelProperty()
    {
        return this._channelProperty;
    } //-- java.lang.String getChannelProperty() 

    /**
     * Returns the value of field 'sourceHostnameProperty'.
     * 
     * @return String
     * @return the value of field 'sourceHostnameProperty'.
     */
    public java.lang.String getSourceHostnameProperty()
    {
        return this._sourceHostnameProperty;
    } //-- java.lang.String getSourceHostnameProperty() 

    /**
     * Returns the value of field 'sourceNameProperty'.
     * 
     * @return String
     * @return the value of field 'sourceNameProperty'.
     */
    public java.lang.String getSourceNameProperty()
    {
        return this._sourceNameProperty;
    } //-- java.lang.String getSourceNameProperty() 

    /**
     * Method hasBackupDeliveryMode
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasBackupDeliveryMode()
    {
        return this._has_backupDeliveryMode;
    } //-- boolean hasBackupDeliveryMode() 

    /**
     * Method hasBackupPriority
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasBackupPriority()
    {
        return this._has_backupPriority;
    } //-- boolean hasBackupPriority() 

    /**
     * Method hasBackupTimeToLive
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasBackupTimeToLive()
    {
        return this._has_backupTimeToLive;
    } //-- boolean hasBackupTimeToLive() 

    /**
     * Method hasChangesDeliveryMode
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasChangesDeliveryMode()
    {
        return this._has_changesDeliveryMode;
    } //-- boolean hasChangesDeliveryMode() 

    /**
     * Method hasChangesPriority
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasChangesPriority()
    {
        return this._has_changesPriority;
    } //-- boolean hasChangesPriority() 

    /**
     * Method hasChangesTimeToLive
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasChangesTimeToLive()
    {
        return this._has_changesTimeToLive;
    } //-- boolean hasChangesTimeToLive() 

    /**
     * Method hasChannelPoolSize
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasChannelPoolSize()
    {
        return this._has_channelPoolSize;
    } //-- boolean hasChannelPoolSize() 

    /**
     * Method isValid
     * 
     * 
     * 
     * @return boolean
     */
    public boolean isValid()
    {
        try {
            validate();
        }
        catch (org.exolab.castor.xml.ValidationException vex) {
            return false;
        }
        return true;
    } //-- boolean isValid() 

    /**
     * Method marshal
     * 
     * 
     * 
     * @param out
     */
    public void marshal(java.io.Writer out)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        
        Marshaller.marshal(this, out);
    } //-- void marshal(java.io.Writer) 

    /**
     * Method marshal
     * 
     * 
     * 
     * @param handler
     */
    public void marshal(org.xml.sax.ContentHandler handler)
        throws java.io.IOException, org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        
        Marshaller.marshal(this, handler);
    } //-- void marshal(org.xml.sax.ContentHandler) 

    /**
     * Sets the value of field 'ASIVersion'.
     * 
     * @param ASIVersion the value of field 'ASIVersion'.
     */
    public void setASIVersion(java.lang.String ASIVersion)
    {
        this._ASIVersion = ASIVersion;
    } //-- void setASIVersion(java.lang.String) 

    /**
     * Sets the value of field 'alarmsNumberProperty'.
     * 
     * @param alarmsNumberProperty the value of field
     * 'alarmsNumberProperty'.
     */
    public void setAlarmsNumberProperty(java.lang.String alarmsNumberProperty)
    {
        this._alarmsNumberProperty = alarmsNumberProperty;
    } //-- void setAlarmsNumberProperty(java.lang.String) 

    /**
     * Sets the value of field 'alarmsTopic'.
     * 
     * @param alarmsTopic the value of field 'alarmsTopic'.
     */
    public void setAlarmsTopic(java.lang.String alarmsTopic)
    {
        this._alarmsTopic = alarmsTopic;
    } //-- void setAlarmsTopic(java.lang.String) 

    /**
     * Sets the value of field 'backupDeliveryMode'.
     * 
     * @param backupDeliveryMode the value of field
     * 'backupDeliveryMode'.
     */
    public void setBackupDeliveryMode(int backupDeliveryMode)
    {
        this._backupDeliveryMode = backupDeliveryMode;
        this._has_backupDeliveryMode = true;
    } //-- void setBackupDeliveryMode(int) 

    /**
     * Sets the value of field 'backupPriority'.
     * 
     * @param backupPriority the value of field 'backupPriority'.
     */
    public void setBackupPriority(int backupPriority)
    {
        this._backupPriority = backupPriority;
        this._has_backupPriority = true;
    } //-- void setBackupPriority(int) 

    /**
     * Sets the value of field 'backupProperty'.
     * 
     * @param backupProperty the value of field 'backupProperty'.
     */
    public void setBackupProperty(java.lang.String backupProperty)
    {
        this._backupProperty = backupProperty;
    } //-- void setBackupProperty(java.lang.String) 

    /**
     * Sets the value of field 'backupTimeToLive'.
     * 
     * @param backupTimeToLive the value of field 'backupTimeToLive'
     */
    public void setBackupTimeToLive(long backupTimeToLive)
    {
        this._backupTimeToLive = backupTimeToLive;
        this._has_backupTimeToLive = true;
    } //-- void setBackupTimeToLive(long) 

    /**
     * Sets the value of field 'changesDeliveryMode'.
     * 
     * @param changesDeliveryMode the value of field
     * 'changesDeliveryMode'.
     */
    public void setChangesDeliveryMode(int changesDeliveryMode)
    {
        this._changesDeliveryMode = changesDeliveryMode;
        this._has_changesDeliveryMode = true;
    } //-- void setChangesDeliveryMode(int) 

    /**
     * Sets the value of field 'changesPriority'.
     * 
     * @param changesPriority the value of field 'changesPriority'.
     */
    public void setChangesPriority(int changesPriority)
    {
        this._changesPriority = changesPriority;
        this._has_changesPriority = true;
    } //-- void setChangesPriority(int) 

    /**
     * Sets the value of field 'changesTimeToLive'.
     * 
     * @param changesTimeToLive the value of field
     * 'changesTimeToLive'.
     */
    public void setChangesTimeToLive(long changesTimeToLive)
    {
        this._changesTimeToLive = changesTimeToLive;
        this._has_changesTimeToLive = true;
    } //-- void setChangesTimeToLive(long) 

    /**
     * Sets the value of field 'channelPoolSize'.
     * 
     * @param channelPoolSize the value of field 'channelPoolSize'.
     */
    public void setChannelPoolSize(int channelPoolSize)
    {
        this._channelPoolSize = channelPoolSize;
        this._has_channelPoolSize = true;
    } //-- void setChannelPoolSize(int) 

    /**
     * Sets the value of field 'channelProperty'.
     * 
     * @param channelProperty the value of field 'channelProperty'.
     */
    public void setChannelProperty(java.lang.String channelProperty)
    {
        this._channelProperty = channelProperty;
    } //-- void setChannelProperty(java.lang.String) 

    /**
     * Sets the value of field 'sourceHostnameProperty'.
     * 
     * @param sourceHostnameProperty the value of field
     * 'sourceHostnameProperty'.
     */
    public void setSourceHostnameProperty(java.lang.String sourceHostnameProperty)
    {
        this._sourceHostnameProperty = sourceHostnameProperty;
    } //-- void setSourceHostnameProperty(java.lang.String) 

    /**
     * Sets the value of field 'sourceNameProperty'.
     * 
     * @param sourceNameProperty the value of field
     * 'sourceNameProperty'.
     */
    public void setSourceNameProperty(java.lang.String sourceNameProperty)
    {
        this._sourceNameProperty = sourceNameProperty;
    } //-- void setSourceNameProperty(java.lang.String) 

    /**
     * Method unmarshalASIConfiguration
     * 
     * 
     * 
     * @param reader
     * @return ASIConfiguration
     */
    public static cern.laser.source.alarmsysteminterface.impl.configuration.ASIConfiguration unmarshalASIConfiguration(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (cern.laser.source.alarmsysteminterface.impl.configuration.ASIConfiguration) Unmarshaller.unmarshal(cern.laser.source.alarmsysteminterface.impl.configuration.ASIConfiguration.class, reader);
    } //-- cern.laser.source.alarmsysteminterface.impl.configuration.ASIConfiguration unmarshalASIConfiguration(java.io.Reader) 

    /**
     * Method validate
     * 
     */
    public void validate()
        throws org.exolab.castor.xml.ValidationException
    {
        org.exolab.castor.xml.Validator validator = new org.exolab.castor.xml.Validator();
        validator.validate(this);
    } //-- void validate() 

}
