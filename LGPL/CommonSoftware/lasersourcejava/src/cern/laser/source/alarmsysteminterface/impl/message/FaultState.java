/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.6</a>, using an XML
 * Schema.
 * $Id: FaultState.java,v 1.1 2006/08/09 22:41:22 sharring Exp $
 */

package cern.laser.source.alarmsysteminterface.impl.message;

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
 * Class FaultState.
 * 
 * @version $Revision: 1.1 $ $Date: 2006/08/09 22:41:22 $
 */
public class FaultState implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    /**
     * Field _family
     */
    private java.lang.String _family;

    /**
     * Field _member
     */
    private java.lang.String _member;

    /**
     * Field _code
     */
    private int _code;

    /**
     * keeps track of state for field: _code
     */
    private boolean _has_code;

    /**
     * Field _descriptor
     */
    private java.lang.String _descriptor;

    /**
     * Field _userProperties
     */
    private cern.laser.source.alarmsysteminterface.impl.message.Properties _userProperties;

    /**
     * Field _userTimestamp
     */
    private cern.laser.source.alarmsysteminterface.impl.message.Timestamp _userTimestamp;

    /**
     * Field _activatedByBackup
     */
    private boolean _activatedByBackup;

    /**
     * keeps track of state for field: _activatedByBackup
     */
    private boolean _has_activatedByBackup;

    /**
     * Field _terminatedByBackup
     */
    private boolean _terminatedByBackup;

    /**
     * keeps track of state for field: _terminatedByBackup
     */
    private boolean _has_terminatedByBackup;


      //----------------/
     //- Constructors -/
    //----------------/

    public FaultState() {
        super();
    } //-- cern.laser.source.alarmsysteminterface.impl.message.FaultState()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Method deleteActivatedByBackup
     * 
     */
    public void deleteActivatedByBackup()
    {
        this._has_activatedByBackup= false;
    } //-- void deleteActivatedByBackup() 

    /**
     * Method deleteCode
     * 
     */
    public void deleteCode()
    {
        this._has_code= false;
    } //-- void deleteCode() 

    /**
     * Method deleteTerminatedByBackup
     * 
     */
    public void deleteTerminatedByBackup()
    {
        this._has_terminatedByBackup= false;
    } //-- void deleteTerminatedByBackup() 

    /**
     * Returns the value of field 'activatedByBackup'.
     * 
     * @return boolean
     * @return the value of field 'activatedByBackup'.
     */
    public boolean getActivatedByBackup()
    {
        return this._activatedByBackup;
    } //-- boolean getActivatedByBackup() 

    /**
     * Returns the value of field 'code'.
     * 
     * @return int
     * @return the value of field 'code'.
     */
    public int getCode()
    {
        return this._code;
    } //-- int getCode() 

    /**
     * Returns the value of field 'descriptor'.
     * 
     * @return String
     * @return the value of field 'descriptor'.
     */
    public java.lang.String getDescriptor()
    {
        return this._descriptor;
    } //-- java.lang.String getDescriptor() 

    /**
     * Returns the value of field 'family'.
     * 
     * @return String
     * @return the value of field 'family'.
     */
    public java.lang.String getFamily()
    {
        return this._family;
    } //-- java.lang.String getFamily() 

    /**
     * Returns the value of field 'member'.
     * 
     * @return String
     * @return the value of field 'member'.
     */
    public java.lang.String getMember()
    {
        return this._member;
    } //-- java.lang.String getMember() 

    /**
     * Returns the value of field 'terminatedByBackup'.
     * 
     * @return boolean
     * @return the value of field 'terminatedByBackup'.
     */
    public boolean getTerminatedByBackup()
    {
        return this._terminatedByBackup;
    } //-- boolean getTerminatedByBackup() 

    /**
     * Returns the value of field 'userProperties'.
     * 
     * @return Properties
     * @return the value of field 'userProperties'.
     */
    public cern.laser.source.alarmsysteminterface.impl.message.Properties getUserProperties()
    {
        return this._userProperties;
    } //-- cern.laser.source.alarmsysteminterface.impl.message.Properties getUserProperties() 

    /**
     * Returns the value of field 'userTimestamp'.
     * 
     * @return Timestamp
     * @return the value of field 'userTimestamp'.
     */
    public cern.laser.source.alarmsysteminterface.impl.message.Timestamp getUserTimestamp()
    {
        return this._userTimestamp;
    } //-- cern.laser.source.alarmsysteminterface.impl.message.Timestamp getUserTimestamp() 

    /**
     * Method hasActivatedByBackup
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasActivatedByBackup()
    {
        return this._has_activatedByBackup;
    } //-- boolean hasActivatedByBackup() 

    /**
     * Method hasCode
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasCode()
    {
        return this._has_code;
    } //-- boolean hasCode() 

    /**
     * Method hasTerminatedByBackup
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasTerminatedByBackup()
    {
        return this._has_terminatedByBackup;
    } //-- boolean hasTerminatedByBackup() 

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
     * Sets the value of field 'activatedByBackup'.
     * 
     * @param activatedByBackup the value of field
     * 'activatedByBackup'.
     */
    public void setActivatedByBackup(boolean activatedByBackup)
    {
        this._activatedByBackup = activatedByBackup;
        this._has_activatedByBackup = true;
    } //-- void setActivatedByBackup(boolean) 

    /**
     * Sets the value of field 'code'.
     * 
     * @param code the value of field 'code'.
     */
    public void setCode(int code)
    {
        this._code = code;
        this._has_code = true;
    } //-- void setCode(int) 

    /**
     * Sets the value of field 'descriptor'.
     * 
     * @param descriptor the value of field 'descriptor'.
     */
    public void setDescriptor(java.lang.String descriptor)
    {
        this._descriptor = descriptor;
    } //-- void setDescriptor(java.lang.String) 

    /**
     * Sets the value of field 'family'.
     * 
     * @param family the value of field 'family'.
     */
    public void setFamily(java.lang.String family)
    {
        this._family = family;
    } //-- void setFamily(java.lang.String) 

    /**
     * Sets the value of field 'member'.
     * 
     * @param member the value of field 'member'.
     */
    public void setMember(java.lang.String member)
    {
        this._member = member;
    } //-- void setMember(java.lang.String) 

    /**
     * Sets the value of field 'terminatedByBackup'.
     * 
     * @param terminatedByBackup the value of field
     * 'terminatedByBackup'.
     */
    public void setTerminatedByBackup(boolean terminatedByBackup)
    {
        this._terminatedByBackup = terminatedByBackup;
        this._has_terminatedByBackup = true;
    } //-- void setTerminatedByBackup(boolean) 

    /**
     * Sets the value of field 'userProperties'.
     * 
     * @param userProperties the value of field 'userProperties'.
     */
    public void setUserProperties(cern.laser.source.alarmsysteminterface.impl.message.Properties userProperties)
    {
        this._userProperties = userProperties;
    } //-- void setUserProperties(cern.laser.source.alarmsysteminterface.impl.message.Properties) 

    /**
     * Sets the value of field 'userTimestamp'.
     * 
     * @param userTimestamp the value of field 'userTimestamp'.
     */
    public void setUserTimestamp(cern.laser.source.alarmsysteminterface.impl.message.Timestamp userTimestamp)
    {
        this._userTimestamp = userTimestamp;
    } //-- void setUserTimestamp(cern.laser.source.alarmsysteminterface.impl.message.Timestamp) 

    /**
     * Method unmarshalFaultState
     * 
     * 
     * 
     * @param reader
     * @return FaultState
     */
    public static cern.laser.source.alarmsysteminterface.impl.message.FaultState unmarshalFaultState(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (cern.laser.source.alarmsysteminterface.impl.message.FaultState) Unmarshaller.unmarshal(cern.laser.source.alarmsysteminterface.impl.message.FaultState.class, reader);
    } //-- cern.laser.source.alarmsysteminterface.impl.message.FaultState unmarshalFaultState(java.io.Reader) 

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
