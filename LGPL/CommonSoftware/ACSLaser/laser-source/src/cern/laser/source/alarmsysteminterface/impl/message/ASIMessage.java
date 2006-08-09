/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.6</a>, using an XML
 * Schema.
 * $Id: ASIMessage.java,v 1.1 2006/08/09 22:41:22 sharring Exp $
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
 * Class ASIMessage.
 * 
 * @version $Revision: 1.1 $ $Date: 2006/08/09 22:41:22 $
 */
public class ASIMessage implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    /**
     * Field _backup
     */
    private boolean _backup;

    /**
     * keeps track of state for field: _backup
     */
    private boolean _has_backup;

    /**
     * Field _version
     */
    private java.lang.String _version;

    /**
     * Field _sourceName
     */
    private java.lang.String _sourceName;

    /**
     * Field _sourceHostname
     */
    private java.lang.String _sourceHostname;

    /**
     * Field _sourceTimestamp
     */
    private cern.laser.source.alarmsysteminterface.impl.message.Timestamp _sourceTimestamp;

    /**
     * Field _faultStates
     */
    private cern.laser.source.alarmsysteminterface.impl.message.FaultStates _faultStates;


      //----------------/
     //- Constructors -/
    //----------------/

    public ASIMessage() {
        super();
    } //-- cern.laser.source.alarmsysteminterface.impl.message.ASIMessage()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Method deleteBackup
     * 
     */
    public void deleteBackup()
    {
        this._has_backup= false;
    } //-- void deleteBackup() 

    /**
     * Returns the value of field 'backup'.
     * 
     * @return boolean
     * @return the value of field 'backup'.
     */
    public boolean getBackup()
    {
        return this._backup;
    } //-- boolean getBackup() 

    /**
     * Returns the value of field 'faultStates'.
     * 
     * @return FaultStates
     * @return the value of field 'faultStates'.
     */
    public cern.laser.source.alarmsysteminterface.impl.message.FaultStates getFaultStates()
    {
        return this._faultStates;
    } //-- cern.laser.source.alarmsysteminterface.impl.message.FaultStates getFaultStates() 

    /**
     * Returns the value of field 'sourceHostname'.
     * 
     * @return String
     * @return the value of field 'sourceHostname'.
     */
    public java.lang.String getSourceHostname()
    {
        return this._sourceHostname;
    } //-- java.lang.String getSourceHostname() 

    /**
     * Returns the value of field 'sourceName'.
     * 
     * @return String
     * @return the value of field 'sourceName'.
     */
    public java.lang.String getSourceName()
    {
        return this._sourceName;
    } //-- java.lang.String getSourceName() 

    /**
     * Returns the value of field 'sourceTimestamp'.
     * 
     * @return Timestamp
     * @return the value of field 'sourceTimestamp'.
     */
    public cern.laser.source.alarmsysteminterface.impl.message.Timestamp getSourceTimestamp()
    {
        return this._sourceTimestamp;
    } //-- cern.laser.source.alarmsysteminterface.impl.message.Timestamp getSourceTimestamp() 

    /**
     * Returns the value of field 'version'.
     * 
     * @return String
     * @return the value of field 'version'.
     */
    public java.lang.String getVersion()
    {
        return this._version;
    } //-- java.lang.String getVersion() 

    /**
     * Method hasBackup
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasBackup()
    {
        return this._has_backup;
    } //-- boolean hasBackup() 

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
     * Sets the value of field 'backup'.
     * 
     * @param backup the value of field 'backup'.
     */
    public void setBackup(boolean backup)
    {
        this._backup = backup;
        this._has_backup = true;
    } //-- void setBackup(boolean) 

    /**
     * Sets the value of field 'faultStates'.
     * 
     * @param faultStates the value of field 'faultStates'.
     */
    public void setFaultStates(cern.laser.source.alarmsysteminterface.impl.message.FaultStates faultStates)
    {
        this._faultStates = faultStates;
    } //-- void setFaultStates(cern.laser.source.alarmsysteminterface.impl.message.FaultStates) 

    /**
     * Sets the value of field 'sourceHostname'.
     * 
     * @param sourceHostname the value of field 'sourceHostname'.
     */
    public void setSourceHostname(java.lang.String sourceHostname)
    {
        this._sourceHostname = sourceHostname;
    } //-- void setSourceHostname(java.lang.String) 

    /**
     * Sets the value of field 'sourceName'.
     * 
     * @param sourceName the value of field 'sourceName'.
     */
    public void setSourceName(java.lang.String sourceName)
    {
        this._sourceName = sourceName;
    } //-- void setSourceName(java.lang.String) 

    /**
     * Sets the value of field 'sourceTimestamp'.
     * 
     * @param sourceTimestamp the value of field 'sourceTimestamp'.
     */
    public void setSourceTimestamp(cern.laser.source.alarmsysteminterface.impl.message.Timestamp sourceTimestamp)
    {
        this._sourceTimestamp = sourceTimestamp;
    } //-- void setSourceTimestamp(cern.laser.source.alarmsysteminterface.impl.message.Timestamp) 

    /**
     * Sets the value of field 'version'.
     * 
     * @param version the value of field 'version'.
     */
    public void setVersion(java.lang.String version)
    {
        this._version = version;
    } //-- void setVersion(java.lang.String) 

    /**
     * Method unmarshalASIMessage
     * 
     * 
     * 
     * @param reader
     * @return ASIMessage
     */
    public static cern.laser.source.alarmsysteminterface.impl.message.ASIMessage unmarshalASIMessage(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (cern.laser.source.alarmsysteminterface.impl.message.ASIMessage) Unmarshaller.unmarshal(cern.laser.source.alarmsysteminterface.impl.message.ASIMessage.class, reader);
    } //-- cern.laser.source.alarmsysteminterface.impl.message.ASIMessage unmarshalASIMessage(java.io.Reader) 

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
