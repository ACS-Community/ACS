/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.6</a>, using an XML
 * Schema.
 * $Id: Timestamp.java,v 1.1 2006/08/09 22:41:22 sharring Exp $
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
 * Class Timestamp.
 * 
 * @version $Revision: 1.1 $ $Date: 2006/08/09 22:41:22 $
 */
public class Timestamp implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    /**
     * Field _seconds
     */
    private long _seconds;

    /**
     * keeps track of state for field: _seconds
     */
    private boolean _has_seconds;

    /**
     * Field _microseconds
     */
    private long _microseconds;

    /**
     * keeps track of state for field: _microseconds
     */
    private boolean _has_microseconds;


      //----------------/
     //- Constructors -/
    //----------------/

    public Timestamp() {
        super();
    } //-- cern.laser.source.alarmsysteminterface.impl.message.Timestamp()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Method deleteMicroseconds
     * 
     */
    public void deleteMicroseconds()
    {
        this._has_microseconds= false;
    } //-- void deleteMicroseconds() 

    /**
     * Method deleteSeconds
     * 
     */
    public void deleteSeconds()
    {
        this._has_seconds= false;
    } //-- void deleteSeconds() 

    /**
     * Returns the value of field 'microseconds'.
     * 
     * @return long
     * @return the value of field 'microseconds'.
     */
    public long getMicroseconds()
    {
        return this._microseconds;
    } //-- long getMicroseconds() 

    /**
     * Returns the value of field 'seconds'.
     * 
     * @return long
     * @return the value of field 'seconds'.
     */
    public long getSeconds()
    {
        return this._seconds;
    } //-- long getSeconds() 

    /**
     * Method hasMicroseconds
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasMicroseconds()
    {
        return this._has_microseconds;
    } //-- boolean hasMicroseconds() 

    /**
     * Method hasSeconds
     * 
     * 
     * 
     * @return boolean
     */
    public boolean hasSeconds()
    {
        return this._has_seconds;
    } //-- boolean hasSeconds() 

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
     * Sets the value of field 'microseconds'.
     * 
     * @param microseconds the value of field 'microseconds'.
     */
    public void setMicroseconds(long microseconds)
    {
        this._microseconds = microseconds;
        this._has_microseconds = true;
    } //-- void setMicroseconds(long) 

    /**
     * Sets the value of field 'seconds'.
     * 
     * @param seconds the value of field 'seconds'.
     */
    public void setSeconds(long seconds)
    {
        this._seconds = seconds;
        this._has_seconds = true;
    } //-- void setSeconds(long) 

    /**
     * Method unmarshalTimestamp
     * 
     * 
     * 
     * @param reader
     * @return Timestamp
     */
    public static cern.laser.source.alarmsysteminterface.impl.message.Timestamp unmarshalTimestamp(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (cern.laser.source.alarmsysteminterface.impl.message.Timestamp) Unmarshaller.unmarshal(cern.laser.source.alarmsysteminterface.impl.message.Timestamp.class, reader);
    } //-- cern.laser.source.alarmsysteminterface.impl.message.Timestamp unmarshalTimestamp(java.io.Reader) 

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
