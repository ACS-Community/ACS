/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.6</a>, using an XML
 * Schema.
 * $Id: Properties.java,v 1.1 2006/08/09 22:41:22 sharring Exp $
 */

package cern.laser.source.alarmsysteminterface.impl.message;

  //---------------------------------/
 //- Imported classes and packages -/
//---------------------------------/

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;
import java.util.Enumeration;
import java.util.Vector;
import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;
import org.xml.sax.ContentHandler;

/**
 * Class Properties.
 * 
 * @version $Revision: 1.1 $ $Date: 2006/08/09 22:41:22 $
 */
public class Properties implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    /**
     * Field _propertyList
     */
    private java.util.Vector _propertyList;


      //----------------/
     //- Constructors -/
    //----------------/

    public Properties() {
        super();
        _propertyList = new Vector();
    } //-- cern.laser.source.alarmsysteminterface.impl.message.Properties()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Method addProperty
     * 
     * 
     * 
     * @param vProperty
     */
    public void addProperty(cern.laser.source.alarmsysteminterface.impl.message.Property vProperty)
        throws java.lang.IndexOutOfBoundsException
    {
        _propertyList.addElement(vProperty);
    } //-- void addProperty(cern.laser.source.alarmsysteminterface.impl.message.Property) 

    /**
     * Method addProperty
     * 
     * 
     * 
     * @param index
     * @param vProperty
     */
    public void addProperty(int index, cern.laser.source.alarmsysteminterface.impl.message.Property vProperty)
        throws java.lang.IndexOutOfBoundsException
    {
        _propertyList.insertElementAt(vProperty, index);
    } //-- void addProperty(int, cern.laser.source.alarmsysteminterface.impl.message.Property) 

    /**
     * Method enumerateProperty
     * 
     * 
     * 
     * @return Enumeration
     */
    public java.util.Enumeration enumerateProperty()
    {
        return _propertyList.elements();
    } //-- java.util.Enumeration enumerateProperty() 

    /**
     * Method getProperty
     * 
     * 
     * 
     * @param index
     * @return Property
     */
    public cern.laser.source.alarmsysteminterface.impl.message.Property getProperty(int index)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _propertyList.size())) {
            throw new IndexOutOfBoundsException();
        }
        
        return (cern.laser.source.alarmsysteminterface.impl.message.Property) _propertyList.elementAt(index);
    } //-- cern.laser.source.alarmsysteminterface.impl.message.Property getProperty(int) 

    /**
     * Method getProperty
     * 
     * 
     * 
     * @return Property
     */
    public cern.laser.source.alarmsysteminterface.impl.message.Property[] getProperty()
    {
        int size = _propertyList.size();
        cern.laser.source.alarmsysteminterface.impl.message.Property[] mArray = new cern.laser.source.alarmsysteminterface.impl.message.Property[size];
        for (int index = 0; index < size; index++) {
            mArray[index] = (cern.laser.source.alarmsysteminterface.impl.message.Property) _propertyList.elementAt(index);
        }
        return mArray;
    } //-- cern.laser.source.alarmsysteminterface.impl.message.Property[] getProperty() 

    /**
     * Method getPropertyCount
     * 
     * 
     * 
     * @return int
     */
    public int getPropertyCount()
    {
        return _propertyList.size();
    } //-- int getPropertyCount() 

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
     * Method removeAllProperty
     * 
     */
    public void removeAllProperty()
    {
        _propertyList.removeAllElements();
    } //-- void removeAllProperty() 

    /**
     * Method removeProperty
     * 
     * 
     * 
     * @param index
     * @return Property
     */
    public cern.laser.source.alarmsysteminterface.impl.message.Property removeProperty(int index)
    {
        java.lang.Object obj = _propertyList.elementAt(index);
        _propertyList.removeElementAt(index);
        return (cern.laser.source.alarmsysteminterface.impl.message.Property) obj;
    } //-- cern.laser.source.alarmsysteminterface.impl.message.Property removeProperty(int) 

    /**
     * Method setProperty
     * 
     * 
     * 
     * @param index
     * @param vProperty
     */
    public void setProperty(int index, cern.laser.source.alarmsysteminterface.impl.message.Property vProperty)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _propertyList.size())) {
            throw new IndexOutOfBoundsException();
        }
        _propertyList.setElementAt(vProperty, index);
    } //-- void setProperty(int, cern.laser.source.alarmsysteminterface.impl.message.Property) 

    /**
     * Method setProperty
     * 
     * 
     * 
     * @param propertyArray
     */
    public void setProperty(cern.laser.source.alarmsysteminterface.impl.message.Property[] propertyArray)
    {
        //-- copy array
        _propertyList.removeAllElements();
        for (int i = 0; i < propertyArray.length; i++) {
            _propertyList.addElement(propertyArray[i]);
        }
    } //-- void setProperty(cern.laser.source.alarmsysteminterface.impl.message.Property) 

    /**
     * Method unmarshalProperties
     * 
     * 
     * 
     * @param reader
     * @return Properties
     */
    public static cern.laser.source.alarmsysteminterface.impl.message.Properties unmarshalProperties(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (cern.laser.source.alarmsysteminterface.impl.message.Properties) Unmarshaller.unmarshal(cern.laser.source.alarmsysteminterface.impl.message.Properties.class, reader);
    } //-- cern.laser.source.alarmsysteminterface.impl.message.Properties unmarshalProperties(java.io.Reader) 

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
