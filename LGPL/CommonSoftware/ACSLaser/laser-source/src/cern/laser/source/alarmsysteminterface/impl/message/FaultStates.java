/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.6</a>, using an XML
 * Schema.
 * $Id: FaultStates.java,v 1.1 2006/08/09 22:41:22 sharring Exp $
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
 * Class FaultStates.
 * 
 * @version $Revision: 1.1 $ $Date: 2006/08/09 22:41:22 $
 */
public class FaultStates implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    /**
     * Field _faultStateList
     */
    private java.util.Vector _faultStateList;


      //----------------/
     //- Constructors -/
    //----------------/

    public FaultStates() {
        super();
        _faultStateList = new Vector();
    } //-- cern.laser.source.alarmsysteminterface.impl.message.FaultStates()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Method addFaultState
     * 
     * 
     * 
     * @param vFaultState
     */
    public void addFaultState(cern.laser.source.alarmsysteminterface.impl.message.FaultState vFaultState)
        throws java.lang.IndexOutOfBoundsException
    {
        _faultStateList.addElement(vFaultState);
    } //-- void addFaultState(cern.laser.source.alarmsysteminterface.impl.message.FaultState) 

    /**
     * Method addFaultState
     * 
     * 
     * 
     * @param index
     * @param vFaultState
     */
    public void addFaultState(int index, cern.laser.source.alarmsysteminterface.impl.message.FaultState vFaultState)
        throws java.lang.IndexOutOfBoundsException
    {
        _faultStateList.insertElementAt(vFaultState, index);
    } //-- void addFaultState(int, cern.laser.source.alarmsysteminterface.impl.message.FaultState) 

    /**
     * Method enumerateFaultState
     * 
     * 
     * 
     * @return Enumeration
     */
    public java.util.Enumeration enumerateFaultState()
    {
        return _faultStateList.elements();
    } //-- java.util.Enumeration enumerateFaultState() 

    /**
     * Method getFaultState
     * 
     * 
     * 
     * @param index
     * @return FaultState
     */
    public cern.laser.source.alarmsysteminterface.impl.message.FaultState getFaultState(int index)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _faultStateList.size())) {
            throw new IndexOutOfBoundsException();
        }
        
        return (cern.laser.source.alarmsysteminterface.impl.message.FaultState) _faultStateList.elementAt(index);
    } //-- cern.laser.source.alarmsysteminterface.impl.message.FaultState getFaultState(int) 

    /**
     * Method getFaultState
     * 
     * 
     * 
     * @return FaultState
     */
    public cern.laser.source.alarmsysteminterface.impl.message.FaultState[] getFaultState()
    {
        int size = _faultStateList.size();
        cern.laser.source.alarmsysteminterface.impl.message.FaultState[] mArray = new cern.laser.source.alarmsysteminterface.impl.message.FaultState[size];
        for (int index = 0; index < size; index++) {
            mArray[index] = (cern.laser.source.alarmsysteminterface.impl.message.FaultState) _faultStateList.elementAt(index);
        }
        return mArray;
    } //-- cern.laser.source.alarmsysteminterface.impl.message.FaultState[] getFaultState() 

    /**
     * Method getFaultStateCount
     * 
     * 
     * 
     * @return int
     */
    public int getFaultStateCount()
    {
        return _faultStateList.size();
    } //-- int getFaultStateCount() 

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
     * Method removeAllFaultState
     * 
     */
    public void removeAllFaultState()
    {
        _faultStateList.removeAllElements();
    } //-- void removeAllFaultState() 

    /**
     * Method removeFaultState
     * 
     * 
     * 
     * @param index
     * @return FaultState
     */
    public cern.laser.source.alarmsysteminterface.impl.message.FaultState removeFaultState(int index)
    {
        java.lang.Object obj = _faultStateList.elementAt(index);
        _faultStateList.removeElementAt(index);
        return (cern.laser.source.alarmsysteminterface.impl.message.FaultState) obj;
    } //-- cern.laser.source.alarmsysteminterface.impl.message.FaultState removeFaultState(int) 

    /**
     * Method setFaultState
     * 
     * 
     * 
     * @param index
     * @param vFaultState
     */
    public void setFaultState(int index, cern.laser.source.alarmsysteminterface.impl.message.FaultState vFaultState)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _faultStateList.size())) {
            throw new IndexOutOfBoundsException();
        }
        _faultStateList.setElementAt(vFaultState, index);
    } //-- void setFaultState(int, cern.laser.source.alarmsysteminterface.impl.message.FaultState) 

    /**
     * Method setFaultState
     * 
     * 
     * 
     * @param faultStateArray
     */
    public void setFaultState(cern.laser.source.alarmsysteminterface.impl.message.FaultState[] faultStateArray)
    {
        //-- copy array
        _faultStateList.removeAllElements();
        for (int i = 0; i < faultStateArray.length; i++) {
            _faultStateList.addElement(faultStateArray[i]);
        }
    } //-- void setFaultState(cern.laser.source.alarmsysteminterface.impl.message.FaultState) 

    /**
     * Method unmarshalFaultStates
     * 
     * 
     * 
     * @param reader
     * @return FaultStates
     */
    public static cern.laser.source.alarmsysteminterface.impl.message.FaultStates unmarshalFaultStates(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (cern.laser.source.alarmsysteminterface.impl.message.FaultStates) Unmarshaller.unmarshal(cern.laser.source.alarmsysteminterface.impl.message.FaultStates.class, reader);
    } //-- cern.laser.source.alarmsysteminterface.impl.message.FaultStates unmarshalFaultStates(java.io.Reader) 

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
