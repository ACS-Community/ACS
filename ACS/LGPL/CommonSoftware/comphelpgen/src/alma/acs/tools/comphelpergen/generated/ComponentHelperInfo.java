/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/*
 * This class was automatically generated with 
 * <a href="http://castor.exolab.org">Castor 0.9.3.9+</a>, using an
 * XML Schema.
 * $Id$
 */

package alma.acs.tools.comphelpergen.generated;

  //---------------------------------/
 //- Imported classes and packages -/
//---------------------------------/

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Enumeration;
import org.exolab.castor.xml.*;
import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.ValidationException;
import org.xml.sax.ContentHandler;

/**
 * 
 * 
 * @version $Revision$ $Date$
**/
public class ComponentHelperInfo implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    private java.lang.String _outputRootDirectory;

    private java.util.ArrayList _componentInterfaceList;


      //----------------/
     //- Constructors -/
    //----------------/

    public ComponentHelperInfo() {
        super();
        _componentInterfaceList = new ArrayList();
    } //-- alma.acs.tools.comphelpergen.generated.ComponentHelperInfo()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * 
     * 
     * @param vComponentInterface
    **/
    public void addComponentInterface(ComponentInterface vComponentInterface)
        throws java.lang.IndexOutOfBoundsException
    {
        _componentInterfaceList.add(vComponentInterface);
    } //-- void addComponentInterface(ComponentInterface) 

    /**
     * 
     * 
     * @param index
     * @param vComponentInterface
    **/
    public void addComponentInterface(int index, ComponentInterface vComponentInterface)
        throws java.lang.IndexOutOfBoundsException
    {
        _componentInterfaceList.add(index, vComponentInterface);
    } //-- void addComponentInterface(int, ComponentInterface) 

    /**
    **/
    public void clearComponentInterface()
    {
        _componentInterfaceList.clear();
    } //-- void clearComponentInterface() 

    /**
    **/
    public java.util.Enumeration enumerateComponentInterface()
    {
        return new org.exolab.castor.util.IteratorEnumeration(_componentInterfaceList.iterator());
    } //-- java.util.Enumeration enumerateComponentInterface() 

    /**
     * 
     * 
     * @param index
    **/
    public ComponentInterface getComponentInterface(int index)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _componentInterfaceList.size())) {
            throw new IndexOutOfBoundsException();
        }
        
        return (ComponentInterface) _componentInterfaceList.get(index);
    } //-- ComponentInterface getComponentInterface(int) 

    /**
    **/
    public ComponentInterface[] getComponentInterface()
    {
        int size = _componentInterfaceList.size();
        ComponentInterface[] mArray = new ComponentInterface[size];
        for (int index = 0; index < size; index++) {
            mArray[index] = (ComponentInterface) _componentInterfaceList.get(index);
        }
        return mArray;
    } //-- ComponentInterface[] getComponentInterface() 

    /**
    **/
    public int getComponentInterfaceCount()
    {
        return _componentInterfaceList.size();
    } //-- int getComponentInterfaceCount() 

    /**
     * Returns the value of field 'outputRootDirectory'.
     * 
     * @return the value of field 'outputRootDirectory'.
    **/
    public java.lang.String getOutputRootDirectory()
    {
        return this._outputRootDirectory;
    } //-- java.lang.String getOutputRootDirectory() 

    /**
    **/
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
     * 
     * 
     * @param out
    **/
    public void marshal(java.io.Writer out)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        
        Marshaller.marshal(this, out);
    } //-- void marshal(java.io.Writer) 

    /**
     * 
     * 
     * @param handler
    **/
    public void marshal(org.xml.sax.ContentHandler handler)
        throws java.io.IOException, org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        
        Marshaller.marshal(this, handler);
    } //-- void marshal(org.xml.sax.ContentHandler) 

    /**
     * 
     * 
     * @param vComponentInterface
    **/
    public boolean removeComponentInterface(ComponentInterface vComponentInterface)
    {
        boolean removed = _componentInterfaceList.remove(vComponentInterface);
        return removed;
    } //-- boolean removeComponentInterface(ComponentInterface) 

    /**
     * 
     * 
     * @param index
     * @param vComponentInterface
    **/
    public void setComponentInterface(int index, ComponentInterface vComponentInterface)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _componentInterfaceList.size())) {
            throw new IndexOutOfBoundsException();
        }
        _componentInterfaceList.set(index, vComponentInterface);
    } //-- void setComponentInterface(int, ComponentInterface) 

    /**
     * 
     * 
     * @param componentInterfaceArray
    **/
    public void setComponentInterface(ComponentInterface[] componentInterfaceArray)
    {
        //-- copy array
        _componentInterfaceList.clear();
        for (int i = 0; i < componentInterfaceArray.length; i++) {
            _componentInterfaceList.add(componentInterfaceArray[i]);
        }
    } //-- void setComponentInterface(ComponentInterface) 

    /**
     * Sets the value of field 'outputRootDirectory'.
     * 
     * @param outputRootDirectory the value of field
     * 'outputRootDirectory'.
    **/
    public void setOutputRootDirectory(java.lang.String outputRootDirectory)
    {
        this._outputRootDirectory = outputRootDirectory;
    } //-- void setOutputRootDirectory(java.lang.String) 

    /**
     * 
     * 
     * @param reader
    **/
    public static alma.acs.tools.comphelpergen.generated.ComponentHelperInfo unmarshalComponentHelperInfo(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (alma.acs.tools.comphelpergen.generated.ComponentHelperInfo) Unmarshaller.unmarshal(alma.acs.tools.comphelpergen.generated.ComponentHelperInfo.class, reader);
    } //-- alma.acs.tools.comphelpergen.generated.ComponentHelperInfo unmarshalComponentHelperInfo(java.io.Reader) 

    /**
    **/
    public void validate()
        throws org.exolab.castor.xml.ValidationException
    {
        org.exolab.castor.xml.Validator validator = new org.exolab.castor.xml.Validator();
        validator.validate(this);
    } //-- void validate() 

}
