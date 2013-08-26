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
import org.exolab.castor.xml.*;
import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.ValidationException;
import org.xml.sax.ContentHandler;

/**
 * 
 * 
 * @version $Revision$ $Date$
**/
public class ComponentInterface implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    private java.lang.String _corbaRepositoryId;

    private java.lang.String _idlPackage;

    private java.lang.String _componentClassName;

    private java.lang.String _internalInterface;


      //----------------/
     //- Constructors -/
    //----------------/

    public ComponentInterface() {
        super();
    } //-- alma.acs.tools.comphelpergen.generated.ComponentInterface()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Returns the value of field 'componentClassName'.
     * 
     * @return the value of field 'componentClassName'.
    **/
    public java.lang.String getComponentClassName()
    {
        return this._componentClassName;
    } //-- java.lang.String getComponentClassName() 

    /**
     * Returns the value of field 'corbaRepositoryId'.
     * 
     * @return the value of field 'corbaRepositoryId'.
    **/
    public java.lang.String getCorbaRepositoryId()
    {
        return this._corbaRepositoryId;
    } //-- java.lang.String getCorbaRepositoryId() 

    /**
     * Returns the value of field 'idlPackage'.
     * 
     * @return the value of field 'idlPackage'.
    **/
    public java.lang.String getIdlPackage()
    {
        return this._idlPackage;
    } //-- java.lang.String getIdlPackage() 

    /**
     * Returns the value of field 'internalInterface'.
     * 
     * @return the value of field 'internalInterface'.
    **/
    public java.lang.String getInternalInterface()
    {
        return this._internalInterface;
    } //-- java.lang.String getInternalInterface() 

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
     * Sets the value of field 'componentClassName'.
     * 
     * @param componentClassName the value of field
     * 'componentClassName'.
    **/
    public void setComponentClassName(java.lang.String componentClassName)
    {
        this._componentClassName = componentClassName;
    } //-- void setComponentClassName(java.lang.String) 

    /**
     * Sets the value of field 'corbaRepositoryId'.
     * 
     * @param corbaRepositoryId the value of field
     * 'corbaRepositoryId'.
    **/
    public void setCorbaRepositoryId(java.lang.String corbaRepositoryId)
    {
        this._corbaRepositoryId = corbaRepositoryId;
    } //-- void setCorbaRepositoryId(java.lang.String) 

    /**
     * Sets the value of field 'idlPackage'.
     * 
     * @param idlPackage the value of field 'idlPackage'.
    **/
    public void setIdlPackage(java.lang.String idlPackage)
    {
        this._idlPackage = idlPackage;
    } //-- void setIdlPackage(java.lang.String) 

    /**
     * Sets the value of field 'internalInterface'.
     * 
     * @param internalInterface the value of field
     * 'internalInterface'.
    **/
    public void setInternalInterface(java.lang.String internalInterface)
    {
        this._internalInterface = internalInterface;
    } //-- void setInternalInterface(java.lang.String) 

    /**
     * 
     * 
     * @param reader
    **/
    public static alma.acs.tools.comphelpergen.generated.ComponentInterface unmarshalComponentInterface(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (alma.acs.tools.comphelpergen.generated.ComponentInterface) Unmarshaller.unmarshal(alma.acs.tools.comphelpergen.generated.ComponentInterface.class, reader);
    } //-- alma.acs.tools.comphelpergen.generated.ComponentInterface unmarshalComponentInterface(java.io.Reader) 

    /**
    **/
    public void validate()
        throws org.exolab.castor.xml.ValidationException
    {
        org.exolab.castor.xml.Validator validator = new org.exolab.castor.xml.Validator();
        validator.validate(this);
    } //-- void validate() 

}
