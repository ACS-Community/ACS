/*
 * ALMA - Atacama Large Millimiter Array
 * Copyright (c) European Southern Observatory, 2011 
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */
package alma.acs.monitoring.blobber;

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
import org.exolab.castor.xml.Validator;
import org.xml.sax.ContentHandler;

public class ACSAssemblyTypeT
  implements Serializable
{
  private String _name;
  private String _devName;
  private String _description;
  private ACSDefaultRole _defaultRole;
  private Vector _baciPropertyList;

  public ACSAssemblyTypeT()
  {
    this._baciPropertyList = new Vector();
  }

  public void addBaciProperty(ACSBaciPropertyT vBaciProperty)
    throws IndexOutOfBoundsException
  {
    this._baciPropertyList.addElement(vBaciProperty);
  }

  public void addBaciProperty(int index, ACSBaciPropertyT vBaciProperty)
    throws IndexOutOfBoundsException
  {
    this._baciPropertyList.insertElementAt(vBaciProperty, index);
  }

  public Enumeration enumerateBaciProperty()
  {
    return this._baciPropertyList.elements();
  }

  public ACSBaciPropertyT getBaciProperty(int index)
    throws IndexOutOfBoundsException
  {
    if ((index < 0) || (index > this._baciPropertyList.size())) {
      throw new IndexOutOfBoundsException();
    }

    return (ACSBaciPropertyT)this._baciPropertyList.elementAt(index);
  }

  public ACSBaciPropertyT[] getBaciProperty()
  {
    int size = this._baciPropertyList.size();
    ACSBaciPropertyT[] mArray = new ACSBaciPropertyT[size];
    for (int index = 0; index < size; index++) {
      mArray[index] = ((ACSBaciPropertyT)this._baciPropertyList.elementAt(index));
    }
    return mArray;
  }

  public int getBaciPropertyCount()
  {
    return this._baciPropertyList.size();
  }

  public ACSDefaultRole getDefaultRole()
  {
    return this._defaultRole;
  }

  public String getDescription()
  {
    return this._description;
  }

  public String getDevName()
  {
    return this._devName;
  }

  public String getName()
  {
    return this._name;
  }

  public boolean isValid()
  {
    try
    {
      validate();
    }
    catch (ValidationException vex) {
      return false;
    }
    return true;
  }

  public void marshal(Writer out)
    throws MarshalException, ValidationException
  {
    Marshaller.marshal(this, out);
  }

  public void marshal(ContentHandler handler)
    throws IOException, MarshalException, ValidationException
  {
    Marshaller.marshal(this, handler);
  }

  public void removeAllBaciProperty()
  {
    this._baciPropertyList.removeAllElements();
  }

  public ACSBaciPropertyT removeBaciProperty(int index)
  {
    Object obj = this._baciPropertyList.elementAt(index);
    this._baciPropertyList.removeElementAt(index);
    return (ACSBaciPropertyT)obj;
  }

  public void setBaciProperty(int index, ACSBaciPropertyT vBaciProperty)
    throws IndexOutOfBoundsException
  {
    if ((index < 0) || (index > this._baciPropertyList.size())) {
      throw new IndexOutOfBoundsException();
    }
    this._baciPropertyList.setElementAt(vBaciProperty, index);
  }

  public void setBaciProperty(ACSBaciPropertyT[] baciPropertyArray)
  {
    this._baciPropertyList.removeAllElements();
    for (int i = 0; i < baciPropertyArray.length; i++)
      this._baciPropertyList.addElement(baciPropertyArray[i]);
  }

  public void setDefaultRole(ACSDefaultRole defaultRole)
  {
    this._defaultRole = defaultRole;
  }

  public void setDescription(String description)
  {
    this._description = description;
  }

  public void setDevName(String devName)
  {
    this._devName = devName;
  }

  public void setName(String name)
  {
    this._name = name;
  }

  public static ACSAssemblyTypeT unmarshalAssemblyTypeT(Reader reader)
    throws MarshalException, ValidationException
  {
    return (ACSAssemblyTypeT)Unmarshaller.unmarshal(ACSAssemblyTypeT.class, reader);
  }

  public void validate()
    throws ValidationException
  {
    Validator validator = new Validator();
    validator.validate(this);
  }
}
