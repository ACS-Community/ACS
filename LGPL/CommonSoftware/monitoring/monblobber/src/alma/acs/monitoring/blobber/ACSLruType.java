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
import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;
import org.exolab.castor.xml.Validator;
import org.xml.sax.ContentHandler;

public class ACSLruType
  implements Serializable
{
  private String _name;
  private String _fullname;
  private String _icd;
  private long _icdDate;
  private boolean _has_icdDate;
  private String _description;
  private String _notes;
  private ACSAssemblyTypeT _assemblyType;

  public void deleteIcdDate()
  {
    this._has_icdDate = false;
  }

  public ACSAssemblyTypeT getAssemblyType()
  {
    return this._assemblyType;
  }

  public String getDescription()
  {
    return this._description;
  }

  public String getFullname()
  {
    return this._fullname;
  }

  public String getIcd()
  {
    return this._icd;
  }

  public long getIcdDate()
  {
    return this._icdDate;
  }

  public String getName()
  {
    return this._name;
  }

  public String getNotes()
  {
    return this._notes;
  }

  public boolean hasIcdDate()
  {
    return this._has_icdDate;
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

  public void setAssemblyType(ACSAssemblyTypeT assemblyType)
  {
    this._assemblyType = assemblyType;
  }

  public void setDescription(String description)
  {
    this._description = description;
  }

  public void setFullname(String fullname)
  {
    this._fullname = fullname;
  }

  public void setIcd(String icd)
  {
    this._icd = icd;
  }

  public void setIcdDate(long icdDate)
  {
    this._icdDate = icdDate;
    this._has_icdDate = true;
  }

  public void setName(String name)
  {
    this._name = name;
  }

  public void setNotes(String notes)
  {
    this._notes = notes;
  }

  public static ACSLruType unmarshalLruType(Reader reader)
    throws MarshalException, ValidationException
  {
    return (ACSLruType)Unmarshaller.unmarshal(ACSLruType.class, reader);
  }

  public void validate()
    throws ValidationException
  {
    Validator validator = new Validator();
    validator.validate(this);
  }
}
