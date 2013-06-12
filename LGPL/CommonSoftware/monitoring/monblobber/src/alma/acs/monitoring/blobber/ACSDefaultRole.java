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

public class ACSDefaultRole
  implements Serializable
{
  private Object _node;
  private Object _baseAddress;
  private Object _channel;

  public Object getBaseAddress()
  {
    return this._baseAddress;
  }

  public Object getChannel()
  {
    return this._channel;
  }

  public Object getNode()
  {
    return this._node;
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

  public void setBaseAddress(Object baseAddress)
  {
    this._baseAddress = baseAddress;
  }

  public void setChannel(Object channel)
  {
    this._channel = channel;
  }

  public void setNode(Object node)
  {
    this._node = node;
  }

  public static ACSDefaultRole unmarshalDefaultRole(Reader reader)
    throws MarshalException, ValidationException
  {
    return (ACSDefaultRole)Unmarshaller.unmarshal(ACSDefaultRole.class, reader);
  }

  public void validate()
    throws ValidationException
  {
    Validator validator = new Validator();
    validator.validate(this);
  }
}
