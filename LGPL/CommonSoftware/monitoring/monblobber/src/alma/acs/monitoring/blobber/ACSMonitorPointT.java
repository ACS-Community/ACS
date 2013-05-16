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

public class ACSMonitorPointT
  implements Serializable
{
  private String _monitorpointname;
  private String _datatype;
  private String _rca;
  private String _terelated;
  private String _rawdatatype;
  private String _worlddatatype;
  private String _units;
  private String _scale;
  private String _offset;
  private String _minrange;
  private String _maxrange;
  private String _description;

  public String getDatatype()
  {
    return this._datatype;
  }

  public String getDescription()
  {
    return this._description;
  }

  public String getMaxrange()
  {
    return this._maxrange;
  }

  public String getMinrange()
  {
    return this._minrange;
  }

  public String getMonitorpointname()
  {
    return this._monitorpointname;
  }

  public String getOffset()
  {
    return this._offset;
  }

  public String getRawdatatype()
  {
    return this._rawdatatype;
  }

  public String getRca()
  {
    return this._rca;
  }

  public String getScale()
  {
    return this._scale;
  }

  public String getTerelated()
  {
    return this._terelated;
  }

  public String getUnits()
  {
    return this._units;
  }

  public String getWorlddatatype()
  {
    return this._worlddatatype;
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

  public void setDatatype(String datatype)
  {
    this._datatype = datatype;
  }

  public void setDescription(String description)
  {
    this._description = description;
  }

  public void setMaxrange(String maxrange)
  {
    this._maxrange = maxrange;
  }

  public void setMinrange(String minrange)
  {
    this._minrange = minrange;
  }

  public void setMonitorpointname(String monitorpointname)
  {
    this._monitorpointname = monitorpointname;
  }

  public void setOffset(String offset)
  {
    this._offset = offset;
  }

  public void setRawdatatype(String rawdatatype)
  {
    this._rawdatatype = rawdatatype;
  }

  public void setRca(String rca)
  {
    this._rca = rca;
  }

  public void setScale(String scale)
  {
    this._scale = scale;
  }

  public void setTerelated(String terelated)
  {
    this._terelated = terelated;
  }

  public void setUnits(String units)
  {
    this._units = units;
  }

  public void setWorlddatatype(String worlddatatype)
  {
    this._worlddatatype = worlddatatype;
  }

  public static ACSMonitorPointT unmarshalMonitorPointT(Reader reader)
    throws MarshalException, ValidationException
  {
    return (ACSMonitorPointT)Unmarshaller.unmarshal(ACSMonitorPointT.class, reader);
  }

  public void validate()
    throws ValidationException
  {
    Validator validator = new Validator();
    validator.validate(this);
  }
}
