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

public class ACSBaciPropertyT
  implements Serializable
{
  private String _propertyname;
  private String _description;
  private String _format;
  private String _units;
  private String _resolution;
  private String _archivePriority;
  private String _archiveMinInt;
  private String _archiveMaxInt;
  private String _defaultTimerTrig;
  private String _minTimerTrig;
  private String _initializeDevio;
  private String _minDeltaTrig;
  private String _defaultValue;
  private String _graphMin;
  private String _graphMax;
  private String _minStep;
  private String _archiveDelta;
  private String _alarmHighOn;
  private String _alarmLowOn;
  private String _alarmHighOff;
  private String _alarmLowOff;
  private String _alarmTimerTrig;
  private String _minValue;
  private String _maxValue;
  private String _bitdescription;
  private String _whenset;
  private String _whencleared;
  private String _statedescription;
  private String _condition;
  private String _alarmOn;
  private String _alarmOff;
  private String _data;
  private String _alarmFaultFamily;
  private String _alarmFaultMember;
  private String _alarmLevel;
  private String _archiveSuppress;
  private String _archiveMechanism;
  private Vector _monitorPointList;

  public ACSBaciPropertyT()
  {
    this._monitorPointList = new Vector();
  }

  public void addMonitorPoint(ACSMonitorPointT vMonitorPoint)
    throws IndexOutOfBoundsException
  {
    this._monitorPointList.addElement(vMonitorPoint);
  }

  public void addMonitorPoint(int index, ACSMonitorPointT vMonitorPoint)
    throws IndexOutOfBoundsException
  {
    this._monitorPointList.insertElementAt(vMonitorPoint, index);
  }

  public Enumeration enumerateMonitorPoint()
  {
    return this._monitorPointList.elements();
  }

  public String getAlarmFaultFamily()
  {
    return this._alarmFaultFamily;
  }

  public String getAlarmFaultMember()
  {
    return this._alarmFaultMember;
  }

  public String getAlarmHighOff()
  {
    return this._alarmHighOff;
  }

  public String getAlarmHighOn()
  {
    return this._alarmHighOn;
  }

  public String getAlarmLevel()
  {
    return this._alarmLevel;
  }

  public String getAlarmLowOff()
  {
    return this._alarmLowOff;
  }

  public String getAlarmLowOn()
  {
    return this._alarmLowOn;
  }

  public String getAlarmOff()
  {
    return this._alarmOff;
  }

  public String getAlarmOn()
  {
    return this._alarmOn;
  }

  public String getAlarmTimerTrig()
  {
    return this._alarmTimerTrig;
  }

  public String getArchiveDelta()
  {
    return this._archiveDelta;
  }

  public String getArchiveMaxInt()
  {
    return this._archiveMaxInt;
  }

  public String getArchiveMechanism()
  {
    return this._archiveMechanism;
  }

  public String getArchiveMinInt()
  {
    return this._archiveMinInt;
  }

  public String getArchivePriority()
  {
    return this._archivePriority;
  }

  public String getArchiveSuppress()
  {
    return this._archiveSuppress;
  }

  public String getBitdescription()
  {
    return this._bitdescription;
  }

  public String getCondition()
  {
    return this._condition;
  }

  public String getData()
  {
    return this._data;
  }

  public String getDefaultTimerTrig()
  {
    return this._defaultTimerTrig;
  }

  public String getDefaultValue()
  {
    return this._defaultValue;
  }

  public String getDescription()
  {
    return this._description;
  }

  public String getFormat()
  {
    return this._format;
  }

  public String getGraphMax()
  {
    return this._graphMax;
  }

  public String getGraphMin()
  {
    return this._graphMin;
  }

  public String getInitializeDevio()
  {
    return this._initializeDevio;
  }

  public String getMaxValue()
  {
    return this._maxValue;
  }

  public String getMinDeltaTrig()
  {
    return this._minDeltaTrig;
  }

  public String getMinStep()
  {
    return this._minStep;
  }

  public String getMinTimerTrig()
  {
    return this._minTimerTrig;
  }

  public String getMinValue()
  {
    return this._minValue;
  }

  public ACSMonitorPointT getMonitorPoint(int index)
    throws IndexOutOfBoundsException
  {
    if ((index < 0) || (index > this._monitorPointList.size())) {
      throw new IndexOutOfBoundsException();
    }

    return (ACSMonitorPointT)this._monitorPointList.elementAt(index);
  }

  public ACSMonitorPointT[] getMonitorPoint()
  {
    int size = this._monitorPointList.size();
    ACSMonitorPointT[] mArray = new ACSMonitorPointT[size];
    for (int index = 0; index < size; index++) {
      mArray[index] = ((ACSMonitorPointT)this._monitorPointList.elementAt(index));
    }
    return mArray;
  }

  public int getMonitorPointCount()
  {
    return this._monitorPointList.size();
  }

  public String getPropertyname()
  {
    return this._propertyname;
  }

  public String getResolution()
  {
    return this._resolution;
  }

  public String getStatedescription()
  {
    return this._statedescription;
  }

  public String getUnits()
  {
    return this._units;
  }

  public String getWhencleared()
  {
    return this._whencleared;
  }

  public String getWhenset()
  {
    return this._whenset;
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

  public void removeAllMonitorPoint()
  {
    this._monitorPointList.removeAllElements();
  }

  public ACSMonitorPointT removeMonitorPoint(int index)
  {
    Object obj = this._monitorPointList.elementAt(index);
    this._monitorPointList.removeElementAt(index);
    return (ACSMonitorPointT)obj;
  }

  public void setAlarmFaultFamily(String alarmFaultFamily)
  {
    this._alarmFaultFamily = alarmFaultFamily;
  }

  public void setAlarmFaultMember(String alarmFaultMember)
  {
    this._alarmFaultMember = alarmFaultMember;
  }

  public void setAlarmHighOff(String alarmHighOff)
  {
    this._alarmHighOff = alarmHighOff;
  }

  public void setAlarmHighOn(String alarmHighOn)
  {
    this._alarmHighOn = alarmHighOn;
  }

  public void setAlarmLevel(String alarmLevel)
  {
    this._alarmLevel = alarmLevel;
  }

  public void setAlarmLowOff(String alarmLowOff)
  {
    this._alarmLowOff = alarmLowOff;
  }

  public void setAlarmLowOn(String alarmLowOn)
  {
    this._alarmLowOn = alarmLowOn;
  }

  public void setAlarmOff(String alarmOff)
  {
    this._alarmOff = alarmOff;
  }

  public void setAlarmOn(String alarmOn)
  {
    this._alarmOn = alarmOn;
  }

  public void setAlarmTimerTrig(String alarmTimerTrig)
  {
    this._alarmTimerTrig = alarmTimerTrig;
  }

  public void setArchiveDelta(String archiveDelta)
  {
    this._archiveDelta = archiveDelta;
  }

  public void setArchiveMaxInt(String archiveMaxInt)
  {
    this._archiveMaxInt = archiveMaxInt;
  }

  public void setArchiveMechanism(String archiveMechanism)
  {
    this._archiveMechanism = archiveMechanism;
  }

  public void setArchiveMinInt(String archiveMinInt)
  {
    this._archiveMinInt = archiveMinInt;
  }

  public void setArchivePriority(String archivePriority)
  {
    this._archivePriority = archivePriority;
  }

  public void setArchiveSuppress(String archiveSuppress)
  {
    this._archiveSuppress = archiveSuppress;
  }

  public void setBitdescription(String bitdescription)
  {
    this._bitdescription = bitdescription;
  }

  public void setCondition(String condition)
  {
    this._condition = condition;
  }

  public void setData(String data)
  {
    this._data = data;
  }

  public void setDefaultTimerTrig(String defaultTimerTrig)
  {
    this._defaultTimerTrig = defaultTimerTrig;
  }

  public void setDefaultValue(String defaultValue)
  {
    this._defaultValue = defaultValue;
  }

  public void setDescription(String description)
  {
    this._description = description;
  }

  public void setFormat(String format)
  {
    this._format = format;
  }

  public void setGraphMax(String graphMax)
  {
    this._graphMax = graphMax;
  }

  public void setGraphMin(String graphMin)
  {
    this._graphMin = graphMin;
  }

  public void setInitializeDevio(String initializeDevio)
  {
    this._initializeDevio = initializeDevio;
  }

  public void setMaxValue(String maxValue)
  {
    this._maxValue = maxValue;
  }

  public void setMinDeltaTrig(String minDeltaTrig)
  {
    this._minDeltaTrig = minDeltaTrig;
  }

  public void setMinStep(String minStep)
  {
    this._minStep = minStep;
  }

  public void setMinTimerTrig(String minTimerTrig)
  {
    this._minTimerTrig = minTimerTrig;
  }

  public void setMinValue(String minValue)
  {
    this._minValue = minValue;
  }

  public void setMonitorPoint(int index, ACSMonitorPointT vMonitorPoint)
    throws IndexOutOfBoundsException
  {
    if ((index < 0) || (index > this._monitorPointList.size())) {
      throw new IndexOutOfBoundsException();
    }
    this._monitorPointList.setElementAt(vMonitorPoint, index);
  }

  public void setMonitorPoint(ACSMonitorPointT[] monitorPointArray)
  {
    this._monitorPointList.removeAllElements();
    for (int i = 0; i < monitorPointArray.length; i++)
      this._monitorPointList.addElement(monitorPointArray[i]);
  }

  public void setPropertyname(String propertyname)
  {
    this._propertyname = propertyname;
  }

  public void setResolution(String resolution)
  {
    this._resolution = resolution;
  }

  public void setStatedescription(String statedescription)
  {
    this._statedescription = statedescription;
  }

  public void setUnits(String units)
  {
    this._units = units;
  }

  public void setWhencleared(String whencleared)
  {
    this._whencleared = whencleared;
  }

  public void setWhenset(String whenset)
  {
    this._whenset = whenset;
  }

  public static ACSBaciPropertyT unmarshalBaciPropertyT(Reader reader)
    throws MarshalException, ValidationException
  {
    return (ACSBaciPropertyT)Unmarshaller.unmarshal(ACSBaciPropertyT.class, reader);
  }

  public void validate()
    throws ValidationException
  {
    Validator validator = new Validator();
    validator.validate(this);
  }
}
