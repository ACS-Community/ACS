/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: AlarmDefinitionListType.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import java.util.ArrayList;

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class AlarmDefinitionListType.
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class AlarmDefinitionListType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * LASER alarm definition
	 */
	private java.util.ArrayList _alarmDefinitionList;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public AlarmDefinitionListType()
	{
		super();
		_alarmDefinitionList = new ArrayList();
	} // -- com.cosylab.acs.laser.dao.xml.AlarmDefinitionListType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method addAlarmDefinition
	 * 
	 * 
	 * 
	 * @param vAlarmDefinition
	 */
	public void addAlarmDefinition(
			com.cosylab.acs.laser.dao.xml.AlarmDefinition vAlarmDefinition)
			throws java.lang.IndexOutOfBoundsException
	{
		_alarmDefinitionList.add(vAlarmDefinition);
	} // -- void
		// addAlarmDefinition(com.cosylab.acs.laser.dao.xml.AlarmDefinition)

	/**
	 * Method addAlarmDefinition
	 * 
	 * 
	 * 
	 * @param index
	 * @param vAlarmDefinition
	 */
	public void addAlarmDefinition(int index,
			com.cosylab.acs.laser.dao.xml.AlarmDefinition vAlarmDefinition)
			throws java.lang.IndexOutOfBoundsException
	{
		_alarmDefinitionList.add(index, vAlarmDefinition);
	} // -- void addAlarmDefinition(int,
		// com.cosylab.acs.laser.dao.xml.AlarmDefinition)

	/**
	 * Method clearAlarmDefinition
	 * 
	 */
	public void clearAlarmDefinition()
	{
		_alarmDefinitionList.clear();
	} // -- void clearAlarmDefinition()

	/**
	 * Method enumerateAlarmDefinition
	 * 
	 * 
	 * 
	 * @return Enumeration
	 */
	public java.util.Enumeration enumerateAlarmDefinition()
	{
		return new org.exolab.castor.util.IteratorEnumeration(
				_alarmDefinitionList.iterator());
	} // -- java.util.Enumeration enumerateAlarmDefinition()

	/**
	 * Method getAlarmDefinition
	 * 
	 * 
	 * 
	 * @param index
	 * @return AlarmDefinition
	 */
	public com.cosylab.acs.laser.dao.xml.AlarmDefinition getAlarmDefinition(
			int index) throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _alarmDefinitionList.size())) {
			throw new IndexOutOfBoundsException();
		}

		return (com.cosylab.acs.laser.dao.xml.AlarmDefinition) _alarmDefinitionList
				.get(index);
	} // -- com.cosylab.acs.laser.dao.xml.AlarmDefinition
		// getAlarmDefinition(int)

	/**
	 * Method getAlarmDefinition
	 * 
	 * 
	 * 
	 * @return AlarmDefinition
	 */
	public com.cosylab.acs.laser.dao.xml.AlarmDefinition[] getAlarmDefinition()
	{
		int size = _alarmDefinitionList.size();
		com.cosylab.acs.laser.dao.xml.AlarmDefinition[] mArray = new com.cosylab.acs.laser.dao.xml.AlarmDefinition[size];
		for (int index = 0; index < size; index++) {
			mArray[index] = (com.cosylab.acs.laser.dao.xml.AlarmDefinition) _alarmDefinitionList
					.get(index);
		}
		return mArray;
	} // -- com.cosylab.acs.laser.dao.xml.AlarmDefinition[]
		// getAlarmDefinition()

	/**
	 * Method getAlarmDefinitionCount
	 * 
	 * 
	 * 
	 * @return int
	 */
	public int getAlarmDefinitionCount()
	{
		return _alarmDefinitionList.size();
	} // -- int getAlarmDefinitionCount()

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
		} catch (org.exolab.castor.xml.ValidationException vex) {
			return false;
		}
		return true;
	} // -- boolean isValid()

	/**
	 * Method marshal
	 * 
	 * 
	 * 
	 * @param out
	 */
	public void marshal(java.io.Writer out)
			throws org.exolab.castor.xml.MarshalException,
			org.exolab.castor.xml.ValidationException
	{

		Marshaller.marshal(this, out);
	} // -- void marshal(java.io.Writer)

	/**
	 * Method marshal
	 * 
	 * 
	 * 
	 * @param handler
	 */
	public void marshal(org.xml.sax.ContentHandler handler)
			throws java.io.IOException, org.exolab.castor.xml.MarshalException,
			org.exolab.castor.xml.ValidationException
	{

		Marshaller.marshal(this, handler);
	} // -- void marshal(org.xml.sax.ContentHandler)

	/**
	 * Method removeAlarmDefinition
	 * 
	 * 
	 * 
	 * @param vAlarmDefinition
	 * @return boolean
	 */
	public boolean removeAlarmDefinition(
			com.cosylab.acs.laser.dao.xml.AlarmDefinition vAlarmDefinition)
	{
		boolean removed = _alarmDefinitionList.remove(vAlarmDefinition);
		return removed;
	} // -- boolean
		// removeAlarmDefinition(com.cosylab.acs.laser.dao.xml.AlarmDefinition)

	/**
	 * Method setAlarmDefinition
	 * 
	 * 
	 * 
	 * @param index
	 * @param vAlarmDefinition
	 */
	public void setAlarmDefinition(int index,
			com.cosylab.acs.laser.dao.xml.AlarmDefinition vAlarmDefinition)
			throws java.lang.IndexOutOfBoundsException
	{
		// -- check bounds for index
		if ((index < 0) || (index > _alarmDefinitionList.size())) {
			throw new IndexOutOfBoundsException();
		}
		_alarmDefinitionList.set(index, vAlarmDefinition);
	} // -- void setAlarmDefinition(int,
		// com.cosylab.acs.laser.dao.xml.AlarmDefinition)

	/**
	 * Method setAlarmDefinition
	 * 
	 * 
	 * 
	 * @param alarmDefinitionArray
	 */
	public void setAlarmDefinition(
			com.cosylab.acs.laser.dao.xml.AlarmDefinition[] alarmDefinitionArray)
	{
		// -- copy array
		_alarmDefinitionList.clear();
		for (int i = 0; i < alarmDefinitionArray.length; i++) {
			_alarmDefinitionList.add(alarmDefinitionArray[i]);
		}
	} // -- void
		// setAlarmDefinition(com.cosylab.acs.laser.dao.xml.AlarmDefinition)

	/**
	 * Method unmarshal
	 * 
	 * 
	 * 
	 * @param reader
	 * @return Object
	 */
	public static java.lang.Object unmarshal(java.io.Reader reader)
			throws org.exolab.castor.xml.MarshalException,
			org.exolab.castor.xml.ValidationException
	{
		return (com.cosylab.acs.laser.dao.xml.AlarmDefinitionListType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.AlarmDefinitionListType.class,
						reader);
	} // -- java.lang.Object unmarshal(java.io.Reader)

	/**
	 * Method validate
	 * 
	 */
	public void validate() throws org.exolab.castor.xml.ValidationException
	{
		org.exolab.castor.xml.Validator validator = new org.exolab.castor.xml.Validator();
		validator.validate(this);
	} // -- void validate()

}
