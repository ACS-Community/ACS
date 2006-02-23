/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: Threshold.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class Threshold.
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class Threshold implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * Field _value
	 */
	private int _value;

	/**
	 * keeps track of state for field: _value
	 */
	private boolean _has_value;

	/**
	 * LASER alarm definition
	 */
	private com.cosylab.acs.laser.dao.xml.AlarmDefinition _alarmDefinition;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public Threshold()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.Threshold()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method deleteValue
	 * 
	 */
	public void deleteValue()
	{
		this._has_value = false;
	} // -- void deleteValue()

	/**
	 * Returns the value of field 'alarmDefinition'. The field 'alarmDefinition'
	 * has the following description: LASER alarm definition
	 * 
	 * @return AlarmDefinition
	 * @return the value of field 'alarmDefinition'.
	 */
	public com.cosylab.acs.laser.dao.xml.AlarmDefinition getAlarmDefinition()
	{
		return this._alarmDefinition;
	} // -- com.cosylab.acs.laser.dao.xml.AlarmDefinition getAlarmDefinition()

	/**
	 * Returns the value of field 'value'.
	 * 
	 * @return int
	 * @return the value of field 'value'.
	 */
	public int getValue()
	{
		return this._value;
	} // -- int getValue()

	/**
	 * Method hasValue
	 * 
	 * 
	 * 
	 * @return boolean
	 */
	public boolean hasValue()
	{
		return this._has_value;
	} // -- boolean hasValue()

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
	 * Sets the value of field 'alarmDefinition'. The field 'alarmDefinition'
	 * has the following description: LASER alarm definition
	 * 
	 * @param alarmDefinition
	 *            the value of field 'alarmDefinition'.
	 */
	public void setAlarmDefinition(
			com.cosylab.acs.laser.dao.xml.AlarmDefinition alarmDefinition)
	{
		this._alarmDefinition = alarmDefinition;
	} // -- void
		// setAlarmDefinition(com.cosylab.acs.laser.dao.xml.AlarmDefinition)

	/**
	 * Sets the value of field 'value'.
	 * 
	 * @param value
	 *            the value of field 'value'.
	 */
	public void setValue(int value)
	{
		this._value = value;
		this._has_value = true;
	} // -- void setValue(int)

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
		return (com.cosylab.acs.laser.dao.xml.Threshold) Unmarshaller
				.unmarshal(com.cosylab.acs.laser.dao.xml.Threshold.class,
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
