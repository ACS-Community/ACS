/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: MaintenanceMaskType.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class MaintenanceMaskType.
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class MaintenanceMaskType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * Field _from
	 */
	private org.exolab.castor.types.Date _from;

	/**
	 * Field _to
	 */
	private org.exolab.castor.types.Date _to;

	/**
	 * LASER alarm definition
	 */
	private com.cosylab.acs.laser.dao.xml.AlarmDefinition _alarmDefinition;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public MaintenanceMaskType()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.MaintenanceMaskType()

	// -----------/
	// - Methods -/
	// -----------/

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
	 * Returns the value of field 'from'.
	 * 
	 * @return Date
	 * @return the value of field 'from'.
	 */
	public org.exolab.castor.types.Date getFrom()
	{
		return this._from;
	} // -- org.exolab.castor.types.Date getFrom()

	/**
	 * Returns the value of field 'to'.
	 * 
	 * @return Date
	 * @return the value of field 'to'.
	 */
	public org.exolab.castor.types.Date getTo()
	{
		return this._to;
	} // -- org.exolab.castor.types.Date getTo()

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
	 * Sets the value of field 'from'.
	 * 
	 * @param from
	 *            the value of field 'from'.
	 */
	public void setFrom(org.exolab.castor.types.Date from)
	{
		this._from = from;
	} // -- void setFrom(org.exolab.castor.types.Date)

	/**
	 * Sets the value of field 'to'.
	 * 
	 * @param to
	 *            the value of field 'to'.
	 */
	public void setTo(org.exolab.castor.types.Date to)
	{
		this._to = to;
	} // -- void setTo(org.exolab.castor.types.Date)

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
		return (com.cosylab.acs.laser.dao.xml.MaintenanceMaskType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.MaintenanceMaskType.class,
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
