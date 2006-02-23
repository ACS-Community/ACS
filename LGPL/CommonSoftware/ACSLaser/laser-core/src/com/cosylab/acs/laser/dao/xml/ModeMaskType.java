/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: ModeMaskType.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class ModeMaskType.
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class ModeMaskType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * the accelerator mode
	 */
	private java.lang.String _machineMode;

	/**
	 * LASER alarm definition
	 */
	private com.cosylab.acs.laser.dao.xml.AlarmDefinition _alarmDefinition;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public ModeMaskType()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.ModeMaskType()

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
	 * Returns the value of field 'machineMode'. The field 'machineMode' has the
	 * following description: the accelerator mode
	 * 
	 * @return String
	 * @return the value of field 'machineMode'.
	 */
	public java.lang.String getMachineMode()
	{
		return this._machineMode;
	} // -- java.lang.String getMachineMode()

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
	 * Sets the value of field 'machineMode'. The field 'machineMode' has the
	 * following description: the accelerator mode
	 * 
	 * @param machineMode
	 *            the value of field 'machineMode'.
	 */
	public void setMachineMode(java.lang.String machineMode)
	{
		this._machineMode = machineMode;
	} // -- void setMachineMode(java.lang.String)

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
		return (com.cosylab.acs.laser.dao.xml.ModeMaskType) Unmarshaller
				.unmarshal(com.cosylab.acs.laser.dao.xml.ModeMaskType.class,
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
