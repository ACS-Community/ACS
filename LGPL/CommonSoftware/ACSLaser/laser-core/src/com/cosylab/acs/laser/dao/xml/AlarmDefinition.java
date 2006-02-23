/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: AlarmDefinition.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * LASER alarm definition
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class AlarmDefinition extends com.cosylab.acs.laser.dao.xml.AlarmDefinitionType
		implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * Field _faultFamily
	 */
	private java.lang.String _faultFamily;

	/**
	 * Field _faultMember
	 */
	private java.lang.String _faultMember;

	/**
	 * Field _faultCode
	 */
	private int _faultCode;

	/**
	 * keeps track of state for field: _faultCode
	 */
	private boolean _has_faultCode;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public AlarmDefinition()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.AlarmDefinition()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Method deleteFaultCode
	 * 
	 */
	public void deleteFaultCode()
	{
		this._has_faultCode = false;
	} // -- void deleteFaultCode()

	/**
	 * Returns the value of field 'faultCode'.
	 * 
	 * @return int
	 * @return the value of field 'faultCode'.
	 */
	public int getFaultCode()
	{
		return this._faultCode;
	} // -- int getFaultCode()

	/**
	 * Returns the value of field 'faultFamily'.
	 * 
	 * @return String
	 * @return the value of field 'faultFamily'.
	 */
	public java.lang.String getFaultFamily()
	{
		return this._faultFamily;
	} // -- java.lang.String getFaultFamily()

	/**
	 * Returns the value of field 'faultMember'.
	 * 
	 * @return String
	 * @return the value of field 'faultMember'.
	 */
	public java.lang.String getFaultMember()
	{
		return this._faultMember;
	} // -- java.lang.String getFaultMember()

	/**
	 * Method hasFaultCode
	 * 
	 * 
	 * 
	 * @return boolean
	 */
	public boolean hasFaultCode()
	{
		return this._has_faultCode;
	} // -- boolean hasFaultCode()

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
	 * Sets the value of field 'faultCode'.
	 * 
	 * @param faultCode
	 *            the value of field 'faultCode'.
	 */
	public void setFaultCode(int faultCode)
	{
		this._faultCode = faultCode;
		this._has_faultCode = true;
	} // -- void setFaultCode(int)

	/**
	 * Sets the value of field 'faultFamily'.
	 * 
	 * @param faultFamily
	 *            the value of field 'faultFamily'.
	 */
	public void setFaultFamily(java.lang.String faultFamily)
	{
		this._faultFamily = faultFamily;
	} // -- void setFaultFamily(java.lang.String)

	/**
	 * Sets the value of field 'faultMember'.
	 * 
	 * @param faultMember
	 *            the value of field 'faultMember'.
	 */
	public void setFaultMember(java.lang.String faultMember)
	{
		this._faultMember = faultMember;
	} // -- void setFaultMember(java.lang.String)

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
		return (com.cosylab.acs.laser.dao.xml.AlarmDefinition) Unmarshaller
				.unmarshal(com.cosylab.acs.laser.dao.xml.AlarmDefinition.class,
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
