/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: MaskDefinitions.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * LASER mask definitions
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class MaskDefinitions implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * Field _masksToCreate
	 */
	private com.cosylab.acs.laser.dao.xml.MasksToCreate _masksToCreate;

	/**
	 * Field _masksToRemove
	 */
	private com.cosylab.acs.laser.dao.xml.MasksToRemove _masksToRemove;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public MaskDefinitions()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.MaskDefinitions()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'masksToCreate'.
	 * 
	 * @return MasksToCreate
	 * @return the value of field 'masksToCreate'.
	 */
	public com.cosylab.acs.laser.dao.xml.MasksToCreate getMasksToCreate()
	{
		return this._masksToCreate;
	} // -- com.cosylab.acs.laser.dao.xml.MasksToCreate getMasksToCreate()

	/**
	 * Returns the value of field 'masksToRemove'.
	 * 
	 * @return MasksToRemove
	 * @return the value of field 'masksToRemove'.
	 */
	public com.cosylab.acs.laser.dao.xml.MasksToRemove getMasksToRemove()
	{
		return this._masksToRemove;
	} // -- com.cosylab.acs.laser.dao.xml.MasksToRemove getMasksToRemove()

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
	 * Sets the value of field 'masksToCreate'.
	 * 
	 * @param masksToCreate
	 *            the value of field 'masksToCreate'.
	 */
	public void setMasksToCreate(
			com.cosylab.acs.laser.dao.xml.MasksToCreate masksToCreate)
	{
		this._masksToCreate = masksToCreate;
	} // -- void setMasksToCreate(com.cosylab.acs.laser.dao.xml.MasksToCreate)

	/**
	 * Sets the value of field 'masksToRemove'.
	 * 
	 * @param masksToRemove
	 *            the value of field 'masksToRemove'.
	 */
	public void setMasksToRemove(
			com.cosylab.acs.laser.dao.xml.MasksToRemove masksToRemove)
	{
		this._masksToRemove = masksToRemove;
	} // -- void setMasksToRemove(com.cosylab.acs.laser.dao.xml.MasksToRemove)

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
		return (com.cosylab.acs.laser.dao.xml.MaskDefinitions) Unmarshaller
				.unmarshal(com.cosylab.acs.laser.dao.xml.MaskDefinitions.class,
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
