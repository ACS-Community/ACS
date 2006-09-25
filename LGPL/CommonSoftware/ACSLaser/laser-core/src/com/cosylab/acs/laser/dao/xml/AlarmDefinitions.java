/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: AlarmDefinitions.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * LASER alarm definitions
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class AlarmDefinitions implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * definitions to create
	 */
	private com.cosylab.acs.laser.dao.xml.AlarmsToCreate _alarmsToCreate;

	/**
	 * definitions to update
	 */
	private com.cosylab.acs.laser.dao.xml.AlarmsToUpdate _alarmsToUpdate;

	/**
	 * definitions to remove
	 */
	private com.cosylab.acs.laser.dao.xml.AlarmsToRemove _alarmsToRemove;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public AlarmDefinitions()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.AlarmDefinitions()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'alarmsToCreate'. The field 'alarmsToCreate'
	 * has the following description: definitions to create
	 * 
	 * @return AlarmsToCreate
	 * @return the value of field 'alarmsToCreate'.
	 */
	public com.cosylab.acs.laser.dao.xml.AlarmsToCreate getAlarmsToCreate()
	{
		return this._alarmsToCreate;
	} // -- com.cosylab.acs.laser.dao.xml.AlarmsToCreate getAlarmsToCreate()

	/**
	 * Returns the value of field 'alarmsToRemove'. The field 'alarmsToRemove'
	 * has the following description: definitions to remove
	 * 
	 * @return AlarmsToRemove
	 * @return the value of field 'alarmsToRemove'.
	 */
	public com.cosylab.acs.laser.dao.xml.AlarmsToRemove getAlarmsToRemove()
	{
		return this._alarmsToRemove;
	} // -- com.cosylab.acs.laser.dao.xml.AlarmsToRemove getAlarmsToRemove()

	/**
	 * Returns the value of field 'alarmsToUpdate'. The field 'alarmsToUpdate'
	 * has the following description: definitions to update
	 * 
	 * @return AlarmsToUpdate
	 * @return the value of field 'alarmsToUpdate'.
	 */
	public com.cosylab.acs.laser.dao.xml.AlarmsToUpdate getAlarmsToUpdate()
	{
		return this._alarmsToUpdate;
	} // -- com.cosylab.acs.laser.dao.xml.AlarmsToUpdate getAlarmsToUpdate()

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
	 * Sets the value of field 'alarmsToCreate'. The field 'alarmsToCreate' has
	 * the following description: definitions to create
	 * 
	 * @param alarmsToCreate
	 *            the value of field 'alarmsToCreate'.
	 */
	public void setAlarmsToCreate(
			com.cosylab.acs.laser.dao.xml.AlarmsToCreate alarmsToCreate)
	{
		this._alarmsToCreate = alarmsToCreate;
	} // -- void
		// setAlarmsToCreate(com.cosylab.acs.laser.dao.xml.AlarmsToCreate)

	/**
	 * Sets the value of field 'alarmsToRemove'. The field 'alarmsToRemove' has
	 * the following description: definitions to remove
	 * 
	 * @param alarmsToRemove
	 *            the value of field 'alarmsToRemove'.
	 */
	public void setAlarmsToRemove(
			com.cosylab.acs.laser.dao.xml.AlarmsToRemove alarmsToRemove)
	{
		this._alarmsToRemove = alarmsToRemove;
	} // -- void
		// setAlarmsToRemove(com.cosylab.acs.laser.dao.xml.AlarmsToRemove)

	/**
	 * Sets the value of field 'alarmsToUpdate'. The field 'alarmsToUpdate' has
	 * the following description: definitions to update
	 * 
	 * @param alarmsToUpdate
	 *            the value of field 'alarmsToUpdate'.
	 */
	public void setAlarmsToUpdate(
			com.cosylab.acs.laser.dao.xml.AlarmsToUpdate alarmsToUpdate)
	{
		this._alarmsToUpdate = alarmsToUpdate;
	} // -- void
		// setAlarmsToUpdate(com.cosylab.acs.laser.dao.xml.AlarmsToUpdate)

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
		return (com.cosylab.acs.laser.dao.xml.AlarmDefinitions) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.AlarmDefinitions.class,
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
