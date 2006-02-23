/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: SourceDefinitions.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * LASER source definitions
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class SourceDefinitions implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * definitions to create
	 */
	private com.cosylab.acs.laser.dao.xml.SourcesToCreate _sourcesToCreate;

	/**
	 * definitions to update
	 */
	private com.cosylab.acs.laser.dao.xml.SourcesToUpdate _sourcesToUpdate;

	/**
	 * definitions to remove
	 */
	private com.cosylab.acs.laser.dao.xml.SourcesToRemove _sourcesToRemove;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public SourceDefinitions()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.SourceDefinitions()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'sourcesToCreate'. The field 'sourcesToCreate'
	 * has the following description: definitions to create
	 * 
	 * @return SourcesToCreate
	 * @return the value of field 'sourcesToCreate'.
	 */
	public com.cosylab.acs.laser.dao.xml.SourcesToCreate getSourcesToCreate()
	{
		return this._sourcesToCreate;
	} // -- com.cosylab.acs.laser.dao.xml.SourcesToCreate getSourcesToCreate()

	/**
	 * Returns the value of field 'sourcesToRemove'. The field 'sourcesToRemove'
	 * has the following description: definitions to remove
	 * 
	 * @return SourcesToRemove
	 * @return the value of field 'sourcesToRemove'.
	 */
	public com.cosylab.acs.laser.dao.xml.SourcesToRemove getSourcesToRemove()
	{
		return this._sourcesToRemove;
	} // -- com.cosylab.acs.laser.dao.xml.SourcesToRemove getSourcesToRemove()

	/**
	 * Returns the value of field 'sourcesToUpdate'. The field 'sourcesToUpdate'
	 * has the following description: definitions to update
	 * 
	 * @return SourcesToUpdate
	 * @return the value of field 'sourcesToUpdate'.
	 */
	public com.cosylab.acs.laser.dao.xml.SourcesToUpdate getSourcesToUpdate()
	{
		return this._sourcesToUpdate;
	} // -- com.cosylab.acs.laser.dao.xml.SourcesToUpdate getSourcesToUpdate()

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
	 * Sets the value of field 'sourcesToCreate'. The field 'sourcesToCreate'
	 * has the following description: definitions to create
	 * 
	 * @param sourcesToCreate
	 *            the value of field 'sourcesToCreate'.
	 */
	public void setSourcesToCreate(
			com.cosylab.acs.laser.dao.xml.SourcesToCreate sourcesToCreate)
	{
		this._sourcesToCreate = sourcesToCreate;
	} // -- void
		// setSourcesToCreate(com.cosylab.acs.laser.dao.xml.SourcesToCreate)

	/**
	 * Sets the value of field 'sourcesToRemove'. The field 'sourcesToRemove'
	 * has the following description: definitions to remove
	 * 
	 * @param sourcesToRemove
	 *            the value of field 'sourcesToRemove'.
	 */
	public void setSourcesToRemove(
			com.cosylab.acs.laser.dao.xml.SourcesToRemove sourcesToRemove)
	{
		this._sourcesToRemove = sourcesToRemove;
	} // -- void
		// setSourcesToRemove(com.cosylab.acs.laser.dao.xml.SourcesToRemove)

	/**
	 * Sets the value of field 'sourcesToUpdate'. The field 'sourcesToUpdate'
	 * has the following description: definitions to update
	 * 
	 * @param sourcesToUpdate
	 *            the value of field 'sourcesToUpdate'.
	 */
	public void setSourcesToUpdate(
			com.cosylab.acs.laser.dao.xml.SourcesToUpdate sourcesToUpdate)
	{
		this._sourcesToUpdate = sourcesToUpdate;
	} // -- void
		// setSourcesToUpdate(com.cosylab.acs.laser.dao.xml.SourcesToUpdate)

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
		return (com.cosylab.acs.laser.dao.xml.SourceDefinitions) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.SourceDefinitions.class,
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
