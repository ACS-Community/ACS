/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: CategoryDefinitions.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * LASER category definitions
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class CategoryDefinitions implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * definitions to create
	 */
	private com.cosylab.acs.laser.dao.xml.CategoriesToCreate _categoriesToCreate;

	/**
	 * definitions to update
	 */
	private com.cosylab.acs.laser.dao.xml.CategoriesToUpdate _categoriesToUpdate;

	/**
	 * definitions to remove
	 */
	private com.cosylab.acs.laser.dao.xml.CategoriesToRemove _categoriesToRemove;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public CategoryDefinitions()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.CategoryDefinitions()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'categoriesToCreate'. The field
	 * 'categoriesToCreate' has the following description: definitions to create
	 * 
	 * @return CategoriesToCreate
	 * @return the value of field 'categoriesToCreate'.
	 */
	public com.cosylab.acs.laser.dao.xml.CategoriesToCreate getCategoriesToCreate()
	{
		return this._categoriesToCreate;
	} // -- com.cosylab.acs.laser.dao.xml.CategoriesToCreate
		// getCategoriesToCreate()

	/**
	 * Returns the value of field 'categoriesToRemove'. The field
	 * 'categoriesToRemove' has the following description: definitions to remove
	 * 
	 * @return CategoriesToRemove
	 * @return the value of field 'categoriesToRemove'.
	 */
	public com.cosylab.acs.laser.dao.xml.CategoriesToRemove getCategoriesToRemove()
	{
		return this._categoriesToRemove;
	} // -- com.cosylab.acs.laser.dao.xml.CategoriesToRemove
		// getCategoriesToRemove()

	/**
	 * Returns the value of field 'categoriesToUpdate'. The field
	 * 'categoriesToUpdate' has the following description: definitions to update
	 * 
	 * @return CategoriesToUpdate
	 * @return the value of field 'categoriesToUpdate'.
	 */
	public com.cosylab.acs.laser.dao.xml.CategoriesToUpdate getCategoriesToUpdate()
	{
		return this._categoriesToUpdate;
	} // -- com.cosylab.acs.laser.dao.xml.CategoriesToUpdate
		// getCategoriesToUpdate()

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
	 * Sets the value of field 'categoriesToCreate'. The field
	 * 'categoriesToCreate' has the following description: definitions to create
	 * 
	 * @param categoriesToCreate
	 *            the value of field 'categoriesToCreate'.
	 */
	public void setCategoriesToCreate(
			com.cosylab.acs.laser.dao.xml.CategoriesToCreate categoriesToCreate)
	{
		this._categoriesToCreate = categoriesToCreate;
	} // -- void
		// setCategoriesToCreate(com.cosylab.acs.laser.dao.xml.CategoriesToCreate)

	/**
	 * Sets the value of field 'categoriesToRemove'. The field
	 * 'categoriesToRemove' has the following description: definitions to remove
	 * 
	 * @param categoriesToRemove
	 *            the value of field 'categoriesToRemove'.
	 */
	public void setCategoriesToRemove(
			com.cosylab.acs.laser.dao.xml.CategoriesToRemove categoriesToRemove)
	{
		this._categoriesToRemove = categoriesToRemove;
	} // -- void
		// setCategoriesToRemove(com.cosylab.acs.laser.dao.xml.CategoriesToRemove)

	/**
	 * Sets the value of field 'categoriesToUpdate'. The field
	 * 'categoriesToUpdate' has the following description: definitions to update
	 * 
	 * @param categoriesToUpdate
	 *            the value of field 'categoriesToUpdate'.
	 */
	public void setCategoriesToUpdate(
			com.cosylab.acs.laser.dao.xml.CategoriesToUpdate categoriesToUpdate)
	{
		this._categoriesToUpdate = categoriesToUpdate;
	} // -- void
		// setCategoriesToUpdate(com.cosylab.acs.laser.dao.xml.CategoriesToUpdate)

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
		return (com.cosylab.acs.laser.dao.xml.CategoryDefinitions) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.CategoryDefinitions.class,
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
