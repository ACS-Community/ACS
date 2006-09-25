/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: AlarmCategoryDefinitions.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * LASER alarm-category link definitions
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class AlarmCategoryDefinitions implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * definitions to create
	 */
	private com.cosylab.acs.laser.dao.xml.CategoryLinksToCreate _categoryLinksToCreate;

	/**
	 * definitions to remove
	 */
	private com.cosylab.acs.laser.dao.xml.CategoryLinksToRemove _categoryLinksToRemove;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public AlarmCategoryDefinitions()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.AlarmCategoryDefinitions()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'categoryLinksToCreate'. The field
	 * 'categoryLinksToCreate' has the following description: definitions to
	 * create
	 * 
	 * @return CategoryLinksToCreate
	 * @return the value of field 'categoryLinksToCreate'.
	 */
	public com.cosylab.acs.laser.dao.xml.CategoryLinksToCreate getCategoryLinksToCreate()
	{
		return this._categoryLinksToCreate;
	} // -- com.cosylab.acs.laser.dao.xml.CategoryLinksToCreate
		// getCategoryLinksToCreate()

	/**
	 * Returns the value of field 'categoryLinksToRemove'. The field
	 * 'categoryLinksToRemove' has the following description: definitions to
	 * remove
	 * 
	 * @return CategoryLinksToRemove
	 * @return the value of field 'categoryLinksToRemove'.
	 */
	public com.cosylab.acs.laser.dao.xml.CategoryLinksToRemove getCategoryLinksToRemove()
	{
		return this._categoryLinksToRemove;
	} // -- com.cosylab.acs.laser.dao.xml.CategoryLinksToRemove
		// getCategoryLinksToRemove()

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
	 * Sets the value of field 'categoryLinksToCreate'. The field
	 * 'categoryLinksToCreate' has the following description: definitions to
	 * create
	 * 
	 * @param categoryLinksToCreate
	 *            the value of field 'categoryLinksToCreate'.
	 */
	public void setCategoryLinksToCreate(
			com.cosylab.acs.laser.dao.xml.CategoryLinksToCreate categoryLinksToCreate)
	{
		this._categoryLinksToCreate = categoryLinksToCreate;
	} // -- void
		// setCategoryLinksToCreate(com.cosylab.acs.laser.dao.xml.CategoryLinksToCreate)

	/**
	 * Sets the value of field 'categoryLinksToRemove'. The field
	 * 'categoryLinksToRemove' has the following description: definitions to
	 * remove
	 * 
	 * @param categoryLinksToRemove
	 *            the value of field 'categoryLinksToRemove'.
	 */
	public void setCategoryLinksToRemove(
			com.cosylab.acs.laser.dao.xml.CategoryLinksToRemove categoryLinksToRemove)
	{
		this._categoryLinksToRemove = categoryLinksToRemove;
	} // -- void
		// setCategoryLinksToRemove(com.cosylab.acs.laser.dao.xml.CategoryLinksToRemove)

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
		return (com.cosylab.acs.laser.dao.xml.AlarmCategoryDefinitions) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.AlarmCategoryDefinitions.class,
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
