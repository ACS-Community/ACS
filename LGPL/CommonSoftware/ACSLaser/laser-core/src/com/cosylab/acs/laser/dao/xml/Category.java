/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: Category.java,v 1.2 2005/06/14 15:21:12 mslenc Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * the category
 * 
 * @version $Revision: 1.2 $ $Date: 2005/06/14 15:21:12 $
 */
public class Category implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * LASER alarm category definition
	 */
	private com.cosylab.acs.laser.dao.xml.CategoryDefinition _categoryDefinition;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public Category()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.Category()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'categoryDefinition'. The field
	 * 'categoryDefinition' has the following description: LASER alarm category
	 * definition
	 * 
	 * @return CategoryDefinition
	 * @return the value of field 'categoryDefinition'.
	 */
	public com.cosylab.acs.laser.dao.xml.CategoryDefinition getCategoryDefinition()
	{
		return this._categoryDefinition;
	} // -- com.cosylab.acs.laser.dao.xml.CategoryDefinition
		// getCategoryDefinition()

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
	 * Sets the value of field 'categoryDefinition'. The field
	 * 'categoryDefinition' has the following description: LASER alarm category
	 * definition
	 * 
	 * @param categoryDefinition
	 *            the value of field 'categoryDefinition'.
	 */
	public void setCategoryDefinition(
			com.cosylab.acs.laser.dao.xml.CategoryDefinition categoryDefinition)
	{
		this._categoryDefinition = categoryDefinition;
	} // -- void
		// setCategoryDefinition(com.cosylab.acs.laser.dao.xml.CategoryDefinition)

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
		return (com.cosylab.acs.laser.dao.xml.Category) Unmarshaller.unmarshal(
				com.cosylab.acs.laser.dao.xml.Category.class, reader);
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
