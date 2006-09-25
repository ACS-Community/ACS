/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: CategoryDefinitionType.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class CategoryDefinitionType.
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class CategoryDefinitionType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * category description
	 */
	private java.lang.String _description;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public CategoryDefinitionType()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.CategoryDefinitionType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'description'. The field 'description' has the
	 * following description: category description
	 * 
	 * @return String
	 * @return the value of field 'description'.
	 */
	public java.lang.String getDescription()
	{
		return this._description;
	} // -- java.lang.String getDescription()

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
	 * Sets the value of field 'description'. The field 'description' has the
	 * following description: category description
	 * 
	 * @param description
	 *            the value of field 'description'.
	 */
	public void setDescription(java.lang.String description)
	{
		this._description = description;
	} // -- void setDescription(java.lang.String)

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
		return (com.cosylab.acs.laser.dao.xml.CategoryDefinitionType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.CategoryDefinitionType.class,
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
