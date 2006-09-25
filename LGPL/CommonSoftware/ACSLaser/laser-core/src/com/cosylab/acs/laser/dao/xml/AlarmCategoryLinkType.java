/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.7</a>, using an XML
 * Schema.
 * $Id: AlarmCategoryLinkType.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */

package com.cosylab.acs.laser.dao.xml;

// ---------------------------------/
// - Imported classes and packages -/
// ---------------------------------/

import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;

/**
 * Class AlarmCategoryLinkType.
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 */
public class AlarmCategoryLinkType implements java.io.Serializable
{

	// --------------------------/
	// - Class/Member Variables -/
	// --------------------------/

	/**
	 * the category
	 */
	private com.cosylab.acs.laser.dao.xml.Category _category;

	/**
	 * the fault state
	 */
	private com.cosylab.acs.laser.dao.xml.Alarm _alarm;

	// ----------------/
	// - Constructors -/
	// ----------------/

	public AlarmCategoryLinkType()
	{
		super();
	} // -- com.cosylab.acs.laser.dao.xml.AlarmCategoryLinkType()

	// -----------/
	// - Methods -/
	// -----------/

	/**
	 * Returns the value of field 'alarm'. The field 'alarm' has the following
	 * description: the fault state
	 * 
	 * @return Alarm
	 * @return the value of field 'alarm'.
	 */
	public com.cosylab.acs.laser.dao.xml.Alarm getAlarm()
	{
		return this._alarm;
	} // -- com.cosylab.acs.laser.dao.xml.Alarm getAlarm()

	/**
	 * Returns the value of field 'category'. The field 'category' has the
	 * following description: the category
	 * 
	 * @return Category
	 * @return the value of field 'category'.
	 */
	public com.cosylab.acs.laser.dao.xml.Category getCategory()
	{
		return this._category;
	} // -- com.cosylab.acs.laser.dao.xml.Category getCategory()

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
	 * Sets the value of field 'alarm'. The field 'alarm' has the following
	 * description: the fault state
	 * 
	 * @param alarm
	 *            the value of field 'alarm'.
	 */
	public void setAlarm(com.cosylab.acs.laser.dao.xml.Alarm alarm)
	{
		this._alarm = alarm;
	} // -- void setAlarm(com.cosylab.acs.laser.dao.xml.Alarm)

	/**
	 * Sets the value of field 'category'. The field 'category' has the
	 * following description: the category
	 * 
	 * @param category
	 *            the value of field 'category'.
	 */
	public void setCategory(com.cosylab.acs.laser.dao.xml.Category category)
	{
		this._category = category;
	} // -- void setCategory(com.cosylab.acs.laser.dao.xml.Category)

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
		return (com.cosylab.acs.laser.dao.xml.AlarmCategoryLinkType) Unmarshaller
				.unmarshal(
						com.cosylab.acs.laser.dao.xml.AlarmCategoryLinkType.class,
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
